pcaCoordinates <- function(x){
  nc <- ncol(x)
  x <- prcomp(x)[["x"]]
  if (ncol(x) < nc){
    for (i in 1:(nc - ncol(x))){
      x <- cbind(x, 0)
    }
  }
  x
}

identifyBoundary <- function(data_maj, data_min, 
                             clusters_maj, clusters_min){
  colnames(data_maj)[ncol(data_maj)] <- "Class"
  colnames(data_min)[ncol(data_min)] <- "Class"
  
  f <- function(data_train, data_test){
    model <- kknn(Class ~ ., 
                  train = data_train, 
                  test = data_test, 
                  k = 1, 
                  kernel = "rectangular")
    unique(model[["C"]])
  }
  
  bnd_maj <- lapply(1:length(unique(clusters_maj)), 
                    function(i) 
                      f(data_maj[i == clusters_maj, , drop = FALSE], data_min))
  bnd_min <- lapply(1:length(unique(clusters_min)), 
                    function(i) 
                      f(data_min[i == clusters_min, , drop = FALSE], data_maj))
  
  list(bnd_maj = bnd_maj, 
       bnd_min = bnd_min)
}

synthesiseBoundary <- function(x_maj, x_min, 
                               bnd_maj, bnd_min, 
                               bootstrap_reps = 10){
  idx_maj <- vapply(bnd_maj, function(x) length(x) > 5, logical(1))
  bnd_maj <- bnd_maj[idx_maj]
  idx_min <- vapply(bnd_min, function(x) length(x) > 5, logical(1))
  bnd_min <- bnd_min[idx_min]
  
  f <- function(x, idx){
    vapply(x[idx, ], sd, numeric(1))
  }
  sds_maj <- vapply(bnd_maj, f, numeric(ncol(x_maj)), x = x_maj)
  sds_min <- vapply(bnd_min, f, numeric(ncol(x_min)), x = x_min)
  
  g <- function(x, idx){
    x <- x[idx, ]
    bootstrap_samples <- replicate(bootstrap_reps, 
                                   sample(nrow(x), replace = TRUE), 
                                   simplify = FALSE)
    rowMeans(vapply(bootstrap_samples, f, numeric(ncol(x)), x = x))
  }
  boot_sds_maj <- vapply(bnd_maj, g, numeric(ncol(x_maj)), x = x_maj)
  boot_sds_min <- vapply(bnd_min, g, numeric(ncol(x_min)), x = x_min)
  
  which_clusters_maj <- colSums(boot_sds_maj > sds_maj) > 0
  which_clusters_min <- colSums(boot_sds_min > sds_min) > 0
  
  offset_maj <- boot_sds_maj - sds_maj
  offset_min <- boot_sds_min - sds_min
  
  h <- function(x, bnd, i, offset){
    x <- x[bnd[[i]], ]
    sweep(x, 2, offset[, i], `+`)
  }
  bnd_maj <- bnd_maj[which_clusters_maj]
  offset_maj <- offset_maj[, which_clusters_maj, drop = FALSE]
  x_maj_synth <- lapply(seq_along(bnd_maj), h, 
                        x = x_maj, bnd = bnd_maj, offset = offset_maj)
  
  bnd_min <- bnd_min[which_clusters_min]
  offset_min <- offset_min[, which_clusters_min, drop = FALSE]
  x_min_synth <- lapply(seq_along(bnd_min), h, 
                        x = x_min, bnd = bnd_min, offset = offset_min)
  
  x_maj_synth <- rbindList(x_maj_synth)
  x_min_synth <- rbindList(x_min_synth)
  
  list(Majority = x_maj_synth, 
       Minority = x_min_synth)
}