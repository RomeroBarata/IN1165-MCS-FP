BSBDG <- function(data){
  x_maj <- subset(data, Class == -1, select = -Class)
  x_min <- subset(data, Class == 1, select = -Class)
  
  clusters_maj <- clusters(x_maj)
  clusters_min <- clusters(x_min)
  
  ## Ignoring the PCA step for a while
  # f <- function(x, clusters, i){
  #   x <- x[clusters == i, , drop = FALSE]
  #   pcaCoordinates(x)
  # }
  # x_maj <- lapply(seq_along(unique(clusters_maj)),
  #                 f, x = x_maj, clusters = clusters_maj)
  # x_maj <- as.data.frame(rbindList(x_maj))
  # x_min <- lapply(seq_along(unique(clusters_min)),
  #                 f, x = x_min, clusters = clusters_min)
  # x_min <- as.data.frame(rbindList(x_min))
  # 
  # colnames(x_maj) <- colnames(data[, -ncol(data)])
  # colnames(x_min) <- colnames(data[, -ncol(data)])
  # 
  # clusters_maj <- sort(clusters_maj)
  # clusters_min <- sort(clusters_min)
  
  boundaries <- identifyBoundary(cbind(x_maj, data.frame(Class = -1)), 
                                 cbind(x_min, data.frame(Class = 1)), 
                                 clusters_maj, clusters_min)
  synth <- synthesiseBoundary(x_maj, x_min, 
                              boundaries[["bnd_maj"]], 
                              boundaries[["bnd_min"]])
  
  synth[["Majority"]] <- cbind(synth[["Majority"]], data.frame(Class = -1))
  synth[["Minority"]] <- cbind(synth[["Minority"]], data.frame(Class = 1))
  synth <- do.call(rbind, c(synth, list(make.row.names = FALSE)))
  synth[["Class"]] <- factor(synth[["Class"]], levels = c(1, -1))
  synth
}

# pcaCoordinates <- function(x){
#   if (anyNA(x) || anyInf(x)){
#     median_values <- vapply(x, median, numeric(1), na.rm = TRUE)
#     x[] <- lapply(seq_along(x),
#                   function(i) {x[[i]][is.na(x[[i]])] <- 
#                     median_values[i]; x[[i]]})
#   }
#   nc <- ncol(x)
#   x <- prcomp(x)[["x"]]
#   if (ncol(x) < nc){
#     for (i in 1:(nc - ncol(x))){
#       x <- cbind(x, 0)
#     }
#   }
#   x
# }

identifyBoundary <- function(data_maj, data_min, 
                             clusters_maj, clusters_min){
  colnames(data_maj)[ncol(data_maj)] <- "Class"
  colnames(data_min)[ncol(data_min)] <- "Class"
  
  f <- function(data_train, data_test){
    if (nrow(data_train) <= 5) return(1:nrow(data_train))
    if (anyNA(data_train)){
      median_values <- vapply(data_train[, -ncol(data_train)], 
                              median, numeric(1), na.rm = TRUE)
      data_train[, -ncol(data_train)] <- 
        substituteNA(data_train[, -ncol(data_train)], median_values)
    }
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
  original_bnd_maj <- bnd_maj
  original_bnd_min <- bnd_min
  
  idx_maj <- vapply(bnd_maj, function(x) length(x) > 5, logical(1))
  bnd_maj <- bnd_maj[idx_maj]
  idx_min <- vapply(bnd_min, function(x) length(x) > 5, logical(1))
  bnd_min <- bnd_min[idx_min]
  
  if ((length(bnd_maj) == 0) || (length(bnd_min) == 0)){
    warning("Clusters are too small.")
    return(list(Majority = x_maj, Minority = x_min))
  }
  
  f <- function(x, idx){
    res <- vapply(x[idx, ], sd, numeric(1), na.rm = TRUE)
    res[is.na(res)] <- 0
    res
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
  
  offset_maj <- as.matrix(boot_sds_maj - sds_maj)
  offset_min <- as.matrix(boot_sds_min - sds_min)
  
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
  
  x_maj_synth <- do.call(rbind, x_maj_synth)
  x_min_synth <- do.call(rbind, x_min_synth)
  
  if (is.null(x_maj_synth)){
    x_maj_synth <- lapply(original_bnd_maj, 
                          function(idx) x_maj[idx, , drop = FALSE])
    x_maj_synth <- do.call(rbind, x_maj_synth)
  }
  
  if (is.null(x_min_synth)){
    x_min_synth <- lapply(original_bnd_min, 
                          function(idx) x_min[idx, , drop = FALSE])
    x_min_synth <- do.call(rbind, x_min_synth)
  }
  
  list(Majority = x_maj_synth, 
       Minority = x_min_synth)
}