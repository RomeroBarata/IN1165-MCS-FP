pcaCoordinates <- function(x){
  prcomp(x)[["x"]]
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
  
  list(Boundary_Majority = bnd_maj, 
       Boundary_Minority = bnd_min)
}