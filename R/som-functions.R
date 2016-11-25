clusters <- function(x){
  if (!is.matrix(x)) x <- as.matrix(x)
  
  cols <- caret::nzv(x)
  if (length(cols) > 0)
    x <- x[, -cols, drop = FALSE]
  if (anyNA(x)){
    median_values <- apply(x, 2, median, na.rm = TRUE)
    for (j in 1:ncol(x)){
      x[is.na(x[, j]), j] <- median_values[j]
    }
  }
  x <- scale(x)  # Center and scale the attributes
  
  num_examples <- nrow(x)
  if (num_examples <= 100) 
    xdim <- ydim <- 4
  else if (num_examples <= 1000)
    xdim <- ydim <- 8
  else if (num_examples <= 5000) 
    xdim <- ydim <- 16
  else 
    xdim <- ydim <- 20
  
  som_grid <- somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal")
  som_model <- som(x, 
                   grid = som_grid, 
                   rlen = 100, 
                   alpha = c(0.05, 0.01), 
                   n.hood = "circular")
  
  wss <- vapply(1:15, 
                function(i) sum(kmeans(som_model[["codes"]], 
                                       centers = i)[["withinss"]]), 
                numeric(1))
  num_clusters <- which.min(abs(wss - (0.5 * wss[1] + 0.5 * wss[15])))
  kmeans_model <- kmeans(x, centers = num_clusters, iter.max = 100, nstart = 10)
  unname(kmeans_model[["cluster"]])
}