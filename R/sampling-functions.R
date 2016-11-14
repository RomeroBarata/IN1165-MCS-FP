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