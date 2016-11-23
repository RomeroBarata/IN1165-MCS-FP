createStratifiedPartition <- function(y, folds = 10){
  if (is.data.frame(y)) y <- unlist(y, use.names = FALSE)
  classes_dist <- table(y)
  
  partition <- vector(mode = "numeric", length = length(y))
  for(i in seq_along(classes_dist)){
    if (length(folds) == 1){
      max_sample <- ceiling(classes_dist[i] / folds) * folds
      folds_idx <- rep_len(1:folds, length.out = max_sample)
    } else{
      offset <- classes_dist[i] %% 10
      max_sample <- classes_dist[i] - offset + 10
      folds_idx <- rep_len(rep.int(seq_along(folds), times = folds),
                           length.out = max_sample)
    }
    class_partition <- sample(folds_idx)[1:classes_dist[i]]
    class_id <- names(classes_dist)[i]
    partition[y == class_id] <- class_partition
  }
  partition
}

cvTrain <- function(data, 
                    method, method_args = list(), 
                    sampling = NULL, sampling_args = list(), 
                    folds, repeats, cores = 1, seed = NULL, ...){
  if (!is.null(seed)) set.seed(seed)
  partitions <- replicate(repeats, 
                          createStratifiedPartition(data[["Class"]], folds), 
                          simplify = FALSE)
  
  results <- parallel::mcMap(train, 
                             data = list(data), 
                             method = list(method), 
                             method_args = list(method_args), 
                             sampling = list(sampling), 
                             sampling_args = list(sampling_args), 
                             partition = partitions, 
                             mc.cores = cores)
}

train <- function(data, 
                  method, method_args = list(), 
                  sampling = NULL, sampling_args = list(), 
                  partition, ...){
  folds <- length(unique(partition))
  col_names <- c("examples", "accuracy", "recall", 
                 "precision", "f1", "specificity", 
                 "npv", "f1_neg", "geo_mean")
  results <- matrix(0, nrow = folds, ncol = length(col_names), 
                    dimnames = list(NULL, col_names))
  for (i in seq_len(folds)){
    # Split testing set
    testing_idx <- i == partition
    testing <- data[testing_idx, -ncol(data), drop = FALSE]
    y <- data[testing_idx, ncol(data)]
    
    # Split training set
    training_idx <- i != partition
    training <- data[training_idx, , drop = FALSE]
    
    # (Optional) Sample the training data
    if (!is.null(sampling))
      training <- do.call(sampling, c(list(data = training), sampling_args))
    
    # Train a model
    model <- do.call(method, c(list(data = training), method_args))
    
    # Make predictions
    preds <- predict(model, testing, type = "class")
    
    # Compute performance metrics
    perf <- computeMetrics(preds, y)
    results[i, ] <- c(examples = nrow(training), perf)
  }
  results
}