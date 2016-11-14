neuralNet <- function(data){
  x <- subset(data, select = -Class)
  y <- subset(data, select = Class)
  
  x <- scale(x)
  train_center <- attr(x, "scaled:center")
  train_scale  <- attr(x, "scaled:scale")
  x <- as.data.frame(x)
  
  train_median_values <- vapply(x, median, numeric(1), na.rm = TRUE)
  if (anyNA(x) || anyInf(x)) x[] <- substituteNA(x, train_median_values)
  
  num_hidden <- (ncol(x) + length(unique(y))) / 2
  data <- cbind(x, y)
  model <- nnet(Class ~ ., 
                data = data, 
                size = num_hidden, 
                maxit = 500)
  
  structure(list(model = model, 
                 train_center = train_center, 
                 train_scale = train_scale, 
                 train_median_values = train_median_values), 
            class = "neuralNet")
}

predict.neuralNet <- function(object, newdata, ...){
  newdata <- scale(newdata, 
                   center = object[["train_center"]], 
                   scale = object[["train_scale"]])
  newdata <- as.data.frame(newdata)
  if (anyNA(newdata) || anyInf(newdata)) 
    newdata[] <- substituteNA(newdata, object[["train_median_values"]])
  predict(object[["model"]], newdata, ...)
}

substituteNA <- function(x, values){
  f <- function(i){
    feat <- x[[i]]
    feat[is.na(feat)] <- values[i]
    feat
  }
  lapply(seq_along(x), f)
}