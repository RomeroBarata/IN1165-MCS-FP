adaboostM1 <- function(data, L = 10){
  y <- data[["Class"]]
  w <- rep(1 / nrow(data), nrow(data))
  
  betas <- vector(mode = "numeric", length = L)
  classifiers <- vector(mode = "list", length = L)
  k <- 1
  while (k <= L){
    in_train <- sample(nrow(data), replace = TRUE, prob = w)
    training <- data[in_train, , drop = FALSE]
    
    model <- neuralNet(training)
    preds <- predict(model, subset(data, select = -Class), type = "class")
    
    l <- preds != y
    error <- sum(w * l)
    
    if ((error == 0) || (error >= 0.5)){
      w <- rep(1 / nrow(data), nrow(data))
      next
    }
    
    betas[k] <- error / (1 - error)
    w <- (w * (betas[k]) ^ (1 - l)) / sum(w * (betas[k]) ^ (1 - l))
    classifiers[[k]] <- model
    
    k <- k + 1
  }
  
  structure(list(classifiers = classifiers, 
                 betas = betas), 
            class = "adaboostM1")
}

predict.adaboostM1 <- function(object, newdata, ...){
  classifiers <- object[["classifiers"]]
  betas <- object[["betas"]]
  preds <- vapply(classifiers, predict, character(nrow(newdata)), 
                  newdata = newdata, type = "class")
  f <- function(x){
    pos_idx <- which(x == 1)
    neg_idx <- which(x == -1)
    pos_sup <- sum(log(1 / betas[pos_idx]))
    neg_sup <- sum(log(1 / betas[neg_idx]))
    if (pos_sup > neg_sup) 1 else -1
  }
  factor(apply(preds, 1, f), levels = c(1, -1))
}