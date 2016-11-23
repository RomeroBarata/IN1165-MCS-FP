computeMetrics <- function(preds, ground_truth){
  lvls <- levels(ground_truth)
  names(lvls) <- c("positive", "negative")
  
  TP <- sum((preds == lvls["positive"]) & (ground_truth == lvls["positive"]))
  FP <- sum((preds == lvls["positive"]) & (ground_truth == lvls["negative"]))
  FN <- sum((preds == lvls["negative"]) & (ground_truth == lvls["positive"]))
  TN <- sum((preds == lvls["negative"]) & (ground_truth == lvls["negative"]))
  
  accuracy <- (TP + TN) / (TP + FP + FN + TN)
  
  recall <- if (!is.nan(TP / (TP + FN))) TP / (TP + FN) else 0
  precision <- if (!is.nan(TP / (TP + FP))) TP / (TP + FP) else 1
  f1 <- (2 * recall * precision) / (recall + precision)
  f1 <- if (!is.nan(f1)) f1 else 0
  
  specificity <- if (!is.nan(TN / (TN + FP))) TN / (TN + FP) else 0
  npv <- if (!is.nan(TN / (TN + FN))) TN / (TN + FN) else 1
  f1_neg <- (2 * specificity * npv) / (specificity + npv)
  f1_neg <- if (!is.nan(f1_neg)) f1_neg else 0
  
  geo_mean <- sqrt(recall * specificity)
  
  c(accuracy = accuracy, 
    recall = recall, 
    precision = precision, 
    f1 = f1, 
    specificity = specificity, 
    npv = npv, 
    f1_neg = f1_neg, 
    geo_mean = geo_mean)
}