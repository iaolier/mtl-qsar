### performance metric functions ###

rmse = function(truth, preds)
  sqrt(sum((truth-preds)^2)/length(truth))

nrmse = function(truth = NULL, preds = NULL){
  rmse(truth, preds) / (max(truth) - min(truth))
}

r2 = function(truth, preds) {
  RSS <- sum((truth-preds)^2)
  TSS <- sum((truth-mean(truth))^2)
  
  (1 - RSS/TSS)
}