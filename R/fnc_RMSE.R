### RMSE function ###

RMSE <- function(truth, preds){
  
  RMSE <- sqrt(sum((truth-preds)^2)/NROW(truth))
  return(RMSE)
  
}