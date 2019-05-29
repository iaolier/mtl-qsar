### normalised RMSE function ###

NRMSE <- function(truth, preds){
  
  RMSE <- sqrt(sum((truth-preds)^2)/NROW(truth))
  NRMSE <- RMSE/(max(preds)-min(preds))
  return(NRMSE)
  
}
