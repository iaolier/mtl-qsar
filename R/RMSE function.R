### RMSE function

rmse <- function(truth, preds){
  
  rmse <- sqrt(sum((truth-preds)^2)/NROW(truth)) 
  return(rmse)
}





