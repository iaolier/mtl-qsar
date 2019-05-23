### R2 function ###

 R2 <- function(truth, preds) {
   
   RSS <- sum((truth-preds)^2)
   TSS <- sum((truth-mean(truth))^2)
   
   R2 <- 1 - RSS/TSS
   return(R2)
 
 }