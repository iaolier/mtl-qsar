### rforest NRMSE ###

output <- data.frame()

all_data <- list.files(path="/shared/mtl-qsar/predictions/stl/rforest")


list_data = list.files("/shared/mtl-qsar/predictions/stl/rforest", 
                       full.names = TRUE)

sz = length(list_data)


for(i in 1:sz){
  data_name = list_data[i]
  preds_dset = read.csv(data_name) # reads in each file of data
  
  print(i)
  
  data_id = all_data[i]
  algorithm = 'RF'
  
  for(foldi in 1:10){
    
    tmp <- filter(preds_dset, foldi == fold)
    
    nrmse <- NRMSE(tmp$truth,tmp$prediction)
    fold = foldi
    outp = cbind(data_id, fold, algorithm, nrmse)
    output = rbind(output, outp)
    
  }
  
  out.p = as.data.frame(output)
  write.csv(out.p, file="rforest_NRMSE.csv")
}
