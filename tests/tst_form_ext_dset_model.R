install.packages('dplyr')
install.packages('readr')
install.packages('ranger')
install.packages('foreach')

#### Library functions ====
library(dplyr)
library(readr)
library(ranger)
library(foreach)

#### FUnctions ====
'%ni%' <- Negate('%in%')

#### Load in the data ====
data_splits = list.files('/shared/mtl-qsar/data_splits/', full.names = T) # List of the datasets
# ID shows the row number in a given dataset

assist_data = list.files('/shared/mtl-qsar/datasets/assist_data_v190709', full.names = T) # List with fold information
# row numbers for 10 fold cv

original = list.files('/shared/mtl-qsar/datasets/originals', full.names = T) # lists the original datasets

## Only included to test the 10 completed assistant task sets
data_splits = data_splits[1:10]
original = original[1:10]

#### Prep the analysis ====
## Length of data in use
len = length(data_splits)

#### Sorting the data ====
tst.list = list()
trn.list = list()
a.trn = list()
ex.data = list()

tst.out = list()
stl.out = list()
mtl.out = list()
ext.data = list()

for (dnum in 1:len){
  
  split = read_csv(data_splits[dnum]) # pulls out the split information for the primary task 
  assist = read_csv(assist_data[dnum]) # pull out fold info for given dataset
  orig = read_csv(original[dnum])
  
  names(split)[1] = 'molecule_id'
  
  for (iter in 1:10){
    
   # tmp1 = assist[[iter]] # pulls out the fold IDs
    
    tst = split[split$fold == iter,] # pulls out data matching the CV folds
    tst = merge(orig, tst, by = 'molecule_id')
    tst = select(tst, -c(fold))
    
    trn = split[split$fold != iter,] # pulls out not matching CV folds
    trn = merge(orig, trn, by = 'molecule_id')
    trn = select(trn, -c(fold))
    
    tst.list[[iter]] = tst # stores tst set in list
    trn.list[[iter]] = trn # stores training set in a list
    
    a_trn = assist[assist$fold == iter,]
    a_trn = select(a_trn, -c(fold, dataset_id))
    a.trn[[iter]] = a_trn
  }
  
  stl.out[[dnum]] = trn.list # stores training sets for whole dataset in list - reference with trn.out[['a']]
  mtl.out[[dnum]] = a.trn # stores the assistant task for whole dataset in list
  tst.out[[dnum]] = tst.list # stores test set for whole dataset in list form
 
}

##### Create the extended dataset ====
for(outer in 1:10){
  for(inner in 1:10){
      
    tmp1 = stl.out[[outer]][inner]
    tmp1 = do.call(rbind.data.frame, tmp1)
    tmp2 = mtl.out[[outer]][inner]
    tmp2 = do.call(rbind.data.frame, tmp2)
      
    ext = rbind(tmp1, tmp2)
    
    ex.data[[inner]] = ext
      
  }
    
  ext.data[[outer]] = ex.data
  
}
  
#### Run the MTL model ====

a = '/shared/mtl-qsar/predictions/MTL/'

###### RANDOM FOREST ######
RF.mult = foreach(trains_iter = 1:length(tst.out), .combine = "rbind") %do% {
  for(inloop in 1:10){
  trn.data = ext.data[[trains_iter]][inloop] # Select given datafold 
  trn.data = do.call(rbind.data.frame, trn.data)
  trn.data = trn.data %>% select(-molecule_id) # removes the molecule column
  
  tst.data = tst.out[[trains_iter]][inloop]# Select corresponding datafold
  tst.data = do.call(rbind.data.frame, tst.data)
  tst.data = tst.data %>% select(-molecule_id)
  
  mdl = ranger(pXC50 ~ ., trn.dset) # makes the random frest modell, with the pXC50 as the output and the remainder inputs
  
  preds = predict(mdl, data = tst.data)
  
  names = original[trains_iter]
  names = substr(names, 37, nchar(names))
  names = substr(names, 1, nchar(names)-4)
  
  print(inner) # To chek progress
  
  outp = data_frame(iter = trains_iter, true = tst.data$pXC50, predicted = preds$predictions, dataset_id = names) # stores the output with the actual values and the predicted values in seperate columns
  }
  outp
}

write_csv(RF.mult, '/shared/mtl-qsar/predictions/MTL/RF_MTL_perf.csv')
