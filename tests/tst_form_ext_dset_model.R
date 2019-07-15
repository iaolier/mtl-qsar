install.packages('dplyr')
install.packages('readr')
#### Library functions ====
library(dplyr)
library(readr)

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

tst.out = list()
stl.out = list()
mtl.out = list()

for (dnum in 1:len){
  
  split = read_csv(data_splits[dnum]) # pulls out the split information for the primary task 
  assist = read_csv(assist_data[dnum]) # pull out fold info for given dataset
  orig = read_csv(original[dnum])
  
  for (iter in 1:10){
    
   # tmp1 = assist[[iter]] # pulls out the fold IDs
    
    tst = split[split$fold == iter,] # pulls out data matching the IDs
    trn = split[split$fold != iter,] # pulls out not matching IDs
    
    tst.list[[iter]] = tst # stores tst set in list
    trn.list[[iter]] = trn # stores training set in a list
    
    a_trn = assist[assist$fold == iter,]
    
    a.trn[[iter]] = a_trn
  }
  
  stl.out[[dnum]] = trn.list # stores training sets for whole dataset in list - reference with trn.out[['a']]
  mtl.out[[dnum]] = a.trn # stores the assistant task for whole dataset in list
  tst.out[[dnum]] = tst.list # stores test set for whole dataset in list form
  
  
  
}
