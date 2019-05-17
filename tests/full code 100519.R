############################################################################################################
############################################################################################################
######################################### Functions and Libraries# #########################################
############################################################################################################
############################################################################################################

## Functions ====
# Tanimoto score
tani.dist = function(){
  fp_sims = fp.sim.matrix(trn_set, tst_mol) # Compares how similar two sets of fingerprints are
}

split_dset = function(iter, .dset){
  list(tst_set = .dset %>% filter(cvf$which == iter), # splits the dataset into test and training samples
       trn_set = .dset %>% filter(cvf$which != iter)) 
}

df2matfp = function(df, .fp_prefix = fp_prefix){
  as.matrix(df %>% select(starts_with(.fp_prefix))) # Extracts instances that start with a given value
}

create_fplist = function(matr_fp){
  map(1:nrow(matr_fp), ~ new("fingerprint", nbit = ncol(matr_fp), bits=which(as.logical(matr_fp[.x,]))))
  # takes ever instace in the matrix and changes it to a fingerprint b mapping the function to all 
}

calc_thresh = function(samp_it) {
  fp_sim_mat = fp.sim.matrix(multipl_fp[samp_it$trn_set$molecule_id],
                             multipl_fp[samp_it$tst_set$molecule_id]) # calculate similarity matrix 
  map_dbl(as_data_frame(fp_sim_mat), max) # extracts the highest value from each as the threshold value
}

# Subsetted Data
sub_data = function(threshs, assi_molid){
  thre_molid = names(threshs) # rename
  fp_sim_mat = fp.sim.matrix(multipl_fp[assi_molid],
                             multipl_fp[thre_molid]) # compares similarity 
  simplify(map2(as_data_frame(fp_sim_mat), threshs, ~ which(.x > .y))) # WHAT DOES THIS MEAN?
}

create_extdset = function(dseti){
  prim_dset = multipl %>% filter(File == dseti) # WHAT DOES THIS MEAN? 
  assi_dset = multipl %>% filter(File != dseti) # WHAT DOES THIS MEAN?
  
  cvf = cvFolds(nrow(prim_dset), CV.split) # performs the 10fold cv
  samp_dsets = map(1:CV.split, 
                   ~ list(tst_set = prim_dset %>% filter(cvf$which == .x),
                          trn_set = prim_dset %>% filter(cvf$which != .x)) )
  
 
  threshs = samp_dsets %>% map(calc_thresh) %>% simplify # WHAT DOES THIS MEAN?
  
  sub_ind = sub_data(threshs, assi_dset$molecule_id) ##
  
  # Add assistant dataset 
  if(length(sub_ind) > 0) {
    assi_dset = assi_dset %>% slice(sub_ind)
    prim_dset = prim_dset %>% bind_rows(assi_dset)
  }
  
  # Add fingerprints
  fp_dset = as_data_frame(fp.to.matrix(multipl_fp[prim_dset$molecule_id]))
  names(fp_dset) = fpcolnams # renames the columns
  prim_dset = prim_dset %>% bind_cols(fp_dset) %>% distinct(molecule_id, .keep_all = T) %>% select(-File)
  write_csv(prim_dset, paste0(path_dset_outp, "ext_", dseti)) # stores as csv file and adds string to name
}


## Packages ====
library(tidyverse)
library(ggplot2)
library(foreach)
library(cvTools)
library(readbulk)
library(fingerprint)
library(ranger)
library(randomForest)
library(infotheo)


############################################################################################################
############################################################################################################
########################################## Create the 90-10 Splits #########################################
############################################################################################################
############################################################################################################

## Set Working Directory ====
setwd("C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Work/MTL Paper")
a = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Work/MTL Paper"

dats = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Work/MTL Paper/100_Dataset_Sample" # 180327_data_sample_qsars
dat.sz = list.files(dats, full.names = T)

## Paths ====
path_dset_outp = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Work/MTL Paper/100 Datasets/Splits Train/"
dir.create(path_dset_outp, recursive = T)

path_dset_outp_test = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Work/MTL Paper/100 Datasets/Splits Test/"
dir.create(path_dset_outp_test, recursive = T)

## Seed for reproducability ====
set.seed(123)

## Number of Splits ====
CV.split = 10
nrow = length(dat.sz)

## Split 90% - 10% ====
for (i in 1:nrow) {
  data_fname = dat.sz[i] 
  data = read_csv(data_fname) # reads in each file of data
  
  a = nchar(data_fname) # Number of characters
  abrev_name = substr(data_fname, (a - 13), a) # shortened name
  
  # creates the data splitted into CV.split groups
  cv = cvFolds(n = nrow(data), CV.split) # trying to make sure that the groups are of equal size
  
  index = foreach(iter = 1:CV.split, .combine = "rbind") %do% {
    trn.id = cv$subsets[cv$which != iter] # Takes out row ids that are not equal to the iteration number
    tst.id = cv$subsets[cv$which == iter] # test set are the row ids that are equal to the iteration number
    
    index = list(train = trn.id, test = tst.id) # stores test and train id in list
  }
  
  for (j in 1:CV.split){
    
    a_name = substr(abrev_name, 1, 
                    nchar(abrev_name)-4) # removes the .csv part
    
    a_name = paste(a_name, j-1, sep = '_') # adds iter value to name
    abrevname = paste(a_name, '.csv', sep = '') # adds '.csv' to name
    
    trn.data = slice(data, index[[j]]) # Takes out the training sample based on the row ids
    write_csv(trn.data, paste0(path_dset_outp, "CV_train_", abrevname)) # stores as csv file and adds string to name
    
    tst.data = slice(data, index[[j + 10]]) # Takes out test sample based on the row ids
    write_csv(tst.data, paste0(path_dset_outp_test, "CV_test_", abrevname))
  }
  
}

############################################################################################################
############################################################################################################
######################################## Create an extended dataset ########################################
############################################################################################################
############################################################################################################

## Fingerprints
fp_nbits = 1024
fp_prefix = "FCFP4_1024b"

## Seed for reproducability ====
set.seed(123)

## Set working directory ====
setwd("C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Work/MTL Paper/100 Datasets")

## Paths ====
path_dsets = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Work/MTL Paper/100 Datasets/Splits Train/"
path_dset_outp = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Work/MTL Paper/100 Datasets/extended data/"
dir.create(path_dset_outp, recursive = T)

## Read in all datasets ====
multipl = read_bulk(directory = path_dsets, # Finds the path for the files
                    fun = readr::read_csv,
                    col_types = cols(
                      .default = col_integer(),
                      molecule_id = col_character(),
                      pXC50 = col_double()
                    ))

## Fingerprints
fp_nbits = 1024
fp_prefix = "FCFP4_1024b"
CV.split = 10

## Fingerprint representation ====
multipl_fp = multipl %>% distinct(molecule_id, .keep_all = T) 
molid_list = multipl_fp$molecule_id
multipl_fp = multipl_fp %>% df2matfp %>% create_fplist
names(multipl_fp) = molid_list # renames the columns

# Drop fingerprints from datasets to save memory
# ... but before store fp col names
fpcolnams = str_subset(names(multipl), fp_prefix) 
multipl = multipl %>% select(-starts_with(fp_prefix))

train.split = list.files(path_dsets)

walk(train.split, create_extdset)

############################################################################################################
############################################################################################################
########################################## Create the data splits ##########################################
############################################################################################################
############################################################################################################

## Set Working Directory ====
setwd("G:/MTL Paper/datasets_dhfr/Piled")

## Paths ====
path_dset_outp = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Paper/100 Datasets/CV Single splits Train/"
dir.create(path_dset_outp, recursive = T)

path_dset_outp_ext = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Paper/100 Datasets/CV Assistant Splits Train/"
dir.create(path_dset_outp_ext, recursive = T)

## List the files in the folder needed for the analysis  ====
file = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Paper/100 Datasets/Splits Train"
list_mydata.t = list.files(file, 
                           full.names = T)
list_mydata.t = list_mydata.t

file.ext = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/PhD/MTL Paper/100 Datasets/extended"
list_mydata.ext = list.files(file.ext, 
                             full.names = T)

sz = length(list_mydata.t)

## Seed for reproducability ====
set.seed(123)

## Arrange the splits in the data ====
for( i in 1: sz){
  data_fname = list_mydata.t[i] 
  data = read_csv(data_fname) # reads in each file of data
  
  a = nchar(data_fname) # Number of characters
  abrev_name = substr(data_fname, (a - 13), a) # shortened name
  
  # creates the data splitted into CV.split groups
  cv = cvFolds(n = nrow(data), CV.split) # trying to make sure that the groups are of equal size
  
  index = foreach(iter = 1:CV.split, .combine = "rbind") %do% {
    trn.id = cv$subsets[cv$which != iter] # Takes out row ids that are not equal to the iteration number
    tst.id = cv$subsets[cv$which == iter] # test set are the row ids that are equal to the iteration number
    
    index = list(train = trn.id, test = tst.id) # stores test and train id in list
  }
  
  for (j in 1:CV.split){
    
    a_name = substr(abrev_name, 1, 
                    nchar(abrev_name)-4) # removes the .csv part
    
    a_name = paste(a_name, j-1, sep = '_') # adds iter value to name
    abrevname = paste(a_name, '.csv', sep = '') # adds '.csv' to name
    
    trn.data = slice(data, index[[j]]) # Takes out the training sample based on the row ids
    write_csv(trn.data, paste0(path_dset_outp, "CV_train_", abrevname)) # stores as csv file and adds string to name
    
  }
  
  data_fname.ext = list_mydata.ext[i]
  data.ext = read_csv(data_fname.ext)# reads in each file of data
  
  prim = nrow(data[i]) # counts rows in primary data
  end = nrow(data.ext[i]) # number of rowws in extened dataset
  
  extd = c(((prim + 1):end))
  ext.dset = list(extended = extd)
  
  assist.data = slice(data.ext, ext.dset[[1]])
  
  size = nrow(assist.data)
  if (size < CV.split){ # Restriction in R needs size to be at least equal to k folds for CV to work
    blanks = matrix(NA, nrow = (CV.split - size), ncol = (ncol(assist.data))) # if not, matrix of NA (not changing anything ) for the missing number of rows is generated
    colnames(blanks) = colnames(assist.data) # for binding, ensures column names are the same
    assist.data = rbind(assist.data, blanks) # binds NA's and the assistant data 
  }
  
  cv.assist = cvFolds(n = nrow(assist.data), CV.split) # cross validation the assistant tasks
  
  index.assist = foreach(iter = 1:CV.split, .combine = "rbind") %do% {
    trn.id = cv$subsets[cv$which != iter] # Takes out row ids that are not equal to the iteration number
    tst.id = cv$subsets[cv$which == iter] # test set are the row ids that are equal to the iteration number
    
    index = list(train = trn.id, test = tst.id) # stores test and train id in list
  }
  
  for (j in 1:CV.split){
    
    a_name = substr(abrev_name, 1, 
                    nchar(abrev_name)-4) # removes the .csv part
    
    a_name = paste(a_name, j-1, sep = '_') # adds iter value to name
    abrevname = paste(a_name, '.csv', sep = '') # adds '.csv' to name
    
    trn.data = slice(data, index.assist[[j]]) # Takes out the training sample based on the row ids
    
    write_csv(assist.data, paste0(path_dset_outp_ext, "assist_", abrevname)) # stores as csv file and adds string to name
  }
}

############################################################################################################
############################################################################################################
############################################ Reformat the data #############################################
############################################################################################################
############################################################################################################

## Set Working Directory ====
setwd("C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/MTL Paper/100 Datasets/Alternative Approach")

## Seed for reproducability ====
set.seed(123)

## List the files in the folder needed for the analysis ====
file_locations = list.files(path = getwd(), 
                            full.names = F)

file_locations

list_assist <-list.files(file_locations[1], 
                         full.names = T)

list_CVtest <-list.files(file_locations[2], 
                         full.names = T)

list_CVtrain <-list.files(file_locations[3], 
                          full.names = T)

## Generate storage files ====
path_dset_trn = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/MTL Paper/100 Datasets/Alternative Approach/Train data/"
dir.create(path_dset_trn, recursive = T)

path_dset_tst = "C:/Users/cmppmcc1/OneDrive - Liverpool John Moores University/MTL Paper/100 Datasets/Alternative Approach/Test data/"
dir.create(path_dset_tst, recursive = T)

file.copy(list_CVtest, path_dset_tst) # copy test files to new folder

## Format training data ====
train = foreach(tr.file = list_CVtrain) %do% {
  read_csv(tr.file)
}

added = foreach(assist = list_assist) %do% {
  read_csv(assist)
}

## Run extender ====
for (i in 1:length(list_CVtrain)) {
  data_fname = list_CVtrain[i]
  a = nchar(data_fname)
  abrev_name = substr(data_fname, (a - 15), a)
  
  dat = rbind(train[[i]], added[[i]])
  
  write_csv(dat, paste0(path_dset_trn, abrev_name))
}

############################################################################################################
############################################################################################################
##################################### Run random forest - STL and MTL ######################################
############################################################################################################
############################################################################################################

## Set Working Directory ====
setwd("F:/MTL Paper/10 Datasets/Standard Assistant/")
a = "F:/MTL Paper/10 Datasets/Standard Assistant"

## Seed for reproducability ====
set.seed(123)

## List the files in the folder needed for the analysis ====
file_locations = list.files(path = getwd(), 
                            full.names = F)

file_locations # check positions

list_added.tr <-list.files(file_locations[6], 
                           full.names = T) # change position number if needed

list_CVtest <-list.files(file_locations[5], 
                         full.names = T)

list_CVtrain <-list.files(file_locations[3], 
                          full.names = T)

# Iterations for 10-fold cross validation
n_iters = 10

## Format the data ====
trains = foreach(data_tr.name = list_CVtrain) %do% {
  read_csv(data_tr.name) # reads in each file of data
}

tests = foreach(data_tst.name = list_CVtest) %do% {
  read_csv(data_tst.name)
}

trains.added = foreach(data_tr.name = list_added.tr) %do% {
  read_csv(data_tr.name) 
}


## Run the Random Forest ====
############################################################
#### SINGLE TASK ====
RF.sep = foreach(trains_iter = 1:length(trains), .combine = "rbind") %do% {
  trn.data = trains[[trains_iter]] # Select given datafold 
  trn.dset = trn.data %>% select(-molecule_id) # removes the molecule column
  
  tst.data = tests[[trains_iter]] # Select corresponding datafold
  tst.dset = tst.data %>% select(-molecule_id)
  
  mdl = ranger(pXC50 ~ ., data = trn.dset) # makes the random frest modell, with the pXC50 as the output and the remainder inputs
  
  preds = predict(mdl, data = tst.dset)
  
  names = list_CVtrain[trains_iter]
  names = substr(names, 26, nchar(names))
  names = substr(names, 1, nchar(names)-6)
  
  outp = data_frame(iter = trains_iter, true = tst.dset$pXC50, predicted = preds$predictions, dataset_id = names) # stores the output with the actual values and the predicted values in seperate columns
}

write_csv(RF.sep, paste0(a, 'RF single perf', '.csv'))
############################################################

#### MTL ====
RF.mult = foreach(trains_iter = 1:length(tests), .combine = "rbind") %do% {
  trn.data = trains.added[[trains_iter]] # Select given datafold 
  trn.data = na.omit(trn.data)
  trn.dset = trn.data %>% select(-molecule_id) # removes the molecule column
  
  tst.data = tests[[trains_iter]] # Select corresponding datafold
  tst.dset = tst.data %>% select(-molecule_id)
  
  mdl = ranger(pXC50 ~ ., trn.dset) # makes the random frest modell, with the pXC50 as the output and the remainder inputs
  
  preds = predict(mdl, data = tst.dset)
  
  names = list_CVtrain[trains_iter]
  names = substr(names, 26, nchar(names))
  names = substr(names, 1, nchar(names)-6)
  
  print(trains_iter) # To chek progress
  
  outp = data_frame(iter = trains_iter, true = tst.dset$pXC50, predicted = preds$predictions, dataset_id = names) # stores the output with the actual values and the predicted values in seperate columns
}

write_csv(RF.mult, paste0(a, 'RF MTL perf', '.csv'))



