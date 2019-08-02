#!/usr/bin/Rscript --vanilla
#  nohup ./scripts/scr01_create_assist_data_v190709.R > ~/logs/scr01_create_assist_data_v190709.log &

#' Script to create the assistant tasks 
#' FIXME: to add support for parallel processing
#' FIXME: to optionally check whether assistant datasets exists. 

library(devtools)
load_all(".")

dsets_prim_path <- "/shared/mtl-qsar/datasets/originals/"
dsets_splits_path <- "/shared/mtl-qsar/data_splits/"

dsets_assist_path <- "/shared/mtl-qsar/datasets/assist_data_v190709/"
dir.create(dsets_assist_path, recursive = T)
#Sys.chmod(dsets_assist_path, "777") #not working

# list of dataset files
dset_fnames <- list.files(dsets_prim_path)
n_dsets_all <- length(dset_fnames)

# Choose pool of datasets 
set.seed(123)
dset_pool_fnames <- dset_fnames[sample(n_dsets_all, 100)]

# Choose original dataset to apply algorithm
dset_orig_fnames <- dset_fnames[sample(n_dsets_all, 10)]

# dataset ids
dset_pool_id <- dset_pool_fnames %>% str_remove_all("data_|.csv")
dset_orig_id <- dset_orig_fnames %>% str_remove_all("data_|.csv")

# Parallel processing setup
#To be added

# Load all datasets to be used
dset_all_fnames <- c(dset_orig_fnames, dset_pool_fnames) %>% unique()
system.time(dset_all <- map(dset_all_fnames, ~ paste0(dsets_prim_path, .x) %>% read.csv))   #To be done in parallel
names(dset_all) <- dset_all_fnames %>% str_remove_all("data_|.csv")

#Load all split data
split_data <- dset_orig_id %>% map(~ paste0(dsets_splits_path, "data-split_", .x, ".csv") %>% read.csv)
names(split_data) <- dset_orig_id

# list of primary datasets
dset_orig <- dset_all[names(dset_all) %in% dset_orig_id]

# chunk size of dataset pool
chk_size <- 5
pool_splits <- data.frame(dataset_id = dset_pool_id, 
           chunk = rep(1:ceiling(length(dset_pool_id)/chk_size), chk_size)[1:length(dset_pool_id)]) %>% 
  split(.$chunk)
dset_pool <- map(pool_splits, ~ {
  dset_all[.x$dataset_id] %>% imap(~ { 
      .x %>% mutate(dataset_id = .y)
    }) %>% bind_rows()
})

rm(dset_all)    # To save some memory

# CV
cv_folds <- 10


iwalk(dset_orig, function(dset_x, did){
  dset_splits <- split_data[[did]] #splits
  dset_x <- inner_join(dset_x, dset_splits, by = c("molecule_id" = "rows_id"))
  
  #aux data
  map_df(dset_pool, function(dset_aux){
    dset_aux <- dset_aux %>% filter(dataset_id != did) # Remove training data
    #CV
    map_df(1:cv_folds, function(cviter){
      # Form training set
      dset_x %>% filter(fold != cviter) %>%
        form_assist_task(dset_aux = dset_aux) %>% mutate(fold = cviter)
    })
  }) %>% write_csv(path = paste0(dsets_assist_path, "data-assist_", did, ".csv"))
})


