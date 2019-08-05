#' Function to form the data splits
#' 

library(tidyverse)

# take data from the directory that contains random forest predictions (it's the most complete directory)
path_preds <- "/shared/mtl-qsar/predictions/stl/rforest/"
fnames <- list.files(path_preds)
dataset_ids <- fnames %>% str_remove_all("preds-did_|.csv")
names(fnames) <- dataset_ids

path_splits <- "/shared/mtl-qsar/data_splits/"
dir.create(path_splits, recursive = T)

iwalk(fnames, ~ {
  read.csv(paste0(path_preds, .x)) %>%
    select(rows_id, fold) %>%
    write_csv(paste0(path_splits, "data-split_", .y, ".csv"))
})
