#' Script to put datasets in the right directory
#' https://happygitwithr.com/rstudio-git-github.html

path_all_datasets <- "/home/shared-data/metaqsar_data/datasets/"
path_all_predictions <- "/home/shared-data/metaqsar_data/experiments/"

path_datasets_dest <- "/shared/mtl-qsar/datasets/originals/"
dir.create(path_datasets_dest, recursive = T)

path_predictions_dest <- "/shared/mtl-qsar/predictions/stl/"
dir.create(path_predictions_dest, recursive = T)

library(DBI)
library(tidyverse)

con1 <- dbConnect(RMySQL::MySQL(), group = "metaqsar")
dset_tbl <- inner_join(
  tbl(con1, "Dataset"),
  tbl(con1, "ProjectDataset") ) %>%
  filter(project_id == 9)

targs <- tbl(con1, "Targets") %>%
  filter(n_compounds >= 30) %>%
  inner_join(dset_tbl) %>% collect()

#datasets
fnams_dsets_from <- paste0(path_all_datasets, targs$file_name)
fnams_dsets_to <- paste0(path_datasets_dest, targs$file_name)
file.copy(from = fnams_dsets_from, to = fnams_dsets_to)

#algorithms
alg_tbl <- inner_join(
  tbl(con1, "Method"),
  tbl(con1, "ProjectMethod") ) %>%
  filter(project_id %in% c(9, 17, 18)) %>% collect()

fnams_alg <- list.files(path_all_predictions, pattern = "perf_did", recursive = T)
fnams_alg1 <- str_split_fixed(fnams_alg, "/", 2)[,2] %>%
  str_remove_all("perf_did_|.csv") %>%
  str_split_fixed("_impid_",2)

all_data <- tibble(dataset_id = as.integer(fnams_alg1[,1]),
                    method_id = as.integer(fnams_alg1[,2]),
                    fname_algs_from = fnams_alg) %>%
  inner_join(targs)
all_data <- inner_join(all_data, alg_tbl) %>%
  mutate(fname_algs_to = paste0(short_name,"/preds-did_", dataset_id, ".csv"))

walk(alg_tbl$short_name, ~ dir.create(paste0(path_predictions_dest, .x),recursive = T))

file.copy(from = paste0(path_all_predictions, all_data$fname_algs_from),
          to = paste0(path_predictions_dest, all_data$fname_algs_to))

write_csv(all_data, "/shared/mtl-qsar/190515_all_data.csv")
  