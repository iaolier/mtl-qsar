
# Script to compute the modi metric on all of the datasets

library(devtools)
load_all(".")

path_dsets = "/shared/mtl-qsar/datasets/originals/"
path_results = "/shared/mtl-qsar/results/"
dir.create(path_results, recursive = T)

fnames = list.files(path_dsets)
names(fnames) = str_remove_all(fnames, "data_|.csv")

dsets = paste0(path_dsets, fnames) %>% 
  map(read_csv, col_types = cols(
  .default = col_double(),
  molecule_id = col_character()
))
names(dsets) = names(fnames)

res = dsets %>% map_dbl(mod_metric) # takes about 20-25 mins

tibble(dataset_id = names(res), mod_metric = res) %>% 
  write_csv(paste0(path_results, "190802_mod_metric_v1_all_datasets.csv"))
