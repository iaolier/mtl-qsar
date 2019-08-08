#!/usr/bin/Rscript --vanilla
#  nohup ./scripts/scr01_create_assist_data.R > ~/logs/scr01_create_assist_data.log &
rm(list = ls())
library(devtools)
load_all(".")

dsets_path = "/shared/mtl-qsar/datasets/originals/"

# list of dataset files
dset_fnames = list.files(dsets_path)
n_dsets_all = length(dset_fnames)

# Choose pool of datasets 
set.seed(123)
dset_pool_fnames = dset_fnames[sample(n_dsets_all, 100)]

# dataset ids
dset_pool_id = dset_pool_fnames %>% str_remove_all("data_|.csv")

# load datasets to memory
system.time(dset_pool <- paste0(dsets_path, dset_pool_fnames) %>% 
              map(read_csv, col_types = cols(
                .default = col_double(),
                molecule_id = col_character())
              )
            )
names(dset_pool) = dset_pool_id

dset_pool = bind_rows(dset_pool, .id = "dataset_id")

# Get pXC50 info
activity_data = dset_pool %>% 
  select(dataset_id, molecule_id, pXC50) 

# Get similarity matrix
system.time(
sim_mat <- dset_pool %>% distinct(molecule_id, .keep_all = T) %>%
  df2matfp(.fp_prefix = "FCFP4_1024b") %>% 
  create_fplist %>%
  fp.sim.matrix
)

library(bigmemory)

sim_mat_big = as.big.matrix(sim_mat)
temp_dir = "/shared"

system.time(
write.big.matrix(sim_mat_big, file.path(temp_dir, "foo.txt"))
)
