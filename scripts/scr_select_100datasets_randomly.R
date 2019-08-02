

library(DBI)
library(tidyverse)

con1 <- dbConnect(RMySQL::MySQL(), group = "metaqsar")
dset_tbl <- inner_join(
  tbl(con1, "Dataset"),
  tbl(con1, "ProjectDataset") ) %>%
  filter(project_id == 9)

targs <- tbl(con1, "Targets") %>%
  filter(n_compounds >= 30, !str_detect(pref_name, "Dihydrofolate reductase")) %>%
  inner_join(dset_tbl) %>% collect()

set.seed(123)
targs <- targs %>%
  sample_n(100)

targs <- targs %>%
  select(c(target_id:n_compounds,dataset_id,file_name))

write_csv(targs, "1900718_target_list_100sample.csv")

paste0("/shared/mtl-qsar/datasets/originals/", targs$file_name) %>%
  file.copy(from = ., to = "/shared/mtl-qsar/datasets/originals_100sample/" )
