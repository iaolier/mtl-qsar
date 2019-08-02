# Testing data split function

library(devtools)
load_all(".")

#read dataset and split info
dset1 <- read.csv("/shared/mtl-qsar/datasets/originals/data_30543.csv")
dsplit1 <- read.csv("/shared/mtl-qsar/data_splits/data-split_30543.csv")

dset1 <- dset1 %>% rename(rows_id = molecule_id)
tmp <- create_splits(dset1, dsplit1)