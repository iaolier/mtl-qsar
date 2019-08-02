#!/usr/bin/Rscript --vanilla
#  nohup ./tests/tst_mtl_preds_10data.R > ~/logs/tst_mtl_preds_10data.log &

#' Script to compare stl and mtl through RF model performance.

library(devtools)
load_all(".")

#paths
dsets_prim_path <- "/shared/mtl-qsar/datasets/originals/"
dsets_splits_path <- "/shared/mtl-qsar/data_splits/"
dsets_assist_path <- "/shared/mtl-qsar/datasets/assist_data_v190709/"

#output path
preds_stl_path <- "/shared/mtl-qsar/tmp/predictions/stl/ranger/"
dir.create(preds_stl_path, recursive = T)
preds_mtl_path <- "/shared/mtl-qsar/tmp/predictions/mtl/ranger_v190709/"
dir.create(preds_mtl_path, recursive = T)


# available assistant datasets:
assist_data_fnam <- list.files(dsets_assist_path)

# get ids
data_ids <- assist_data_fnam %>% str_remove_all("data-assist_|.csv")


library(ranger)

make_preds_ranger <- function(dset_trn, dset_tst) {
  #browser()
  mdl <- ranger(pXC50 ~ ., data = dset_trn)
  dset_tst %>% select(rows_id = molecule_id, truth = pXC50) %>% 
    mutate(prediction = (predict(mdl, data = dset_tst))$predictions)
}

walk(data_ids, function(did){
  dset <- paste0(dsets_prim_path,"data_",did,".csv") %>% read.csv
  d_split <- paste0(dsets_splits_path,"data-split_",did,".csv") %>% read.csv %>% rename(molecule_id = rows_id)
  d_assist <- paste0(dsets_assist_path,"data-assist_",did,".csv") %>% read.csv
  #browser()
  dset <- inner_join(d_split, dset)
  dset_tests <- dset %>% split(.$fold)
  dset_prims <- map(1:10, ~ filter(dset, fold != .))
  dset_assists <- map(1:10, ~ filter(d_assist, fold == .))
  dset_exts <- map(1:10, ~ bind_rows(dset_prims[[.]], dset_assists[[.]]))
  dset_exts <- map(dset_exts, ~ select(.x, -c(molecule_id, fold, dataset_id)))
  dset_prims <- map(dset_prims, ~ select(.x, -c(molecule_id, fold)))
  map_dfr(1:10, ~ make_preds_ranger(dset_prims[[.x]], dset_tests[[.x]]), .id = "fold") %>% 
    write_csv(path = paste0(preds_stl_path, "preds-did_", did, ".csv"))
  map_dfr(1:10, ~ make_preds_ranger(dset_exts[[.x]], dset_tests[[.x]]), .id = "fold") %>% 
    write_csv(path = paste0(preds_mtl_path, "preds-did_", did, ".csv"))
})
