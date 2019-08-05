
preds_stl_path <- "/shared/mtl-qsar/tmp/predictions/stl/ranger/"
preds_mtl_path <- "/shared/mtl-qsar/tmp/predictions/mtl/ranger/"
preds_mtl_path <- "/shared/mtl-qsar/tmp/predictions/mtl/ranger_v190709//"

library(tidyverse)

preds_mtl_fnames <- list.files(preds_mtl_path)
data_ids <- preds_mtl_fnames %>% str_remove_all("preds-did_|.csv")
names(preds_mtl_fnames) <- data_ids

preds_mtl <- preds_mtl_fnames %>% paste0(preds_mtl_path, .) %>% map_dfr(read.csv, .id = "dataset") %>% mutate(lrn_mode = "MTL")

#preds_stl_fnames <- list.files(preds_stl_path)
#names(preds_mtl_fnames) <- data_ids

preds_stl <- data_ids %>% paste0(preds_stl_path, "preds-did_",.,".csv") %>% map_dfr(read.csv, .id = "dataset") %>% mutate(lrn_mode = "STL")

preds <- bind_rows(preds_stl, preds_mtl)

preds <- preds %>% mutate(sqerr = (truth - prediction)^2)

pred_sum <- preds %>% group_by(dataset, lrn_mode, fold) %>%
  summarise(rmse = sqrt(sum(sqerr)/n())) %>% ungroup()

ggplot(pred_sum, aes(x = dataset, y = rmse, colour = lrn_mode)) + geom_boxplot()
