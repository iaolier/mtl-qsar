
# Script to compute model performance over all of the predictions. 
# RMSE, NRMSE, R2

path_preds = "/shared/mtl-qsar/predictions/stl/"
output_results = "/shared/mtl-qsar/results/190802_performance_stl_all.csv"

library(devtools)
load_all(".")

learners = list.files(path_preds)
names(learners) = learners

perf_data = map_dfr(learners, function(lrn){
  path_lrn = paste0(path_preds, lrn, "/")
  dsets_fnam = list.files(path_lrn)
  names(dsets_fnam) = str_remove_all(dsets_fnam, "preds-did_|.csv")
  map_dfr(dsets_fnam, function(fnamx){
    paste0(path_lrn, fnamx) %>% 
      read_csv(col_types = cols(
        rep = col_double(),
        fold = col_double(),
        rows_id = col_character(),
        truth = col_double(),
        prediction = col_double()
      )) %>% 
      group_by(fold) %>% 
      summarise(rmse = rmse(truth, prediction),
                nrmse = nrmse(truth, prediction),
                r2 = r2(truth, prediction))
  }, .id = "dataset")
}, .id = "learner")

write_csv(perf_data, output_results)
