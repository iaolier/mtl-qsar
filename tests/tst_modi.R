
# Test modi metric

library(devtools)
load_all(".")

data = read_csv("/shared/mtl-qsar/datasets/originals/data_30546.csv")
mod_metric(data)
