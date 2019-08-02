### combining data ###


files_1  <- list.files(path="/home/muireann/code/mtl.qsar/results/R2_RMSE",full.names = TRUE)
files_2 <- list.files(path="/home/muireann/code/mtl.qsar/results/NRMSE",full.names = TRUE)

tables_1 <- lapply(files_1, read.csv, header = TRUE)
tables_2 <- lapply(files_2, read.csv, header = TRUE)

combined_df_1 <- do.call(rbind , tables_1)
combined_df_2 <- do.call(rbind , tables_2)

combined_data <- cbind(combined_df_1, nrmse=combined_df_2$nrmse)

write.csv(combined_data, file="combined_data.csv")