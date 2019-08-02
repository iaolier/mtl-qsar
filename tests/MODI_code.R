#### tst_modi ####

library(tidyverse)
library(fingerprint)


out.put <- data.frame()

all.data <- list.files(path='/shared/mtl-qsar/datasets/originals')
list.data <- list.files(path='/shared/mtl-qsar/datasets/originals', full.names = TRUE)

size <- length(list.data)

fp_nbits = 1024
fp_prefix = "FCFP4_1024b"

df2matfp = function(df, .fp_prefix = fp_prefix){
  as.matrix(df %>% select(starts_with(.fp_prefix))) # Extracts instances that start with a given value
}

create_fplist = function(matr_fp){
  map(1:nrow(matr_fp), ~ new("fingerprint", nbit = ncol(matr_fp), bits=which(as.logical(matr_fp[.x,]))))
  # takes ever instace in the matrix and changes it to a fingerprint b mapping the function to all 
}


for(j in 1:size){
  data.name = list.data[j]
  dset = read.csv(data.name) # reads in each file of data
  
  print(j)
  
  data.id = all.data[j]
  
  molid_list = dset$molecule_id
  multipl_fp = dset %>% df2matfp %>% create_fplist
  names(multipl_fp) = molid_list
  
  tan_matrix <- fp.sim.matrix(multipl_fp)
  colnames(tan_matrix) <- molid_list
  rownames(tan_matrix) <- molid_list
  diag(tan_matrix) <- 0   # makes diagonal values 0 
  
  max_output <- vector() 
  
  for(i in 1:nrow(tan_matrix)){
    
    #maxtan <- max(tan_matrix[,i])
    maxtan <- which.max(tan_matrix[,i])
    max_output[i] <- maxtan
    
  }
  
  #sd_dset <- sapply(dset['pXC50'], sd)     #sd of pXC50 dset1     #sd(dset$pXC50)
  sd_dset <- sd(dset$pXC50)
  
  max_output <- as.data.frame(max_output)
  max_output <- max_output %>% 
    mutate(pXC50_1 = dset$pXC50,
           pXC50_2 = dset$pXC50[max_output],
           diff = abs(pXC50_1 - pXC50_2),
           N_metric = ifelse(diff > sd_dset, 0, 1))
  
  #N_metric <- max_output %>% 
  #  filter(max_output<sd_dset)
  
  MODI <- sum(max_output$N_metric)/nrow(max_output)
  
  #browser()
  outp = cbind(data.id, MODI)
  out.put = rbind(out.put, outp)
  
  out.p = as.data.frame(out.put)
  
}



out.p.n <- as.data.frame(as.numeric(as.character(out.p$MODI)))   #change MODI from factor to numeric
out.p.n <- cbind(out.p$data.id, out.p.n)















#### MODI_plot ####


combined_data <- read_csv("combined_data.csv")

combined_data <- combined_data %>% 
  group_by(data_id, algorithm) %>%
  summarise(rmse = mean(rmse, na.rm = T),
            r2 = mean(r2, na.rm = T),
            nrmse = mean(nrmse, na.rm = T)) %>% ungroup()


combined_data$data_id <- str_remove_all(combined_data$data_id, "preds-did_")
combined_data$data_id <- str_remove_all(combined_data$data_id, ".csv")


out.p.n$`out.p$data.id` <- str_remove_all(out.p.n$`out.p$data.id`, "data_")
out.p.n$`out.p$data.id` <- str_remove_all(out.p.n$`out.p$data.id`, ".csv")

# join datasets and filter values 

colnames(out.p.n)[1] <- "data_id"

all_data <- inner_join(combined_data, out.p.n)

colnames(all_data)[6] <- "MODI"


all_data <- all_data %>%
  filter(all_data$nrmse <2 )



ggplot(all_data, mapping = aes(x=MODI, y=nrmse)) +
  facet_wrap(~algorithm) +
  geom_point() +
  geom_smooth(method=lm)