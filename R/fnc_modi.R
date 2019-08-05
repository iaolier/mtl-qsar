
# modi-like metric: for a dataset:
# estimate similarity matrix 
# for each compound choose the most similar one and get the difference in activity
# if difference is > sd(activity) 1, else 0
# N_metric = # of 1s
# metric = N_metric / Ntotal

make_diag_0 = function(.mat) {
  diag(.mat) = 0
  .mat
}

get_indx_max_sim = function(.mat) {
  .mat %>% make_diag_0 %>% 
    apply(MARGIN=2,which.max)
}

mod_metric = function(.data) {
  indx_sim = .data %>% 
    df2matfp(.fp_prefix = "FCFP4_1024") %>% 
    create_fplist %>%
    fp.sim.matrix %>% 
    get_indx_max_sim 
  
  sd_dset = sd(.data$pXC50)
  
  .data = .data %>% 
    mutate(pXC50_2 = pXC50[indx_sim],
           diff = abs(pXC50 - pXC50_2),
           N_sim = ifelse(diff > sd_dset, 0, 1))
  
  sum(.data$N_sim)/nrow(.data)
}