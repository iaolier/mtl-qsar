
## Functions ====

df2matfp <- function(df, .fp_prefix = fp_prefix){
  as.matrix(df %>% select(starts_with(.fp_prefix))) # Extracts instances that start with a given value
}

create_fplist <- function(matr_fp){
  map(1:nrow(matr_fp), ~ new("fingerprint", nbit = ncol(matr_fp), bits=which(as.logical(matr_fp[.x,]))))
  # takes ever instace in the matrix and changes it to a fingerprint b mapping the function to all 
}

get_thresh <- function(fplist, tol = 1.0) {
  fp_sim_mat <- fp.sim.matrix(fplist) # calculate similarity matrix 
  diag(fp_sim_mat) <- 0
  th <- map_dbl(as_data_frame(fp_sim_mat), max) # extracts the highest value from each as the threshold value
  th*tol
}

sel_mols <- function(fplist_org, fplist_pool, thresholds) {
  sim_mat <- fp.sim.matrix(fplist_org, fplist_pool)
  map_df(1:nrow(sim_mat), ~ tibble(row_pool = which(sim_mat[.x,] >= thresholds[.x]), row_orig = .x )) 
}

form_assist_task <- function(dset_prim, dset_aux) {
  #create fingerprint lists
  fplist_prim <- df2matfp(dset_prim, .fp_prefix = "FCFP4_1024b") %>% create_fplist
  fplist_aux <- df2matfp(dset_aux, .fp_prefix = "FCFP4_1024b") %>% create_fplist
  
  #estimate threshold
  #thrs <- fplist_prim %>% get_thresh
  thrs <- rep(0.8, length(fplist_prim))
  
  #select molecules that tanimoto > threshold
  sels <- sel_mols(fplist_prim, fplist_aux, thrs)
  
  #std dev of pXC50 of primary dataset
  sd_dsetp <- sd(dset_prim$pXC50)
  
  #push out rows with diff > 1 sd
  sels <- sels %>% 
    mutate(pXC50_org = dset_prim$pXC50[row_orig], 
           pXC50_pool = dset_aux$pXC50[row_pool],
           diff_pXC50 = abs(pXC50_org - pXC50_pool)) %>%
    filter(diff_pXC50 < sd_dsetp) %>% 
    distinct(row_pool)
  
  #return assistant dataset
  dset_aux %>% slice(sels$row_pool)
  
}