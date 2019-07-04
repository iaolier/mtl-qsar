
## Functions ====

df2matfp = function(df, .fp_prefix = fp_prefix){
  as.matrix(df %>% select(starts_with(.fp_prefix))) # Extracts instances that start with a given value
}

create_fplist = function(matr_fp){
  map(1:nrow(matr_fp), ~ new("fingerprint", nbit = ncol(matr_fp), bits=which(as.logical(matr_fp[.x,]))))
  # takes ever instace in the matrix and changes it to a fingerprint b mapping the function to all 
}

get_thresh = function(fplist, tol = 1.0) {
  fp_sim_mat = fp.sim.matrix(fplist) # calculate similarity matrix 
  map_dbl(as_data_frame(fp_sim_mat), max) # extracts the highest value from each as the threshold value
}

