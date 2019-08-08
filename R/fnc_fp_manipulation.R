
df2matfp <- function(df, .fp_prefix = fp_prefix){
  as.matrix(df %>% select(starts_with(.fp_prefix))) # Extracts instances that start with a given value
}

create_fplist <- function(matr_fp){
  map(1:nrow(matr_fp), ~ new("fingerprint", nbit = ncol(matr_fp), bits=which(as.logical(matr_fp[.x,]))))
}
