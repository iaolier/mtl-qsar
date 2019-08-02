library(devtools)
load_all(".")

dset1 <- read.csv("/shared/mtl-qsar/datasets/originals/data_30543.csv")
dset2 <- read.csv("/shared/mtl-qsar/datasets/originals/data_30546.csv")

fplist1 <- df2matfp(dset1, .fp_prefix = "FCFP4_1024b") %>%
  create_fplist

thrs <- fplist1 %>% get_thresh()
names(thrs) <- dset1$molecule_id

fplist2 <- df2matfp(dset2, .fp_prefix = "FCFP4_1024b") %>%
  create_fplist

sels <- sel_mols(fplist1, fplist2, thrs)

sd_dset1 <- sd(dset1$pXC50)

sels <- sels %>% 
  mutate(pXC50_org = dset1$pXC50[row_orig], 
         pXC50_pool = dset2$pXC50[row_pool],
         diff_pXC50 = abs(pXC50_org - pXC50_pool)) %>%
  filter(diff_pXC50 < sd_dset1) %>% 
  distinct(row_pool)

dset2 <- dset2 %>%
  slice(sels$row_pool)

### 
dset2 <- read.csv("/shared/mtl-qsar/datasets/originals/data_30546.csv")

dset_tmp <- form_assist_task(dset1, dset2)

###
fnames <- list.files("/shared/mtl-qsar/datasets/originals/", full.names = T)
fnames <- fnames[1:5]
dset_aux <- map_df(fnames, read.csv)

dset_tmp <- form_assist_task(dset1, dset_aux)

