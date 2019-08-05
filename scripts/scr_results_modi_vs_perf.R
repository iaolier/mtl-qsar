# Script to plot modi vs model performance

perf_results = "/shared/mtl-qsar/results/190802_performance_stl_all.csv"
modi_results = "/shared/mtl-qsar/results/190802_mod_metric_v1_all_datasets.csv"

library(tidyverse)

perf_data = read_csv(perf_results) %>% 
  group_by(dataset, learner) %>%
  summarise(rmse = mean(rmse),
            nrmse = mean(nrmse),
            r2 = mean(r2)) %>% ungroup()
  
modi_data = read_csv(modi_results) %>% rename(dataset = dataset_id)

perf_data %>% inner_join(modi_data) %>%
  filter(is.finite(nrmse) & is.finite(r2)) %>%
  filter(nrmse < 2) %>%
  ggplot(aes(x = mod_metric, y = nrmse)) + 
  facet_wrap(~learner) +
  geom_point()+
  geom_smooth(method=lm)
