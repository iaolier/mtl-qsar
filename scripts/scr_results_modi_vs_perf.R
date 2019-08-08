# Script to plot modi vs model performance

perf_results = "/shared/mtl-qsar/results/190802_performance_stl_all.csv"
#modi_results = "/shared/mtl-qsar/results/190802_mod_metric_v1_all_datasets.csv"
#modi_results = "/shared/mtl-qsar/results/190805_mod_metric_v1_all_datasets_rho05.csv"
modi_results = "/shared/mtl-qsar/results/190805_mod_metric_v1_all_datasets_rho035.csv"

library(tidyverse)

perf_data = read_csv(perf_results) %>% 
  group_by(dataset, learner) %>%
  summarise(rmse = mean(rmse),
            nrmse = mean(nrmse),
            r2 = mean(r2)) %>% ungroup()
  
modi_data = read_csv(modi_results) %>% rename(dataset = dataset_id)

perf_data = perf_data %>% inner_join(modi_data) %>%
  filter(is.finite(nrmse) & is.finite(r2))

perf_data %>%
  filter(nrmse < 2) %>%
  ggplot(aes(x = mod_metric, y = nrmse)) + 
  facet_wrap(vars(learner), nrow = 5, ncol = 4) +
  geom_point(size = 0.1)+
  geom_smooth(method=lm)

perf_data %>%
  filter(r2 > -2) %>%
  ggplot(aes(x = mod_metric, y = r2)) + 
  facet_wrap(vars(learner)) +
  geom_point(size = 0.1)+
  geom_smooth(method=lm)


perf_data %>%
  filter(nrmse < 2, learner == "rforest") %>%
  ggplot(aes(x = mod_metric, y = nrmse)) + 
  #facet_wrap(vars(learner), nrow = 5, ncol = 4) +
  geom_point(size = 0.1)+
  geom_smooth(method=lm)

perf_data %>% 
  distinct(dataset, .keep_all = T) %>%
  ggplot(aes(x=mod_metric)) + geom_histogram(aes(y=..density..),colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
