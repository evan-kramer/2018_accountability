# Decatur Errors
# Evan Kramer
# 10/8/2018

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("N:/ORP_accountability/projects/2018_graduation_rate/Data")

# Bottom 5% - one year, all metrics, all pathways
a = read_csv("N:/ORP_accountability/projects/2018_school_accountability/priority.csv") %>% 
  select(system:designation_ineligible, tvaas_sh, grad_less_than_67, count) %>% 
  full_join(read_csv("N:/ORP_accountability/projects/2018_school_accountability/school_grading_metrics.csv") %>% 
              filter(subgroup == "All Students") %>% 
              transmute(system, school, overall_score = subgroup_average), by = c("system", "school")) %>% 
  group_by(pool) %>% 
  arrange(overall_score) %>%
  mutate(grad_less_than_67 = ifelse(is.na(grad_less_than_67), 0, grad_less_than_67),
         pctile = round(percent_rank(overall_score) * 100, 1),
         csi_all_indicators = as.integer((pctile <= 5 | grad_less_than_67 == 1) & designation_ineligible == 0 & tvaas_sh == 0))

