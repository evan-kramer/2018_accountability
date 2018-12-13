# Suppress ELPA Release
# Evan Kramer
# 10/2/2018

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)

# Switches 
sch = F
dis = F

# School level
if(sch == T) {
  a = read_csv("N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_school_suppressed.csv") %>% 
    select(-ends_with("_name")) %>% 
    left_join(read_csv("N:/ORP_accountability/data/2018_final_accountability_files/school_names.csv"),
              by = c("system", "school")) %>% 
    select(starts_with("system"), starts_with("school"), everything())
  
  names(a) = c("District ID",	"District Name", "School ID",	"School Name", "Subgroup",
               "# Students Eligible for Exit", "# Students Exited", "% Students Exited",
               "# Students Eligible for Growth", "# Students Met Expected Growth",
               "% Students Met Expected Growth",	"Average Literacy Score",	
               "Average Composite Score")
  
  write_csv(a, "N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_school_suppressed.csv", na = "")
} else {
  rm(sch)
}

# District level
if(dis == T) {
  a = read_csv("N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_district_suppressed.csv") %>% 
  select(-ends_with("_name")) %>% 
  left_join(read_csv("N:/ORP_accountability/data/2018_final_accountability_files/school_names.csv") %>% 
              filter(system != 0) %>% 
              group_by(system) %>% 
              summarize(system_name = first(system_name)) %>% 
              ungroup(), by = "system") %>% 
  select(starts_with("system"), everything())
  
  names(a) = c("District ID",	"District Name", "Subgroup",
             "# Students Eligible for Exit", "# Students Exited", "% Students Exited",
             "# Students Eligible for Growth", "# Students Met Expected Growth",
             "% Students Met Expected Growth",	"Average Literacy Score",	
             "Average Composite Score")

  write_csv(a, "N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_district_suppressed.csv", na = "")
} else {
  rm(dis)
}
