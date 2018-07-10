# 2018 File Release Checks
# Evan Kramer
# 7/10/2018

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
setwd("N:/ORP_accountability/projects/2018_student_level_file")

stu = F
sta = F
dis = F
sch = F

# Student level
if(stu == T) {
  # Data
  jw = read_dta("state_student_level_2018_JW_final_07092018.dta")
  ap = read_csv("2018_student_level_file.csv", col_types = "iciccccccciiiidcciciiiiiiiicciiii")
  el = read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/EL status and variables 2018.csv")
  
  # Checks: enrolled, tested, valid tests performance levels
  check = anti_join(mutate(jw, performance_level = ifelse(performance_level == "null", NA, performance_level)),
                           ap, by = c("id" = "state_student_id", "grade", "system", "school", 
                                      "subject", "enrolled", "tested", "valid_test", "performance_level"))
  
  check = anti_join(mutate_at(ap, vars(state_student_id, grade, enrolled, tested, valid_test), funs(as.numeric(.))),
                    mutate(jw, performance_level = ifelse(performance_level == "null", NA, performance_level)),
                    by = c("state_student_id" = "id", "grade", "subject", "enrolled", "tested", "valid_test", "performance_level"))
  
  # Checks: demographics
  check = full_join(mutate_at(jw, vars(system, school, grade, bhn_group, economically_disadvantaged, ell, ell_t1t4, special_ed),
                              funs(as.integer(.))) %>% 
                      select(id, grade, system, school, subject, race, bhn_group, economically_disadvantaged, el = ell, el_t1234 = ell_t1t4, special_ed),
                    transmute(output, state_student_id, grade, system, school, subject, race, bhn_group, economically_disadvantaged,
                    #transmute(ap, state_student_id, grade, system, school, subject, race, bhn_group, economically_disadvantaged,
                              el, el_t1234, special_ed),
                    by = c("id" = "state_student_id", "system", "school", "grade", "subject")) %>% 
    filter(is.na(el_t1234.y) & !is.na(el_t1234.y)) %>% group_by(el_t1234.x, el_t1234.y) %>% 
    summarize(n = n())
  
  # Specific cases 
}

# State level
if(sta == T) {
  
}
# District level
if(dis == T) {
  
}
# School level
if(sch == T) {
  
}