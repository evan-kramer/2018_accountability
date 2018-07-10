# 2018 File Release Checks
# Evan Kramer
# 7/10/2018

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
setwd("N:/ORP_accountability/projects/2018_student_level_file")

student_level = T

# Student level
if(student_level == T) {
  # Data
  jw = read_dta("state_student_level_2018_JW_final_07092018.dta")
  ap = read_csv("2018_student_level_file.csv", col_types = "iciccccccciiiidcciciiiiiiiicciiii")
  
  # Checks
  check = anti_join(mutate(jw, performance_level = ifelse(performance_level == "null", NA, performance_level)),
                           ap, by = c("id" = "state_student_id", "grade", "system", "school", "subject", "enrolled", "tested", "valid_test", "performance_level"))
  
  check = anti_join(mutate_at(ap, vars(state_student_id, grade, enrolled, tested, valid_test), funs(as.numeric(.))),
                    mutate(jw, performance_level = ifelse(performance_level == "null", NA, performance_level)),
                    by = c("state_student_id" = "id", "grade", "subject", "enrolled", "tested", "valid_test", "performance_level"))
  
  
  check = full_join(transmute(jw, id, system, school, subject, grade, test, enrolled, tested, valid_test, performance_level),
                    transmute(ap, id = as.numeric(state_student_id), subject, grade, test, enrolled, tested, valid_test, performance_level),
                    by = c("id", "subject")) %>% 
    #filter(enrolled.x != enrolled.y | (is.na(enrolled.x) & !is.na(enrolled.y)) | (is.na(enrolled.y) & !is.na(enrolled.x)))
    #filter(tested.x != tested.y | (is.na(tested.x) & !is.na(tested.y)) | (is.na(tested.y) & !is.na(tested.x)))
    #filter(valid_test.x != valid_test.y | (is.na(valid_test.x) & !is.na(valid_test.y)) | (is.na(valid_test.y) & !is.na(valid_test.x)))
    filter(performance_level.x != performance_level.y | (is.na(performance_level.x) & !is.na(performance_level.y)) | (is.na(performance_level.y) & !is.na(performance_level.x))) %>% 
    filter(performance_level.x == "Mastered" & performance_level.y == "On Track")
  
  # Specific cases 
}