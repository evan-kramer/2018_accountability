# 2018 Ready Graduate Student Level
# Evan Kramer
# 7/12/2018

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)

dat = T
ana = T

# Data 
if(dat == T) {
  setwd("N:/ORP_accountability/data/")
  grad = read_csv("2017_graduation_rate/student_level_20170830.csv")
  act = read_dta("2017_ACT/2018_ACT_student_level_actcohorthighest_appeals2.dta")
}

# Analysis
if(ana == T) {
  # Student level
  ready_grad_student = transmute(act, student_key = as.integer(student_key), 
                           ready_grad = as.integer(act_composite_highest >= 21)) %>% # include sat_total >= 1060 in future years; no eligible students this year
    right_join(grad, by = "student_key") %>% 
    mutate_at(vars(grad_count, ready_grad), funs(ifelse(grad_cohort == 1 & is.na(.), 0, .))) %>% 
    mutate(All = grad_cohort == 1,
           Asian = race_ethnicity == "A",
           Black = race_ethnicity == "B", 
           Hispanic = race_ethnicity == "H",
           Hawaiian = race_ethnicity == "P",
           Native = race_ethnicity == "I",
           White = race_ethnicity == "W",
           BHN = race_ethnicity %in% c("B", "H", "I"),
           ED = econ_dis == "Y",
           SWD = swd == "Y",
           EL = el == "Y",
           Non_BHN = !race_ethnicity %in% c("B", "H", "I"),
           Non_ED = econ_dis == "N",
           Non_SWD = swd == "N",
           Non_EL = el == "N",
           Super = BHN == T | ED == T | SWD == T | EL == T) 
  
  output = ready_grad_student %>%
    left_join(readxl::read_excel("2018_final_accountability_files/2017-18_E EDFacts School Master FIle_5-3-18.xls", sheet = 2) %>% 
                transmute(system = as.integer(`DG 4 LEA ID (State)`), system_name = `EXTRA ITEM - LEA Name`,
                          school = as.integer(`DG 5 School ID (State)`), school_name = `DG 7 School Name`), 
              by = c("system", "school")) %>%
    select(student_key, system, system_name, school, school_name, first_name:last_name, dob,
           gender, race_ethnicity, el, ed = econ_dis, swd, grad_cohort, grad_count, ready_grad)
  
  write_csv(ready_grad_student, "2018_final_accountability_files/student_ready_grad.csv", na = "")
  
  # School level
  ready_grad_school = tibble()
  for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
              "EL", "Non_ED", "Non_SWD", "Non_EL", "Super")) {
    
    temp = ready_grad_student %>%
      filter_(paste(s, "== T")) %>%
      group_by(system, school) %>%
      summarize_at(vars(grad_cohort, ready_grad), funs(sum(., na.rm = T))) %>% 
      mutate(subgroup = s)
    
    ready_grad_school = bind_rows(ready_grad_school, temp)
  }
  ready_grad_school = mutate(ready_grad_school, pct_ready_grad = round(100 * ready_grad / grad_cohort + 1e-9, 1), subgroup = case_when(
    subgroup == "All" ~ "All Students",
    subgroup == "Black" ~ "Black or African American",
    subgroup == "BHN" ~ "Black/Hispanic/Native American",
    subgroup == "ED" ~ "Economically Disadvantaged",
    subgroup == "EL" ~ "English Learners",
    subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
    subgroup == "Native" ~ "American Indian or Alaska Native",
    subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
    subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
    subgroup == "Non_EL" ~ "Non-English Learners",
    subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
    subgroup == "Super" ~ "Super Subgroup",
    subgroup == "SWD" ~ "Students with Disabilities",
    TRUE ~ subgroup
    )) %>% 
    arrange(system, school, subgroup) %>% 
    left_join(readxl::read_excel("2018_final_accountability_files/2017-18_E EDFacts School Master FIle_5-3-18.xls", sheet = 2) %>% 
                transmute(system = as.integer(`DG 4 LEA ID (State)`), system_name = `EXTRA ITEM - LEA Name`,
                          school = as.integer(`DG 5 School ID (State)`), school_name = `DG 7 School Name`), 
              by = c("system", "school")) %>%
    select(starts_with("system"), starts_with("school"), subgroup, grad_cohort, ends_with("ready_grad"))
  
  write_csv(ready_grad_school, "2018_final_accountability_files/school_ready_grad.csv", na = "")
  
  # District level
  ready_grad_district = tibble()
  for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
              "EL", "Non_ED", "Non_SWD", "Non_EL", "Super")) {
    
    temp = ready_grad_student %>%
      filter_(paste(s, "== T")) %>%
      group_by(system) %>%
      summarize_at(vars(grad_cohort, ready_grad), funs(sum(., na.rm = T))) %>% 
      mutate(subgroup = s)
    
    ready_grad_district = bind_rows(ready_grad_district, temp)
  }
  ready_grad_district = mutate(ready_grad_district, pct_ready_grad = round(100 * ready_grad / grad_cohort + 1e-9, 1), subgroup = case_when(
    subgroup == "All" ~ "All Students",
    subgroup == "Black" ~ "Black or African American",
    subgroup == "BHN" ~ "Black/Hispanic/Native American",
    subgroup == "ED" ~ "Economically Disadvantaged",
    subgroup == "EL" ~ "English Learners",
    subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
    subgroup == "Native" ~ "American Indian or Alaska Native",
    subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
    subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
    subgroup == "Non_EL" ~ "Non-English Learners",
    subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
    subgroup == "Super" ~ "Super Subgroup",
    subgroup == "SWD" ~ "Students with Disabilities",
    TRUE ~ subgroup
  )) %>% 
    arrange(system, subgroup) %>% 
    left_join(readxl::read_excel("2018_final_accountability_files/2017-18_E EDFacts School Master FIle_5-3-18.xls", sheet = 2) %>% 
                transmute(system = as.integer(`DG 4 LEA ID (State)`), system_name = `EXTRA ITEM - LEA Name`) %>% 
                distinct(),
              by = "system") %>%
    select(starts_with("system"), subgroup, grad_cohort, ends_with("ready_grad"))
  
  write_csv(ready_grad_district, "2018_final_accountability_files/district_ready_grad.csv", na = "")
}