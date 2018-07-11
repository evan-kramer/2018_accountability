# 2018 File Release Checks
# Evan Kramer
# 7/11/2018

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)

stu = F
sta = F
dis = F
sch = F
gr2 = T 
elp = F

# Student level
if(stu == T) {
  # Data
  setwd("N:/ORP_accountability/projects/2018_student_level_file")
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
  # Data
  setwd("N:/ORP_accountability/data/2018_final_accountability_files")
  jw = read_csv("assessment_file_2018_JW_state_07092018.csv") %>% 
    mutate(subgroup = ifelse(str_detect(subgroup, "Learner") == F, subgroup, case_when(
      subgroup == "English Language Learners" ~ "English Learners",
      subgroup == "English Language Learners with T1/T2/T3/T4" ~ "English Learners with Transitional 1-4",
      subgroup == "Former English Language Learners T1/T2/T3/T4" ~ "English Learner Transitional 1-4",
      subgroup == "Non-English Language Learners" ~ "Non-English Learners",
      subgroup == "Non-English Language Learners with T1/T2/T3/T4" ~ "Non-English Learners/Transitional 1-4",
      subgroup == "English Language Learners with T1/T2" ~ "English Learners with Transitional 1-2",
      subgroup == "Former English Language Learners T1/T2" ~ "English Learner Transitional 1-2",
      subgroup == "Non-English Language Learners with T1/T2" ~ "Non-English Learners/Transitional 1-2"
    )),
    test = ifelse(test %in% c("MSAA", "ALT_SCI"), "MSAA/Alt-Science", test)) %>% 
    rename(n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
           pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv, pct_on_mastered = pct_ontrack_prof_adv)
  ap = read_csv("state_assessment_file.csv")
  
  # Checks: enrolled, tested, valid_tests, performance levels/percentages
  check = full_join(select(jw, year, system, test, subject, grade, subgroup, enrolled, tested, valid_tests, starts_with("n_"), starts_with("pct_")),
                    select(ap, year, system, test, subject, grade, subgroup, enrolled, tested, valid_tests, starts_with("n_"), starts_with("pct_")),
                    by = c("year", "test", "system", "grade", "subgroup", "subject")) %>% 
    # select(year:subgroup, starts_with("enrolled."), starts_with("tested."), starts_with("valid_tests.")) %>%
    # filter(enrolled.x != enrolled.y | (is.na(enrolled.x) & !is.na(enrolled.y)) | (is.na(enrolled.y) & !is.na(enrolled.x))) %>%
    # filter(tested.x != tested.y | (is.na(tested.x) & !is.na(tested.y)) | (is.na(tested.y) & !is.na(tested.x))) %>%
    # filter(valid_tests.x != valid_tests.y | (is.na(valid_tests.x) & !is.na(valid_tests.y)) | (is.na(valid_tests.y) & !is.na(valid_tests.x))) %>%
    # filter(!is.na(subject) & subgroup != "Non-English Learners" & subject != "Science" & !grade %in% c("3", "4") & enrolled.x != 0)
    
    select(year:subgroup, starts_with("valid_tests"), starts_with("n_below"), starts_with("pct_below")) %>%
    filter(abs(pct_below.x - pct_below.y) >= 0.1 | (is.na(pct_below.x) & !is.na(pct_below.y)) | (is.na(pct_below.y) & !is.na(pct_below.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_approaching"), starts_with("pct_approaching")) %>%
    # filter(abs(pct_approaching.x - pct_approaching.y) >= 0.1 | (is.na(pct_approaching.x) & !is.na(pct_approaching.y)) | (is.na(pct_approaching.y) & !is.na(pct_approaching.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_approaching"), starts_with("pct_approaching")) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_on_track"), starts_with("pct_on_track")) %>%
    # filter(abs(pct_on_track.x - pct_on_track.y) >= 0.1 | (is.na(pct_on_track.x) & !is.na(pct_on_track.y)) | (is.na(pct_on_track.y) & !is.na(pct_on_track.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_mastered"), starts_with("pct_mastered")) %>%
    # filter(abs(pct_mastered.x - pct_mastered.y) >= 0.1 | (is.na(pct_mastered.x) & !is.na(pct_mastered.y)) | (is.na(pct_mastered.y) & !is.na(pct_mastered.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_on_track"), starts_with("n_mastered"), starts_with("pct_on_mastered")) %>%
    # filter(abs(pct_on_mastered.x - pct_on_mastered.y) >= 0.1 | (is.na(pct_on_mastered.x) & !is.na(pct_on_mastered.y)) | (is.na(pct_on_mastered.y) & !is.na(pct_on_mastered.x))) %>%
    filter(!is.na(subject) & subgroup != "Non-English Learners" & valid_tests.x != 0 & subject != "Science" & !grade %in% c("3", "4"))
}

# District level
if(dis == T) {
  # Data
  setwd("N:/ORP_accountability/data/2018_final_accountability_files")
  jw = read_csv("assessment_file_2018_JW_system_07092018.csv") %>% 
    mutate(subgroup = ifelse(str_detect(subgroup, "Learner") == F, subgroup, case_when(
      subgroup == "English Language Learners" ~ "English Learners",
      subgroup == "English Language Learners with T1/T2/T3/T4" ~ "English Learners with Transitional 1-4",
      subgroup == "Former English Language Learners T1/T2/T3/T4" ~ "English Learner Transitional 1-4",
      subgroup == "Non-English Language Learners" ~ "Non-English Learners",
      subgroup == "Non-English Language Learners with T1/T2/T3/T4" ~ "Non-English Learners/Transitional 1-4",
      subgroup == "English Language Learners with T1/T2" ~ "English Learners with Transitional 1-2",
      subgroup == "Former English Language Learners T1/T2" ~ "English Learner Transitional 1-2",
      subgroup == "Non-English Language Learners with T1/T2" ~ "Non-English Learners/Transitional 1-2"
    )),
    test = ifelse(test %in% c("MSAA", "ALT_SCI"), "MSAA/Alt-Science", test)) %>% 
    rename(n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
           pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv, pct_on_mastered = pct_ontrack_prof_adv)
  ap = read_csv("district_assessment_file.csv", col_types = "iicccccdddiiiiddddd")
  
  # Checks: enrolled, tested, valid_tests, performance levels/percentages
  check = full_join(select(jw, year, system, test, subject, grade, subgroup, enrolled, tested, valid_tests, starts_with("n_"), starts_with("pct_")),
                    select(ap, year, system, test, subject, grade, subgroup, enrolled, tested, valid_tests, starts_with("n_"), starts_with("pct_")),
                    by = c("year", "test", "system", "grade", "subgroup", "subject")) %>% 
    
    select(year:subgroup, starts_with("enrolled."), starts_with("tested."), starts_with("valid_tests.")) %>%
    filter(enrolled.x != enrolled.y | (is.na(enrolled.x) & !is.na(enrolled.y)) | (is.na(enrolled.y) & !is.na(enrolled.x))) %>%
    # filter(tested.x != tested.y | (is.na(tested.x) & !is.na(tested.y)) | (is.na(tested.y) & !is.na(tested.x))) %>%
    # filter(valid_tests.x != valid_tests.y | (is.na(valid_tests.x) & !is.na(valid_tests.y)) | (is.na(valid_tests.y) & !is.na(valid_tests.x))) %>%
    filter(enrolled.x != 0 & subgroup != "Non-English Learners" & subject != "Science" & !grade %in% c("3", "4"))
    
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_below"), starts_with("pct_below")) %>%
    # filter(abs(pct_below.x - pct_below.y) >= 0.1 | (is.na(pct_below.x) & !is.na(pct_below.y)) | (is.na(pct_below.y) & !is.na(pct_below.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_approaching"), starts_with("pct_approaching")) %>%
    # filter(abs(pct_approaching.x - pct_approaching.y) >= 0.1 | (is.na(pct_approaching.x) & !is.na(pct_approaching.y)) | (is.na(pct_approaching.y) & !is.na(pct_approaching.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_on_track"), starts_with("pct_on_track")) %>%
    # filter(abs(pct_on_track.x - pct_on_track.y) >= 0.1 | (is.na(pct_on_track.x) & !is.na(pct_on_track.y)) | (is.na(pct_on_track.y) & !is.na(pct_on_track.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_mastered"), starts_with("pct_mastered")) %>%
    # filter(abs(pct_mastered.x - pct_mastered.y) >= 0.1 | (is.na(pct_mastered.x) & !is.na(pct_mastered.y)) | (is.na(pct_mastered.y) & !is.na(pct_mastered.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_on_track"), starts_with("n_mastered"), starts_with("pct_on_mastered")) %>%
    # filter(abs(pct_on_mastered.x - pct_on_mastered.y) >= 0.1 | (is.na(pct_on_mastered.x) & !is.na(pct_on_mastered.y)) | (is.na(pct_on_mastered.y) & !is.na(pct_on_mastered.x))) %>%
    # filter(!is.na(subject) & subgroup != "Non-English Learners" & valid_tests.x != 0 & subject != "Science" & !grade %in% c("3", "4"))
  
  # Specific cases
  filter(jw, system == 170 & test == "EOC" & grade == "11" & subject == "US History" & 
           subgroup == "Non-English Learners/Transitional 1-2") %>% 
    View()
}

# School level
if(sch == T) {
  # Data
  setwd("N:/ORP_accountability/data/2018_final_accountability_files")
  jw = read_csv("assessment_file_2018_JW_school_07092018.csv") %>% 
    mutate(subgroup = ifelse(str_detect(subgroup, "Learner") == F, subgroup, case_when(
      subgroup == "English Language Learners" ~ "English Learners",
      subgroup == "English Language Learners with T1/T2/T3/T4" ~ "English Learners with Transitional 1-4",
      subgroup == "Former English Language Learners T1/T2/T3/T4" ~ "English Learner Transitional 1-4",
      subgroup == "Non-English Language Learners" ~ "Non-English Learners",
      subgroup == "Non-English Language Learners with T1/T2/T3/T4" ~ "Non-English Learners/Transitional 1-4",
      subgroup == "English Language Learners with T1/T2" ~ "English Learners with Transitional 1-2",
      subgroup == "Former English Language Learners T1/T2" ~ "English Learner Transitional 1-2",
      subgroup == "Non-English Language Learners with T1/T2" ~ "Non-English Learners/Transitional 1-2"
    )),
    test = ifelse(test %in% c("MSAA", "ALT_SCI"), "MSAA/Alt-Science", test),
    grade = ifelse(grade == "0", "Missing Grade", grade)) %>% 
    rename(n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
           pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv, pct_on_mastered = pct_ontrack_prof_adv)
  ap = read_csv("school_assessment_file.csv", col_types = "iicicccccdddiiiiddddd")
  
  # Checks: enrolled, tested, valid_tests, performance levels/percentages
  check = full_join(select(jw, year, system, school, test, subject, grade, subgroup, enrolled, tested, valid_tests, starts_with("n_"), starts_with("pct_")),
                    select(ap, year, system, school, test, subject, grade, subgroup, enrolled, tested, valid_tests, starts_with("n_"), starts_with("pct_")),
                    by = c("year", "test", "system", "school", "grade", "subgroup", "subject")) %>% 
    
    # select(year:subgroup, starts_with("enrolled."), starts_with("tested."), starts_with("valid_tests.")) %>%
    # filter(enrolled.x != enrolled.y | (is.na(enrolled.x) & !is.na(enrolled.y)) | (is.na(enrolled.y) & !is.na(enrolled.x))) %>%
    # filter(tested.x != tested.y | (is.na(tested.x) & !is.na(tested.y)) | (is.na(tested.y) & !is.na(tested.x))) %>%
    # filter(valid_tests.x != valid_tests.y | (is.na(valid_tests.x) & !is.na(valid_tests.y)) | (is.na(valid_tests.y) & !is.na(valid_tests.x))) %>%
    # filter(enrolled.x != 0 & subgroup != "Non-English Learners" & subject != "Science" & !grade %in% c("3", "4"))
    
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_below"), starts_with("pct_below")) %>%
    # filter(abs(pct_below.x - pct_below.y) >= 0.1 | (is.na(pct_below.x) & !is.na(pct_below.y)) | (is.na(pct_below.y) & !is.na(pct_below.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_approaching"), starts_with("pct_approaching")) %>%
    # filter(abs(pct_approaching.x - pct_approaching.y) >= 0.1 | (is.na(pct_approaching.x) & !is.na(pct_approaching.y)) | (is.na(pct_approaching.y) & !is.na(pct_approaching.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_on_track"), starts_with("pct_on_track")) %>%
    # filter(abs(pct_on_track.x - pct_on_track.y) >= 0.1 | (is.na(pct_on_track.x) & !is.na(pct_on_track.y)) | (is.na(pct_on_track.y) & !is.na(pct_on_track.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_mastered"), starts_with("pct_mastered")) %>%
    # filter(abs(pct_mastered.x - pct_mastered.y) >= 0.1 | (is.na(pct_mastered.x) & !is.na(pct_mastered.y)) | (is.na(pct_mastered.y) & !is.na(pct_mastered.x))) %>%
    # select(year:subgroup, starts_with("valid_tests"), starts_with("n_on_track"), starts_with("n_mastered"), starts_with("pct_on_mastered")) %>%
    # filter(abs(pct_on_mastered.x - pct_on_mastered.y) >= 0.1 | (is.na(pct_on_mastered.x) & !is.na(pct_on_mastered.y)) | (is.na(pct_on_mastered.y) & !is.na(pct_on_mastered.x))) %>%
    filter(!is.na(subject) & subgroup != "Non-English Learners" & valid_tests.x != 0 & subject != "Science" & !grade %in% c("3", "4"))
    
  # Specific cases
  filter(ap, year == 2016 & system == 130 & school == 93 & test == "EOC" & grade == "All Grades" & 
           subject == "Algebra II" & subgroup == "Non-Economically Disadvantaged") %>% 
    select(valid_tests, starts_with("n_"))
}

# Grade 2
if(gr2 == T) {
  # Data
  setwd("N:/ORP_accountability/projects/2018_grade_2_assessment")
  
  # Figure out column names and types
  jw = read_dta("state_student_level_grade2_2018_JW_final_07112018.dta") %>% 
    transmute(state_student_id = id, system, school, grade, subject = content_area_code, enrolled, tested, valid_test, 
              performance_level, bhn_group, special_ed, economically_disadvantaged, el = ell, el_t1234 = ell_t1t4,
              enrolled_50_pct_district, enrolled_50_pct_school, 
              race = ifelse(race %in% c("Asian", "White", "Black or African American"), race, case_when(
                race == "American Indian or Alaskan Native" ~ "American Indian/Alaska Native",
                race == "Hispanic" ~ "Hispanic/Latino",
                race == "Unidentified" ~ "Unknown",
                race == "Native Hawaiian or Pacific Islander" ~ "Native Hawaiian/Pac. Islander"
              )))
  ap = read_csv("2018_grade_2_student_level_file.csv")
  
  # Checks: enrolled, tested, valid_tests, performance levels/percentages
  check = full_join(jw, ap, by = c("state_student_id", "subject")) %>% 
    select(state_student_id, subject, starts_with("race"), starts_with("bhn"), starts_with("special"), starts_with("econ"), starts_with("el")) %>% 
    filter(race.x != race.y | (is.na(race.x) & !is.na(race.y)) | (is.na(race.y) & !is.na(race.x)) | 
             bhn_group.x != bhn_group.y | (is.na(bhn_group.x) & !is.na(bhn_group.y)) | (is.na(bhn_group.y) & !is.na(bhn_group.x)) | 
             special_ed.x != special_ed.y | (is.na(special_ed.x) & !is.na(special_ed.y)) | (is.na(special_ed.y) & !is.na(special_ed.x)))# |
             #el.x != el.y | (is.na(el.x) & !is.na(el.y)) | (is.na(el.y) & !is.na(el.x)))
             #el_t1234.x != el_t1234.y | (is.na(el_t1234.x) & !is.na(el_t1234.y)) | (is.na(el_t1234.y) & !is.na(el_t1234.x)))
  
  # Specific cases
  filter(ap, year == 2016 & system == 130 & school == 93 & test == "EOC" & grade == "All Grades" & 
           subject == "Algebra II" & subgroup == "Non-Economically Disadvantaged") %>% 
    select(valid_tests, starts_with("n_"))
}