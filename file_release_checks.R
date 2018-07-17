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
gr2 = F 
elp = F
rdg = F
rel = F
abs = F

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
  # jw = read_dta("state_student_level_grade2_2018_JW_final_07112018.dta") %>%
  #   transmute(state_student_id = id, system, school, grade, subject = content_area_code, enrolled, tested, valid_test,
  #             bhn_group, special_ed, economically_disadvantaged, el = ell, el_t1234 = ell_t1t4,
  #             enrolled_50_pct_district, enrolled_50_pct_school,
  #             performance_level = case_when(
  #               performance_level == "1. Below" ~ "Below",
  #               performance_level == "2. Approaching" ~ "Approaching",
  #               performance_level == "3. On Track" ~ "On Track",
  #               performance_level == "4. Mastered" ~ "Mastered",
  #               TRUE ~ performance_level
  #             ),
  #             race = ifelse(race %in% c("Asian", "White", "Black or African American"), race, case_when(
  #               race == "American Indian or Alaskan Native" ~ "American Indian/Alaska Native",
  #               race == "Hispanic" ~ "Hispanic/Latino",
  #               race == "Unidentified" ~ "Unknown",
  #               race == "Native Hawaiian or Pacific Islander" ~ "Native Hawaiian/Pac. Islander"
  #             )))
  # ap = read_csv("2018_grade_2_student_level_file.csv")

  jw = read_dta("system_level_2018_JW_final_07112018.dta") %>%
    mutate_at(vars(year, system, enrolled:n_mastered_adv), funs(as.integer(.))) %>%
    rename(n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
           pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof_adv,
           pct_mastered = pct_mastered_adv, pct_on_mastered = pct_ontrack_prof) %>%
    mutate(subgroup = case_when(
      subgroup == "Black" ~ "Black or African American",
      subgroup == "English Language Learners" ~ "English Learners",
      subgroup == "English Language Learners with T1/T2/T3/T4" ~ "English Learners with Transitional 1-4",
      subgroup == "Former English Language Learners T1/T2/T3/T4" ~ "English Learner Transitional 1-4",
      subgroup == "Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
      subgroup == "Native American" ~ "American Indian or Alaska Native",
      subgroup == "Non-English Language Learners" ~ "Non-English Learners",
      subgroup == "Non-English Language Learners with T1/T2/T3/T4" ~ "Non-English Learners/Transitional 1-4",
      TRUE ~ subgroup
    ))
  ap = read_csv("2018_grade_2_district_level_file.csv")

  # Checks: enrolled, tested, valid_tests, performance levels/percentages
  # check = full_join(jw, ap, by = c("state_student_id", "subject")) %>% 
  # check = full_join(jw, ap, by = c("year", "system", "school", "subject", "subgroup")) %>%
  check = full_join(jw, ap, by = c("year", "system", "subject", "subgroup")) %>%
    
    filter(
      # race.x != race.y | (is.na(race.x) & !is.na(race.y)) | (is.na(race.y) & !is.na(race.x))
      # bhn_group.x != bhn_group.y | (is.na(bhn_group.x) & !is.na(bhn_group.y)) | (is.na(bhn_group.y) & !is.na(bhn_group.x))
      # special_ed.x != special_ed.y | (is.na(special_ed.x) & !is.na(special_ed.y)) | (is.na(special_ed.y) & !is.na(special_ed.x))
      # el.x != el.y | (is.na(el.x) & !is.na(el.y)) | (is.na(el.y) & !is.na(el.x)))
      # el_t1234.x != el_t1234.y | (is.na(el_t1234.x) & !is.na(el_t1234.y)) | (is.na(el_t1234.y) & !is.na(el_t1234.x))
      # enrolled.x != enrolled.y | (is.na(enrolled.x) & !is.na(enrolled.y)) | (is.na(enrolled.y) & !is.na(enrolled.x))
      # valid_test.x != valid_test.y | (is.na(valid_test.x) & !is.na(valid_test.y)) | (is.na(valid_test.y) & !is.na(valid_test.x))
      # performance_level.x != performance_level.y | (is.na(performance_level.x) & !is.na(performance_level.y)) | (is.na(performance_level.y) & !is.na(performance_level.x))
      # enrolled.x != enrolled.y | (is.na(enrolled.x) & !is.na(enrolled.y)) | (is.na(enrolled.y) & !is.na(enrolled.x))
      # n_on_track.x != n_on_track.y | (is.na(n_on_track.x) & !is.na(n_on_track.y)) | (is.na(n_on_track.y) & !is.na(n_on_track.x))
      # pct_on_mastered.x != pct_on_mastered.y | (is.na(pct_on_mastered.x) & !is.na(pct_on_mastered.y)) | (is.na(pct_on_mastered.y) & !is.na(pct_on_mastered.x))
    ) %>% 
    select(year, system, subject, subgroup, contains("valid_tests"), starts_with("pct_"))
  
}

# ELPA
if(elp == T) {
  setwd("N:/ORP_accountability/data/2018_ELPA")
  
  # Student
  # jw = read_dta("WIDA_student_level2018_wlagscores_07062018.dta") %>% 
  #   select(student_id = unique_student_id, system, school, ends_with("performancelevel"), starts_with("performancelevel"),
  #          valid_tests, bhn, ed, swd, el = ell, hispanic = Hispanic, black = Black, native = raceamericanindianalaskannative,
  #          hawaiian_pi = racepacificislanderhawaiian, asian = Asian, white = White) %>% 
  #   mutate_at(vars(native, hawaiian_pi), funs(as.numeric(. == "Y"))) %>% 
  #   mutate_at(vars(ed, bhn, swd), funs(ifelse(is.na(.), 0, .)))
  # ap = read_csv("wida_growth_standard_student_level.csv", col_types = "ddcdccccdddddddddddddddddddddddddddddddddd") %>% 
  #   mutate_at(vars(bhn), funs(ifelse(is.na(.), 0, .)))
  # 
  # # Checks: performance
  # check = full_join(rename(jw, prof_composite = performancelevelcomposite, prof_literacy = literacyperformancelevel),
  #                   select(ap, student_id, system, school, bhn, ed, el, swd, black, hispanic, native, hawaiian_pi, asian, white, starts_with("prof_")),
  #                   by = "student_id") %>% 
  #   filter(
  #     # system.x != system.y | (is.na(system.x) & !is.na(system.y)) | (is.na(system.y) & !is.na(system.x))
  #     # school.x != school.y | (is.na(school.x) & !is.na(school.y)) | (is.na(school.y) & !is.na(school.x))
  #     # bhn.x != bhn.y | (is.na(bhn.x) & !is.na(bhn.y)) | (is.na(bhn.y) & !is.na(bhn.x))
  #     # ed.x != ed.y | (is.na(ed.x) & !is.na(ed.y)) | (is.na(ed.y) & !is.na(ed.x))
  #     # el.x != el.y | (is.na(el.x) & !is.na(el.y)) | (is.na(el.y) & !is.na(el.x))
  #     # swd.x != swd.y | (is.na(swd.x) & !is.na(swd.y)) | (is.na(swd.y) & !is.na(swd.x))
  #     # prof_composite.x != prof_composite.y | (is.na(prof_composite.x) & !is.na(prof_composite.y)) | (is.na(prof_composite.y) & !is.na(prof_composite.x))
  #     prof_literacy.x != prof_literacy.y | (is.na(prof_literacy.x) & !is.na(prof_literacy.y)) | (is.na(prof_literacy.y) & !is.na(prof_literacy.x))
  #   )

  # School
  # jw = read_dta("school_level_elpa_JW_07062018.dta") %>% 
  #     transmute(system = as.integer(system), school = as.integer(school),
  #               subgroup = ifelse(subgroup  == "English Language Learners", "English Learners", subgroup),
  #               subgroup = ifelse(subgroup == "Native Hawaiian or Pacific Islander", "Native Hawaiian or Other Pacific Islander", subgroup),
  #               exit_denom = valid_tests,
  #               n_exit = met_exit_criteriaNEW,
  #               pct_exit = pct_met_exit_criteriaNEW,
  #               growth_standard_denom = n_validtests_growth,
  #               pct_met_growth_standard,
  #               literacy_average = literacy_avg,
  #               composite_average = composite_avg)
  # ap = read_csv("wida_growth_standard_school.csv")
  # check = full_join(jw, ap, by = c("system", "school", "subgroup")) %>% 
  #   filter(composite_average.x != composite_average.y | (is.na(composite_average.x) & !is.na(composite_average.y)) | (is.na(composite_average.y) & !is.na(composite_average.x))) %>% 
  #   select(system, school, subgroup, starts_with("composite_average"))
  
  # District
  # jw = read_dta("system_level_elpa_JW_07062018.dta") %>% 
  #   transmute(system = as.integer(system), 
  #             subgroup = ifelse(subgroup  == "English Language Learners", "English Learners", subgroup),
  #             subgroup = ifelse(subgroup == "Native Hawaiian or Pacific Islander", "Native Hawaiian or Other Pacific Islander", subgroup),
  #             exit_denom = valid_tests,
  #             n_exit = met_exit_criteriaNEW,
  #             pct_exit = pct_met_exit_criteriaNEW,
  #             growth_standard_denom = n_validtests_growth,
  #             pct_met_growth_standard,
  #             literacy_average = literacy_avg,
  #             composite_average = composite_avg)
  # ap = read_csv("wida_growth_standard_district.csv")
  # check = full_join(jw, ap, by = c("system", "subgroup")) %>% 
  #   # filter(composite_average.x != composite_average.y | (is.na(composite_average.x) & !is.na(composite_average.y)) | (is.na(composite_average.y) & !is.na(composite_average.x))) %>% 
  #   filter(literacy_average.x != literacy_average.y | (is.na(literacy_average.x) & !is.na(literacy_average.y)) | (is.na(literacy_average.y) & !is.na(literacy_average.x))) %>%
  #   select(system:subgroup, starts_with("literacy_average"), starts_with("composite_average"))
  
  # Specific cases
}

# Ready graduate
if(rdg == T) {
  setwd("N:/ORP_accountability/data/2018_final_accountability_files")
  
  # Student
  # jw = read_dta("ready_grad_student2018_JW.dta") %>% 
  #   mutate_at(vars(student_key), funs(as.integer(.))) %>% 
  #   mutate(ready_grad = n_21_orhigher)
  # ap = ready_grad_student
  
  # School
  # jw = read_dta("ready_grad_school2018_JW.dta") %>% 
  #   transmute(system = as.integer(system), school = as.integer(school), grad_cohort = as.integer(grad_cohort),
  #             ready_grad = as.integer(n_21_orhigher), subgroup, pct_ready_grad)
  # ap = read_csv("school_ready_grad.csv")
  # check = full_join(jw, ap, by = c("system", "school", "subgroup")) %>% 
  #   filter(pct_ready_grad.x != pct_ready_grad.y | (is.na(pct_ready_grad.x) & !is.na(pct_ready_grad.y)) | (is.na(pct_ready_grad.y) & !is.na(pct_ready_grad.x))) 
  
  # District
  # jw = read_dta("ready_grad_system2018_JW.dta") %>% 
    # transmute(system = as.integer(system), grad_cohort = as.integer(grad_cohort),
              # ready_grad = as.integer(n_21_orhigher), subgroup, pct_ready_grad)
  # ap = read_csv("district_ready_grad.csv")
  # check = full_join(jw, ap, by = c("system", "subgroup")) %>% 
    # filter(pct_ready_grad.x != pct_ready_grad.y | (is.na(pct_ready_grad.x) & !is.na(pct_ready_grad.y)) | (is.na(pct_ready_grad.y) & !is.na(pct_ready_grad.x))) 
}

# State release
if(rel == T) {
  setwd("N:/ORP_accountability/data/2018_final_accountability_files/")
  jw = read_csv("state_release_file2018.csv")
  ap = read_csv("state_release_file.csv")
  check = full_join(jw, ap, by = c("year", "system", "subject", "grade", "subgroup")) %>% 
    filter(abs(pct_on_mastered.x - pct_on_mastered.y) > 0.1 | (is.na(pct_on_mastered.x) & !is.na(pct_on_mastered.y)) | (is.na(pct_on_mastered.y) & !is.na(pct_on_mastered.x))) %>%
    select(year:subgroup, starts_with("valid_tests"), starts_with("n_"), starts_with("pct_"))
  
  jw = read_csv("state_release_assessmentfile2018_suppressed.csv")
  ap = read_csv("state_assessment_file_suppressed.csv")
  # check = full_join(jw, ap, by = c("year", "system", "subject", "grade", "subgroup")) %>% 
  #   filter(pct_on_mastered.x != pct_on_mastered.y | (is.na(pct_on_mastered.x) & !is.na(pct_on_mastered.y)) | (is.na(pct_on_mastered.y) & !is.na(pct_on_mastered.x))) %>%
  #   select(year:subgroup, starts_with("valid_tests"), starts_with("n_"), starts_with("pct_"))
  
  jw = mutate_at(read_dta("system_release_file2018_JW.dta"), vars(year, system, starts_with("n_")), funs(as.numeric(.)))
  ap = read_csv("district_release_file.csv")
  check = full_join(jw, ap, by = c("year", "system", "subject", "grade", "subgroup")) %>% 
    mutate_at(vars(starts_with("n_"), starts_with("pct_")), funs(as.character(.))) %>%
    # filter(pct_below.x != pct_below.y | (is.na(pct_below.x) & !is.na(pct_below.y)) | (is.na(pct_below.y) & !is.na(pct_below.x))) %>%
    filter(pct_approaching.x != pct_approaching.y | (is.na(pct_approaching.x) & !is.na(pct_approaching.y)) | (is.na(pct_approaching.y) & !is.na(pct_approaching.x))) %>%
    select(year:subgroup, starts_with("valid_tests"), starts_with("n_below"), starts_with("n_approaching"),
           starts_with("n_on_track"), starts_with("n_mastered"), starts_with("pct_below")) %>% 
    filter(valid_tests.x != 0)
}

# Missing absenteeism files
if(abs == T) { 
  a = data.frame(fl = list.files("N:/ORP_accountability/projects/NCLBAppeals/Accountability Web Files")) %>% 
    filter(str_detect(fl, "Absenteeism") == T) %>% 
    mutate(system = as.integer(str_replace_all(str_sub(fl, 1, 3), "_", ""))) %>% 
    arrange(fl) %>% 
    group_by(system) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    filter(n < 3)
}
