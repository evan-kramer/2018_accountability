# 2018 Grade 2 Assessment
# Evan Kramer
# 7/10/2018

library(tidyverse)

# Switches
stu = T
sch = T
dis = T

# Student 
if(stu == T) {
  cdf <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_grade_2_cdf.csv") %>%
    mutate(
      absent = reason_not_tested == 1,
      medically_exempt = reason_not_tested == 4,
      residential_facility = reason_not_tested == 5,
      did_not_submit = reason_not_tested == 7,
      breach_adult = ri_status == 1,
      breach_student = ri_status == 2,
      irregular_admin = ri_status == 3,
      incorrect_grade_subject = ri_status == 4,
      refused_to_test = ri_status == 5,
      failed_attemptedness = ri_status == 6,
      original_subject = case_when(
        content_area_code == "ENG" ~ "ELA",
        content_area_code == "MAT" ~ "Math"
      )
    )
  
  student_level <- cdf %>%
    # Replace EL variables with Chris's file
    mutate_at(vars(system, school), funs(as.integer(.))) %>%
    select(-el, -el_t1234) %>%
    left_join(read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/EL status and variables 2018 Grade 2.csv") %>% 
                transmute(unique_student_id = `Student Key`, 
                          el = `IS EL` == 1,
                          el_recently_arrived = `Recently Arrived Year 1` == 1 | `Recently Arrived Year 2` == 1,
                          el_t1234 = between(`T1T2  (T1T4)`, 0, 3)), by = "unique_student_id") %>%
    mutate_at(vars(el, starts_with("el_")), funs(ifelse(is.na(.), 0, .))) %>%
    mutate(
      enrolled = 1,
      valid_test = NA_integer_,
      race = case_when(
        hispanic == "Y" ~ "Hispanic/Latino",
        black == "Y" ~ "Black or African American",
        native_american == "Y" ~ "American Indian/Alaska Native",
        hawaiian_pi == "Y" ~ "Native Hawaiian/Pac. Islander",
        asian == "Y" ~ "Asian",
        white == "Y" ~ "White",
        TRUE ~ "Unknown"
      ),
      bhn_group = race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native"),
      economically_disadvantaged = economically_disadvantaged == "Y",
      special_ed = special_ed == "Y",
      functionally_delayed = functionally_delayed == "Y",
      homebound = homebound == "Y",
      original_performance_level = performance_level,
      subject = original_subject
    ) %>%
    select(system, system_name, school, school_name, original_subject, subject, 
           original_performance_level, performance_level, scale_score,
           enrolled, valid_test, state_student_id = unique_student_id,
           last_name, first_name, grade, race, bhn_group, functionally_delayed, special_ed,
           economically_disadvantaged, el, el_t1234, el_recently_arrived,
           enrolled_50_pct_district, enrolled_50_pct_school, homebound, absent,
           breach_adult, breach_student, irregular_admin, incorrect_grade_subject,
           refused_to_test, failed_attemptedness, residential_facility, did_not_submit,
           ri_status, medically_exempt, teacher_of_record_tln) %>%
    mutate_at(c("system", "school", "state_student_id", "grade", "bhn_group", "functionally_delayed", "special_ed",
                "economically_disadvantaged", "el", "el_t1234", "el_recently_arrived", "homebound", "absent",
                "breach_adult", "breach_student", "irregular_admin", "incorrect_grade_subject", 
                "refused_to_test", "failed_attemptedness", "residential_facility"),
              as.numeric) %>%
    # Grade 2 are all considered enrolled
    # Grade 2 are considered tested if performance level is not missing
    mutate(
      tested = if_else(!is.na(performance_level), 1L, 0L),
      valid_test = if_else(!is.na(performance_level), 1L, 0L),
      performance_level = case_when(
        absent == 1 | refused_to_test == 1 | failed_attemptedness == 1 | irregular_admin == 1 ~ NA_character_,
        TRUE ~ performance_level
      )
    )
  
  output <- student_level %>%
    left_join(transmute(read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/cte_alt_adult_schools.csv"),
                        system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER), 
                        cte_alt_adult = 1L), by = c("system", "school")) %>%
    filter(!is.na(state_student_id) & is.na(cte_alt_adult)) %>%
    select(system, system_name, school, school_name, original_subject, subject,
           original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
           state_student_id, last_name, first_name, grade, race, bhn_group, teacher_of_record_tln,
           functionally_delayed, special_ed, economically_disadvantaged, el, el_t1234, el_recently_arrived,
           enrolled_50_pct_district, enrolled_50_pct_school, homebound, absent, refused_to_test, residential_facility) %>%
    mutate(performance_level = if_else(performance_level == "On track", "On Track", performance_level)) %>%
    arrange(system, school, state_student_id)
  
  # write_csv(output, "N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_student_level_file.csv", na = "")
}

# school
if(sch == T) {
  student_level <- read_csv("N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_student_level_file.csv") %>%
    filter(residential_facility == 0, homebound == 0) %>%
    # Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1234 = el_t1234) %>%
    mutate(year = 2018,
           # grade = if_else(is.na(grade), 0L, grade),
           n_below = if_else(performance_level %in% c("Below", "Below Basic"), 1L, NA_integer_),
           n_approaching = if_else(performance_level %in% c("Approaching", "Basic"), 1L, NA_integer_),
           n_on_track = if_else(performance_level %in% c("On Track", "Proficient"), 1L, NA_integer_),
           n_mastered = if_else(performance_level %in% c("Mastered", "Advanced"), 1L, NA_integer_),
           All = 1L,
           Asian = race == "Asian",
           Black = race == "Black or African American",
           Hispanic = race == "Hispanic/Latino",
           Hawaiian = race == "Native Hawaiian/Pac. Islander",
           Native = race == "American Indian/Alaska Native",
           White = race == "White",
           T1234 = between(EL_T1234, 1, 4) & EL != 1,
           EL_T1234 = EL == T | T1234 == T,
           Non_BHN = BHN == 0L,
           Non_ED = ED == 0L,
           Non_SWD = SWD == 0L,
           Non_EL = EL == 0,
           Non_EL1234 = EL == 0 & EL_T1234 == 0,
           Super = (BHN == 1L | ED == 1L | SWD == 1L | EL_T1234 == 1L)) %>%
    mutate_at(c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
                "EL", "T1234", "EL_T1234", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Non_EL1234", "Super"), as.integer) 
  
  collapse <- tibble()
  
  # Collapse proficiency by subject and subgroup
  for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
              "EL", "T1234", "EL_T1234", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Non_EL1234", "Super")) {
    
    collapse <- student_level %>%
      filter_(paste(s, "== 1L")) %>%
      group_by(year, system, school, original_subject) %>%
      summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
      mutate(subgroup = s) %>%
      bind_rows(collapse, .)
   
  }
  
  school_assessment <- collapse %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(pct_approaching = if_else(valid_tests != 0, round(100 * n_approaching/valid_tests+1e-9, 1), NA_real_),
           pct_on_track = if_else(valid_tests != 0, round(100 * n_on_track/valid_tests+1e-9, 1), NA_real_),
           pct_mastered = if_else(valid_tests != 0, round(100 * n_mastered/valid_tests+1e-9, 1), NA_real_),
           pct_below = if_else(valid_tests != 0, round(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
           pct_on_mastered = if_else(valid_tests != 0, round(100 * (n_on_track + n_mastered)/valid_tests+1e-9, 1), NA_real_),
           # Fix % B/A/O if there are no n_B/A/O,
           pct_approaching = if_else(pct_below != 0 & n_below == 0, 100 - pct_on_track - pct_mastered, pct_approaching),
           pct_below = if_else(pct_below != 0 & n_below == 0, 0, pct_below),
           pct_on_track = if_else(pct_approaching != 0 & n_approaching == 0, 100 - pct_mastered, pct_on_track),
           pct_approaching = if_else(pct_approaching != 0 & n_approaching == 0, 0, pct_approaching),
           subgroup = case_when(
             subgroup == "All" ~ "All Students",
             subgroup == "Black" ~ "Black or African American",
             subgroup == "BHN" ~ "Black/Hispanic/Native American",
             subgroup == "ED" ~ "Economically Disadvantaged",
             subgroup == "EL" ~ "English Learners",
             subgroup == "T1234" ~ "English Learner Transitional 1-4",
             subgroup == "EL_T1234" ~ "English Learners with Transitional 1-4",
             subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
             subgroup == "Native" ~ "American Indian or Alaska Native",
             subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
             subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
             subgroup == "Non_EL" ~ "Non-English Learners",
             subgroup == "Non_EL1234" ~ "Non-English Learners/Transitional 1-4",
             subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
             subgroup == "Super" ~ "Super Subgroup",
             subgroup == "SWD" ~ "Students with Disabilities",
             TRUE ~ subgroup
           )
    ) %>%
    select(year, system, school, subject, subgroup,
           enrolled, tested, valid_tests, n_below, n_approaching, n_on_track, n_mastered, 
           pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>% 
    bind_rows(read_dta("N:/ORP_accountability/projects/2017_grade_2_assessment/school_level_2017_JW_final_10242017.dta") %>% 
                select(year, system, school, subject, subgroup, enrolled:valid_tests, 
                       n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, 
                       n_mastered = n_mastered_adv, pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, 
                       pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv, pct_on_mastered = pct_ontrack_prof_adv) %>% 
                mutate(subgroup = case_when(
                  subgroup == "Native American" ~ "American Indian or Alaska Native",
                  subgroup == "English Language Learners" ~ "English Learners",
                  subgroup == "English Language Learners with T1/T2" ~ "English Learners with Transitional 1-4",
                  subgroup == "Non-English Language Learners" ~ "Non-English Learners",
                  subgroup == "Non-English Language Learners with T1/T2" ~ "Non-English Learners/Transitional 1-4",
                  subgroup == "Black" ~ "Black or African American",
                  subgroup == "Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
                  T ~ subgroup
                ))) %>% 
    left_join(readxl::read_excel("N:/ORP_accountability/data/2018_final_accountability_files/2017-18_E EDFacts School Master FIle_5-3-18.xls", sheet = 2) %>%
                janitor::clean_names() %>%
                transmute(
                  system = as.integer(dg_4_lea_id_state), system_name = extra_item_lea_name,
                  school = as.integer(dg_5_school_id_state), school_name = dg_7_school_name
                ) %>%
                distinct(),
              by = c("system", "school")) %>% 
    select(year, system, system_name, school, school_name, everything()) %>% 
    arrange(system, school, desc(year), subgroup, subject) 
    
  write_csv(school_assessment, "N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_school_level_file.csv", na = "")
}

# District
if(dis == T) {
  student_level <- read_csv("N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_student_level_file.csv") %>%
    filter(residential_facility == 0, homebound == 0) %>%
    # Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1234 = el_t1234) %>%
    mutate(year = 2018,
           # grade = if_else(is.na(grade), 0L, grade),
           n_below = if_else(performance_level %in% c("Below", "Below Basic"), 1L, NA_integer_),
           n_approaching = if_else(performance_level %in% c("Approaching", "Basic"), 1L, NA_integer_),
           n_on_track = if_else(performance_level %in% c("On Track", "Proficient"), 1L, NA_integer_),
           n_mastered = if_else(performance_level %in% c("Mastered", "Advanced"), 1L, NA_integer_),
           All = 1L,
           Asian = race == "Asian",
           Black = race == "Black or African American",
           Hispanic = race == "Hispanic/Latino",
           Hawaiian = race == "Native Hawaiian/Pac. Islander",
           Native = race == "American Indian/Alaska Native",
           White = race == "White",
           T1234 = between(EL_T1234, 1, 4) & EL != 1,
           EL_T1234 = EL == T | T1234 == T,
           Non_BHN = BHN == 0L,
           Non_ED = ED == 0L,
           Non_SWD = SWD == 0L,
           Non_EL = EL == 0,
           Non_EL1234 = EL == 0 & EL_T1234 == 0,
           Super = (BHN == 1L | ED == 1L | SWD == 1L | EL_T1234 == 1L)) %>%
    mutate_at(c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
                "EL", "T1234", "EL_T1234", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Non_EL1234", "Super"), as.integer) 
  
  collapse <- tibble()
  
  # Collapse proficiency by subject and subgroup
  for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
              "EL", "T1234", "EL_T1234", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Non_EL1234", "Super")) {
    
    collapse <- student_level %>%
      filter_(paste(s, "== 1L")) %>%
      group_by(year, system, original_subject) %>%
      summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
      mutate(subgroup = s) %>%
      bind_rows(collapse, .)
    
  }
  
  district_assessment <- collapse %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(pct_approaching = if_else(valid_tests != 0, round(100 * n_approaching/valid_tests+1e-9, 1), NA_real_),
           pct_on_track = if_else(valid_tests != 0, round(100 * n_on_track/valid_tests+1e-9, 1), NA_real_),
           pct_mastered = if_else(valid_tests != 0, round(100 * n_mastered/valid_tests+1e-9, 1), NA_real_),
           pct_below = if_else(valid_tests != 0, round(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
           pct_on_mastered = if_else(valid_tests != 0, round(100 * (n_on_track + n_mastered)/valid_tests+1e-9, 1), NA_real_),
           # Fix % B/A/O if there are no n_B/A/O,
           pct_approaching = if_else(pct_below != 0 & n_below == 0, 100 - pct_on_track - pct_mastered, pct_approaching),
           pct_below = if_else(pct_below != 0 & n_below == 0, 0, pct_below),
           pct_on_track = if_else(pct_approaching != 0 & n_approaching == 0, 100 - pct_mastered, pct_on_track),
           pct_approaching = if_else(pct_approaching != 0 & n_approaching == 0, 0, pct_approaching),
           subgroup = case_when(
             subgroup == "All" ~ "All Students",
             subgroup == "Black" ~ "Black or African American",
             subgroup == "BHN" ~ "Black/Hispanic/Native American",
             subgroup == "ED" ~ "Economically Disadvantaged",
             subgroup == "EL" ~ "English Learners",
             subgroup == "T1234" ~ "English Learner Transitional 1-4",
             subgroup == "EL_T1234" ~ "English Learners with Transitional 1-4",
             subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
             subgroup == "Native" ~ "American Indian or Alaska Native",
             subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
             subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
             subgroup == "Non_EL" ~ "Non-English Learners",
             subgroup == "Non_EL1234" ~ "Non-English Learners/Transitional 1-4",
             subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
             subgroup == "Super" ~ "Super Subgroup",
             subgroup == "SWD" ~ "Students with Disabilities",
             TRUE ~ subgroup
           )
    ) %>%
    bind_rows(read_dta("N:/ORP_accountability/projects/2017_grade_2_assessment/system_level_2017_JW_final_10242017.dta") %>% 
                select(year, system, subject, subgroup, enrolled:valid_tests, 
                       n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, 
                       n_mastered = n_mastered_adv, pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, 
                       pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv, pct_on_mastered = pct_ontrack_prof_adv) %>% 
                mutate(subgroup = case_when(
                  subgroup == "Native American" ~ "American Indian or Alaska Native",
                  subgroup == "English Language Learners" ~ "English Learners",
                  subgroup == "English Language Learners with T1/T2" ~ "English Learners with Transitional 1-4",
                  subgroup == "Non-English Language Learners" ~ "Non-English Learners",
                  subgroup == "Non-English Language Learners with T1/T2" ~ "Non-English Learners/Transitional 1-4",
                  subgroup == "Black" ~ "Black or African American",
                  subgroup == "Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
                  T ~ subgroup
                ))) %>% 
    left_join(readxl::read_excel("N:/ORP_accountability/data/2018_final_accountability_files/2017-18_E EDFacts School Master FIle_5-3-18.xls", sheet = 2) %>%
                janitor::clean_names() %>%
                transmute(
                  system = as.integer(dg_4_lea_id_state), system_name = extra_item_lea_name
                ) %>%
                distinct(),
              by = "system") %>% 
    select(year, system, system_name, subject, subgroup, everything()) %>% 
    arrange(system, desc(year), subgroup, subject) 
  
  # write_csv(district_assessment, "N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_district_level_file.csv", na = "")
}
