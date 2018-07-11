# 2018 Grade 2 Assessment
# Evan Kramer
# 7/10/2018

library(tidyverse)

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
        el = el == "Y",
        el_recently_arrived = (el_arrived_year_1 == "Y" | el_arrived_year_2 == "Y"),
        el_t1234 = el_t1234 %in% 1:4,
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
            absent == 1 | el_recently_arrived == 1 | refused_to_test == 1 | failed_attemptedness == 1 | irregular_admin == 1 ~ NA_character_,
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

write_csv(output, "N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_student_level_file.csv", na = "")
