library(tidyverse)
library(janitor)
library(haven) 
library(lubridate)

# Switches
data = F
abst = F
summ = T
outp = F
supp = F
setwd("N:/ORP_accountability/data/2018_chronic_absenteeism")

# Data 
if(data == T) {
  instructional_days <- readxl::read_excel("Instructional_Days_SchoolFile.xls") %>%
    transmute(year = 2018, system_name = DISTRICT_NAME, system = DISTRICT_NO,
              school_name = SCHOOL_NAME, school = SCHOOL_NO, instructional_days = INSTRUCTIONAL_DAYS)
  
  demographic <- read_delim("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2017-18/Demographics_SY2017_18.txt", delim = "\t") %>%
    clean_names() %>%
    mutate(
      Hispanic = race == 4,
      Black = race == 3,
      Native = race == 1,
      HPI = race == 5,
      Asian = race == 2,
      White = race == 6
    ) %>%
    group_by(student_key) %>%
    summarise_at(c("ed", "swd", "ell", "t1t4", "Hispanic", "Black", "Native", "HPI", "Asian", "White"), max, na.rm = TRUE) %>%
    transmute(student_key, BHN = pmax(Black, Hispanic, Native), ED = ed, SWD = swd, EL = pmax(ell, t1t4),
              Hispanic, Black, Native, HPI, Asian, White)
  
  attendance <- read_dta("instructional_days_student_file.dta") %>% 
    clean_names() %>% 
    transmute(instructional_program_num = as.numeric(instructional_program_num),
              district_no = as.numeric(district_no), school_no = as.numeric(school_no), 
              grade, student_key, first_name, middle_name, last_name, begin_date, end_date, 
              isp_days = as.numeric(isp_days), cnt_total = as.numeric(cnt_total)) 
} else {
  rm(data)
}

# Absenteeism
if(abst == T) {
  absenteeism = clean_names(attendance) %>%
    filter(grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) %>%
    transmute(instructional_program_num, system = district_no, school = school_no, grade,
              student_key = as.integer(student_key), first_name, middle_name, last_name,
              begin_date, end_date, isp_days,
              count_total = if_else(is.na(cnt_total), 0, cnt_total)) %>%
    # For students with same system, school, student ID, enrollment dates, take maximum instructional program days
    # (Drops 0 records)
    group_by(system, school, student_key, grade, begin_date, end_date) %>%
    mutate(count = n(), temp = max(isp_days)) %>%
    filter(count == 1 | isp_days == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days,
    # take maximum number of absences (Drops 9 records)
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days) %>%
    mutate(count = n(), temp = max(count_total)) %>%
    filter(count == 1 | count_total == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days, absences,
    # take maximum instructional program number (Doesn't drop any records)
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total) %>%
    mutate(count = n(), temp = max(instructional_program_num)) %>%
    filter(count == 1 | instructional_program_num == temp) %>%
    # Drop duplicates on system, school, student ID, enrollment dates, instructional program days, absences, instructional program
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total, instructional_program_num) %>%
    mutate(count = 1, temp = cumsum(count)) %>%
    filter(temp == 1) %>%
    # Collapse multiple enrollments at the same school
    rename(n_absences = count_total) %>%
    group_by(system, school, grade, student_key) %>%
    summarize(first_name = first(first_name), middle_name = first(middle_name), last_name = first(last_name),
              n_absences = sum(n_absences, na.rm = T), isp_days = sum(isp_days, na.rm = T)) %>%
    ungroup() %>%
    # Merge on instructional calendar file
    inner_join(instructional_days, by = c("system", "school")) %>%
    mutate(n_students = 1,
           grade = case_when(
             grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ "K through 8th",
             grade %in% c("09", "10", "11", "12") ~ "9th through 12th"
           ),
           chronic_absence = as.integer(n_absences/isp_days >= 0.1),
           All = 1L) %>%
    left_join(demographic, by = "student_key")
} else {
  rm(abst)
}

# Summaries
if(summ == T) {
  school_CA <- tibble()
  system_CA <- tibble()
  state_CA <- tibble()
  
  for (s in c("All", "BHN", "ED", "SWD", "EL", "Black", "Hispanic", "Native", "HPI", "Asian", "White")) {
    
    # School all grades
    school_CA <- absenteeism %>%
      # Filter for relevant subgroup
      filter_(paste(s, "== 1L")) %>%
      # Drop students enrolled less than 50% of school year
      filter(isp_days/instructional_days >= 0.5) %>%
      group_by(year, system, system_name, school, school_name) %>%
      summarise(
        n_students = sum(n_students, na.rm = T),
        n_chronically_absent = sum(chronic_absence, na.rm = T),
        pct_chronically_absent = round(100 * mean(chronic_absence, na.rm = T) + 1e-9, 1)
      ) %>%
      ungroup() %>%
      mutate(subgroup = s, grade = "All Grades") %>%
      bind_rows(school_CA, .)
    
    # System by grade band
    system_CA <- absenteeism %>%
      # Filter for relevant subgroup
      filter_(paste(s, "== 1L")) %>%
      # Collapse multiple enrollments in the same district
      group_by(year, system, system_name, student_key, grade) %>%
      summarise(
        n_absences = sum(n_absences, na.rm = TRUE),
        isp_days = sum(isp_days, na.rm = TRUE),
        instructional_days = max(instructional_days)
      ) %>%
      ungroup() %>%
      # Drop students enrolled less than 50% of school year
      filter(isp_days/instructional_days >= 0.5) %>%
      mutate(
        n_students = 1,
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1)
      ) %>%
      group_by(year, system, system_name, grade) %>%
      summarise(
        n_students = sum(n_students, na.rm = T),
        n_chronically_absent = sum(chronic_absence, na.rm = T),
        pct_chronically_absent = round(100 * mean(chronic_absence, na.rm = T) + 1e-9, 1)
      ) %>%
      ungroup() %>%
      mutate(subgroup = s) %>%
      bind_rows(system_CA, .)
    
    # System all grades
    system_CA <- absenteeism %>%
      # Filter for relevant subgroup
      filter_(paste(s, "== 1L")) %>%
      # Collapse multiple enrollments in the same district
      group_by(year, system, system_name, student_key, grade) %>%
      summarise(
        n_absences = sum(n_absences, na.rm = TRUE),
        isp_days = sum(isp_days, na.rm = TRUE),
        instructional_days = max(instructional_days)
      ) %>%
      ungroup() %>%
      # Drop students enrolled less than 50% of school year
      filter(isp_days/instructional_days >= 0.5) %>%
      mutate(
        n_students = 1,
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1)
      ) %>%
      group_by(year, system, system_name) %>%
      summarise(
        n_students = sum(n_students, na.rm = T),
        n_chronically_absent = sum(chronic_absence, na.rm = T),
        pct_chronically_absent = round(100 * mean(chronic_absence, na.rm = T) + 1e-9, 1)
      ) %>%
      ungroup() %>%
      mutate(subgroup = s, grade = "All Grades") %>%
      bind_rows(system_CA, .)
    
    # State by grade band
    state_CA <- absenteeism %>%
      # Filter for relevant subgroup
      filter_(paste(s, "== 1L")) %>%
      # Add up absences and ISP days across every enrollment
      group_by(year, student_key, grade) %>%
      summarise(
        n_absences = sum(n_absences, na.rm = TRUE),
        isp_days = sum(isp_days, na.rm = TRUE),
        instructional_days = max(instructional_days)
      ) %>%
      ungroup() %>%
      filter(isp_days >= 45) %>%
      mutate(
        n_students = 1,
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1)
      ) %>%
      group_by(year, grade) %>%
      summarise(
        n_students = sum(n_students, na.rm = T),
        n_chronically_absent = sum(chronic_absence, na.rm = T),
        pct_chronically_absent = round(100 * mean(chronic_absence, na.rm = T) + 1e-9, 1)
      ) %>%
      ungroup() %>%
      mutate(subgroup = s) %>%
      bind_rows(state_CA, .)
    
    # State all grades
    state_CA <- absenteeism %>%
      # Filter for relevant subgroup
      filter_(paste(s, "== 1L")) %>%
      # Add up absences and ISP days across every enrollment
      group_by(year, student_key, grade) %>%
      summarise(
        n_absences = sum(n_absences, na.rm = TRUE),
        isp_days = sum(isp_days, na.rm = TRUE),
        instructional_days = max(instructional_days)
      ) %>%
      ungroup() %>%
      filter(isp_days >= 45) %>%
      mutate(
        n_students = 1,
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1)
      ) %>%
      group_by(year) %>%
      summarise(
        n_students = sum(n_students, na.rm = T),
        n_chronically_absent = sum(chronic_absence, na.rm = T),
        pct_chronically_absent = round(100 * mean(chronic_absence, na.rm = T) + 1e-9, 1)
      ) %>%
      ungroup() %>%
      mutate(subgroup = s, grade = "All Grades") %>%
      bind_rows(state_CA, .)
  }
  
  school_output <- school_CA %>%
    transmute(year, system, system_name, school, school_name,
              subgroup = case_when(
                subgroup == "All" ~ "All Students",
                subgroup == "BHN" ~ "Black/Hispanic/Native American",
                subgroup == "ED" ~ "Economically Disadvantaged",
                subgroup == "SWD" ~ "Students with Disabilities",
                subgroup == "EL" ~ "English Learners with Transitional 1-4",
                subgroup == "Black" ~ "Black or African American",
                subgroup == "Native" ~ "American Indian or Alaska Native",
                subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
                TRUE ~ subgroup
              ),
              grade_band = grade,
              n_students, n_chronically_absent, pct_chronically_absent) %>%
    arrange(system, school, subgroup, grade_band)
  
  system_output <- system_CA %>%
    transmute(year, system, system_name,
              subgroup = case_when(
                subgroup == "All" ~ "All Students",
                subgroup == "BHN" ~ "Black/Hispanic/Native American",
                subgroup == "ED" ~ "Economically Disadvantaged",
                subgroup == "SWD" ~ "Students with Disabilities",
                subgroup == "EL" ~ "English Learners with Transitional 1-4",
                subgroup == "Black" ~ "Black or African American",
                subgroup == "Native" ~ "American Indian or Alaska Native",
                subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
                TRUE ~ subgroup
              ),
              grade_band = grade,
              n_students, n_chronically_absent, pct_chronically_absent) %>%
    arrange(system, subgroup, grade_band)
  
  state_output <- state_CA %>%
    transmute(year, system = 0, system_name = "State of Tennessee",
              subgroup = case_when(
                subgroup == "All" ~ "All Students",
                subgroup == "BHN" ~ "Black/Hispanic/Native American",
                subgroup == "ED" ~ "Economically Disadvantaged",
                subgroup == "SWD" ~ "Students with Disabilities",
                subgroup == "EL" ~ "English Learners with Transitional 1-4",
                subgroup == "Black" ~ "Black or African American",
                subgroup == "Native" ~ "American Indian or Alaska Native",
                subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
                TRUE ~ subgroup
              ),
              grade_band = grade,
              n_students, n_chronically_absent, pct_chronically_absent) %>%
    arrange(subgroup, grade_band)
} else {
  rm(summ)
}

# State, district, school, and student output
if(outp == T) {
  # State
  level = "state"
  filedate = str_replace_all(
    round_date(file.mtime(str_c(level, "_chronic_absenteeism.csv")), "day"), 
    "-", "")
  if(!is.na(filedate)) {
    if(!dir.exists(str_c("Previous/", filedate))) {
      dir.create(str_c("Previous/", filedate))
    }
    file.copy(from = str_c(level, "_chronic_absenteeism.csv"),
              to = str_c("Previous/", filedate, "/", level, "_chronic_absenteeism.csv"),
              overwrite = T)
    file.remove(str_c(level, "_chronic_absenteeism.csv"))
  }
  write_csv(state_output, str_c(level, "_chronic_absenteeism.csv"), na = "")
  
  # District
  level = "district"
  filedate = str_replace_all(
    round_date(file.mtime(str_c(level, "_chronic_absenteeism.csv")), "day"), 
    "-", "")
  if(!is.na(filedate)) {
    if(!dir.exists(str_c("Previous/", filedate))) {
      dir.create(str_c("Previous/", filedate))
    }
    file.copy(from = str_c(level, "_chronic_absenteeism.csv"),
              to = str_c("Previous/", filedate, "/", level, "_chronic_absenteeism.csv"),
              overwrite = T)
    file.remove(str_c(level, "_chronic_absenteeism.csv"))
  }
  write_csv(system_output, str_c(level, "_chronic_absenteeism.csv"), na = "")
  
  # School
  level = "school"
  filedate = str_replace_all(
    round_date(file.mtime(str_c(level, "_chronic_absenteeism.csv")), "day"), 
    "-", "")
  if(!is.na(filedate)) {
    if(!dir.exists(str_c("Previous/", filedate))) {
      dir.create(str_c("Previous/", filedate))
    }
    file.copy(from = str_c(level, "_chronic_absenteeism.csv"),
              to = str_c("Previous/", filedate, "/", level, "_chronic_absenteeism.csv"),
              overwrite = T)
    file.remove(str_c(level, "_chronic_absenteeism.csv"))
  }
  write_csv(school_output, str_c(level, "_chronic_absenteeism.csv"), na = "")
  
  # Student
  level = "student"
  
  student_output <- absenteeism %>%
    transmute(system, system_name, school, school_name, student_id = student_key, first_name, middle_name, last_name,               n_absences, isp_days, instructional_calendar_days = instructional_days,
              absentee_rate = round(100 * n_absences/isp_days+1e-9, 1),
              Black, Hispanic, Native, HPI, Asian, White, ED, SWD, EL) %>%
    mutate_at(c("Black", "Hispanic", "Native", "HPI", "Asian", "White", "ED", "SWD", "EL"),
              funs(if_else(is.na(.), 0L, as.integer(.)))) 
  
  filedate = str_replace_all(
    round_date(file.mtime(str_c(level, "_chronic_absenteeism.csv")), "day"), 
    "-", "")
  if(!is.na(filedate)) {
    if(!dir.exists(str_c("Previous/", filedate))) {
      dir.create(str_c("Previous/", filedate))
    }
    file.copy(from = str_c(level, "_chronic_absenteeism.csv"),
              to = str_c("Previous/", filedate, "/", level, "_chronic_absenteeism.csv"),
              overwrite = T)
    file.remove(str_c(level, "_chronic_absenteeism.csv"))
  }
  write_csv(as.data.frame(student_output), str_c(level, "_chronic_absenteeism.csv"), na = "")
  
  rm(level)
} else {
  rm(outp)
}

# Suppress
if(supp == T) {
  for(s in c("state", "district", "school")) {
    if(s == "school") {
      col_types = "ddcdcccddd"; val1 = .05; val2 = .95
    } else {
      col_types = "ddcccddd"; val1 = .01; val2 = .99
    }
    
    read_csv(str_c(s, "_chronic_absenteeism.csv"), col_types = col_types) %>%
      mutate(pct_chronically_absent = ifelse(between(n_chronically_absent / n_students, val1, val2),
                                             as.character(pct_chronically_absent), "**"),
             n_chronically_absent = ifelse(between(n_chronically_absent / n_students, val1, val2),
                                           as.character(n_chronically_absent), "**")) %>% 
      mutate_at(vars(ends_with("_absent")), funs(ifelse(n_students < 10, "*", .))) %>% 
      write_csv(str_c("chr_abs_", str_to_title(s), "-Level_Suppression_2017-18.csv"), na = "") 
  }

  rm(s, col_types, val1, val2)
} else {
  rm(supp)
}