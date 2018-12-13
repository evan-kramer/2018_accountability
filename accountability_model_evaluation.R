# Accountability Results Evaluation
# Evan Kramer
# 11/9/2018

options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
setwd("N:/ORP_accountability")

a = read_csv("data/2018_final_accountability_files/2018_school_accountability_file.csv",
             col_types = "dcdccdccddddddddd")
s = read_csv("projects/2018_school_accountability/school_grading_grades.csv")
m = read_csv("projects/2018_school_accountability/school_grading_metrics.csv")
d = read_csv("data/2018_final_accountability_files/2018_district_accountability_file_JW.csv")

# How are schools growing different groups of students (e.g., skill levels, demographics)?

# Within-school indicator variation? Which are schools' highest indicators? Consistent? By pool?
select(s, system, school, pool, starts_with("score_")) %>% 
  gather(indicator, score, 4:9) %>% 
  mutate(indicator = case_when(
    indicator == "score_achievement" ~ "Achievement",
    indicator == "score_growth" ~ "Growth",
    indicator == "score_elpa" ~ "ELPA",
    indicator == "score_absenteeism" ~ "Chronically Out of School",
    indicator == "score_grad" ~ "Graduation Rate",
    indicator == "score_ready_grad" ~ "Ready Graduate"
  )) %>% 
  arrange(system, school, indicator) %>% 
  group_by(system, school) %>% 
  mutate(max = score == max(score, na.rm = T)) %>% 
  group_by(pool, indicator) %>% 
  summarize(n_max = sum(max, na.rm = T), n = sum(!is.na(score))) %>%
  ungroup() %>%
  mutate(pct = ifelse(n == 0, NA, round(100 * n_max / n, 1))) %>% 
  ggplot(aes(fill = factor(pool, levels = c("K8", "HS")), y = pct, 
             x = factor(indicator, levels = c("Achievement", "Growth", "Chronically Out of School",
                                                 "ELPA", "Graduation Rate", "Ready Graduate")))) + 
    geom_bar(stat = "identity", position = "dodge") + 
    geom_text(aes(label = pct, group = factor(pool, levels = c("K8", "HS"))),
              position = position_dodge(width = 0.9)) + 
    theme_bw() + 
    xlab("Indicator") +
    ylab("Number of Schools") + 
    scale_fill_discrete(name = "Pool") + 
    ggtitle("Highest Scoring Indicator by Pool") 
    
# Other ways to set targets?

# Hitting AMOs as a function of prior performance?
cor(m$score_achievement_abs[m$subgroup == "All Students"], m$score_achievement_target[m$subgroup == "All Students"],
    use = "complete.obs")

ggplot(filter(a, subgroup == "All Students" & !is.na(metric >= AMO_target)), 
       aes(y = metric_prior, x = metric >= AMO_target)) + 
  geom_boxplot() + 
  facet_wrap(~indicator) + 
  theme_bw() + 
  xlab("Met AMO Target") + 
  ylab("Prior Performance")

# Hitting AMOs as a function of subgroups? 
ggplot(filter(a, !is.na(metric >= AMO_target & subgroup %in% c("All Students", "Black/Hispanic/Native American",
                                                               "Economically Disadvantaged", "English Learners with Transitional 1-4",
                                                               "Students with Disabilities"))), 
       aes(y = metric_prior, x = metric >= AMO_target)) + 
  geom_boxplot() + 
  facet_wrap(~subgroup) + 
  theme_bw() + 
  xlab("Met AMO Target") + 
  ylab("Prior Performance")


# What percent of indicator scores came from absolute vs. AMO?
filter(m, subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
                          "English Learners with Transitional 1-4", "Students with Disabilities") & 
         designation_ineligible == 0) %>% 
  select(-contains("weight"), -subgroup_average) %>%
  gather(indicator, score, 6:19) %>% 
  mutate(
    score_type = case_when(
      indicator %in% c("score_absenteeism_abs", "score_achievement_abs",
                       "score_grad_abs", "score_ready_grad_abs") ~ "Absolute",
      indicator %in% c("score_absenteeism_reduction", "score_achievement_target", 
                       "score_grad_target", "score_ready_grad_target") ~ "AMO",
      indicator %in% c("score_absenteeism", "score_achievement", "score_elpa",
                       "score_grad", "score_growth", "score_ready_grad") ~ "Overall"
    ),
    indicator = case_when(
      indicator %in% c("score_achievement", "score_achievement_abs", "score_achievement_target") ~ "Achievement",
      indicator %in% c("score_growth") ~ "Growth",
      indicator %in% c("score_absenteeism", "score_absenteeism_abs", "score_absenteeism_reduction") ~ "Chronically Out of School",
      indicator %in% c("score_elpa") ~ "ELPA",
      indicator %in% c("score_grad", "score_grad_abs", "score_grad_target") ~ "Graduation Rate",
      indicator %in% c("score_grady_grad", "score_ready_grad_abs", "score_ready_grad_target") ~ "Ready Graduate"
    )
  ) %>% 
  arrange(system, school, subgroup, indicator, score_type) %>% 
  spread(score_type, score) %>% 
  mutate(score_type = case_when(
    Absolute == AMO & AMO == Overall ~ "Both", 
    Absolute > AMO & Absolute == Overall ~ "Absolute",
    AMO > Absolute & AMO == Overall ~ "AMO",
    is.na(Absolute) & !is.na(Overall) ~ "AMO, Missing Absolute",
    is.na(AMO) & !is.na(Overall) ~ "Absolute, Missing AMO",
    is.na(AMO) & is.na(Absolute) & is.na(Overall) ~ "Missing All"
  )) %>% 
  group_by(indicator, score_type) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  filter(!indicator %in% c("ELPA", "Growth") & !is.na(score_type) & !is.na(indicator) & score_type != "Missing All") %>% 
  ggplot(aes(y = n, x = indicator, fill = score_type)) + 
    geom_bar(stat = "identity", position = "stack") + 
    geom_label(aes(label = n), position = "stack") + 
    theme_bw()

# How much variation is explained by each indicator
tibble(
  indicator = c("score_achievement", "score_growth", "score_absenteeism", "score_elpa", 
                "score_grad", "score_ready_grad"),
  R2 = c(
    summary(lm(data = filter(s, designation_ineligible == 0), final_average ~ score_achievement))$`r.squared`,
    summary(lm(data = filter(s, designation_ineligible == 0), final_average ~ score_growth))$`r.squared`,
    summary(lm(data = filter(s, designation_ineligible == 0), final_average ~ score_absenteeism))$`r.squared`,
    summary(lm(data = filter(s, designation_ineligible == 0), final_average ~ score_elpa))$`r.squared`,
    summary(lm(data = filter(s, designation_ineligible == 0), final_average ~ score_grad))$`r.squared`,
    summary(lm(data = filter(s, designation_ineligible == 0), final_average ~ score_ready_grad))$`r.squared`
    )
  )
 
# How are indicators correlated with one another?
round(cor(select(s, starts_with("score_")), use = "complete.obs"), 3) %>% 
  as.tibble() %>% 
  mutate(indicator = case_when(
    row_number() == 1 ~ "score_achievement",
    row_number() == 2 ~ "score_growth",
    row_number() == 3 ~ "score_absenteeism",
    row_number() == 4 ~ "score_elpa",
    row_number() == 5 ~ "score_grad",
    row_number() == 6 ~ "score_ready_grad"
  )) %>% 
  gather(key = "indicator2", value = "cor", 1:6)

# What are the salient features of ATSI schools? 



# Were 3-5 or 6-8 success rates higher?
filter(d, subgroup == "All Students" & indicator == "Success Rate" & 
         grade %in% c("3rd through 5th", "6th through 8th") & pathway == "AMO/Absolute") %>% 
  mutate(improvement = metric - metric_prior) %>% 
  # ggplot(aes(improvement, fill = grade)) + 
  ggplot(aes(metric, fill = grade)) +
  geom_histogram(stat = "density") + 
  theme_bw() + 
  # xlab("Success Rate Change") + 
  xlab("Success Rate") +
  ylab("Density") + 
  scale_fill_discrete(name = "Grade")

# TVAAS discrepancies
b = readxl::read_excel("data/2018_tvaas/School Composite Index.xlsx") %>% 
  transmute(system = as.numeric(`District Number`), school = as.numeric(`School Number`),
            system_name = `District Name`, school_name = `School Name`, 
            index = `School-Wide: Composite`) %>% 
  full_join(readxl::read_excel("data/2018_tvaas/School Composite Level.xlsx") %>% 
              transmute(system = as.numeric(`District Number`), school = as.numeric(`School Number`),
                        level = `School-Wide: Composite`),
            by = c("system", "school"))

group_by(b, level) %>% 
  summarize(max = max(index, na.rm = T), min = min(index, na.rm = T))
