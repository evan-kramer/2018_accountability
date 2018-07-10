# 2018 File Release Checks
# Evan Kramer
# 7/10/2018

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
setwd("N:/ORP_accountability/projects/2018_student_level_file")

read = F

# Data
if(read == T) {
  jw = read_dta("state_student_level_2018_JW_final_07092018.dta")
  ap = read_csv("2018_student_level_file.csv")  
}
