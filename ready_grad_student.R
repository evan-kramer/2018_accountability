# 2018 Ready Graduate Student Level
# Evan Kramer
# 7/12/2018

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)

dat = F
ana = T

# Data 
if(dat == T) {
  setwd("N:/ORP_accountability/data/")
  grad = read_csv("2017_graduation_rate/student_level_20170830.csv")
  act = read_dta("2017_ACT/2018_ACT_student_level_actcohorthighest_appeals2.dta")
}

# Analysis
if(ana == T) {
  grad_student = left_join(grad, )
}