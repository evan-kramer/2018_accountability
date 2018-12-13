# Version Control
# 9/13/2018
# Evan Kramer

library(tidyverse)
library(lubridate)
setwd("N:/ORP_accountability/data")

# Define state,
school = NA
district = NA
state = NA

# Define path and filename
domain = "graduation_rate"

for(f in c("state", "district", "school")) {
  file = str_c(getwd(), "/", year(today()), "_", domain, "/", f, "_", domain,".csv")
  if(file %in% list.files(str_c(getwd(), "/", year(today()), "_", domain))) {
    if(!dir.exists(str_c(str_c(getwd(), "/", year(today()), "_", domain), "Previous"))) {
      dir.create(str_c(str_c(getwd(), "/", year(today()), "_", domain), "Previous"))
      dir.create(str_c(str_c(getwd(), "/", year(today()), "_", domain), "Previous/", str_replace_all(now(), "[-:]", "")))
    }
    if(!dir.exists(str_c(str_c(getwd(), "/", year(today()), "_", domain), "Previous/", str_replace_all(now(), "[-:]", "")))) {
      dir.create(str_c(str_c(getwd(), "/", year(today()), "_", domain), "Previous/", str_replace_all(now(), "[-:]", "")))
    }
    file.rename(str_c(str_c(getwd(), "/", year(today()), "_", domain), file),
                str_c(str_c(getwd(), "/", year(today()), "_", domain), "Previous/", str_replace_all(now(), "[-:]", ""), "/", file))
  }
  if(f == "state") {
    # write_csv(state, str_c(str_c(getwd(), "/", year(today()), "_", domain), file), na = "")
  } else if(f == "district") {
    # write_csv(district, str_c(str_c(getwd(), "/", year(today()), "_", domain), file), na = "")
  } else if(f == "school") {
    # write_csv(school, str_c(str_c(getwd(), "/", year(today()), "_", domain), file), na = "")
  }
}


a = readxl::read_excel("C:/Users/CA19130/Downloads/Copy of File Tracker 2.xlsx") %>% 
  mutate(file = str_c(Location, "/", File), last_update = NA)

for(r in 1:nrow(a)) {
  a$last_update[r] = as_datetime(file.mtime(a$file[r]))
}
a$last_update = round_date(as_datetime(a$last_update))

