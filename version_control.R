# Version Control
# 9/13/2018
# Evan Kramer

path = "N:/ORP_accountability/data/2018_graduation_rate/"
for(f in c("state", "district", "school")) {
  file = str_c(f, "_grad_rate.csv")
  if(file %in% list.files(path)) {
    if(!dir.exists(str_c(path, "Previous"))) {
      dir.create(str_c(path, "Previous"))
      dir.create(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))
    }
    if(!dir.exists(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))) {
      dir.create(str_c(path, "Previous/", str_replace_all(now(), "[-:]", "")))
    }
    file.rename(str_c(path, file),
                str_c(path, "Previous/", str_replace_all(now(), "[-:]", ""), "/", file))
  }
  if(f == "state") {
    write_csv(state_grad, str_c(path, file), na = "")
  } else if(f == "district") {
    write_csv(district_grad, str_c(path, file), na = "")
  } else if(f == "school") {
    write_csv(school_grad, str_c(path, file), na = "")
  }
}