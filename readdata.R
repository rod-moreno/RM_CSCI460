

#Reading in all of the saved matches and PUUIDs from scraping
file_list <- list.files(path = "data", pattern = "\\.rds$", full.names = TRUE)

for (file in file_list) {
  # Add tools:: right here 
  obj_name <- tools::file_path_sans_ext(basename(file))
  
  assign(obj_name, readRDS(file))
}
rm(file_list, file, obj_name)

#Reading in all of the saved matches and PUUIDs from scraping
file_list <- list.files(path = "data/data2", pattern = "\\.rds$", full.names = TRUE)

for (file in file_list) {
  # Add tools:: right here 
  obj_name <- tools::file_path_sans_ext(basename(file))
  
  assign(obj_name, readRDS(file))
}
rm(file_list, file, obj_name)
