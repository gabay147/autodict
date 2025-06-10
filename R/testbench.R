source_path = "Dictionaries/2025_Jan/Temp.csv"

fs::path_file(source_path)
fs::path_dir(source_path)


dir_vector <- stringr::str_split_1(source_path, "/")

output_name <- ""

for(i in 1:length(dir_vector)) {
  output_name <- stringr::str_c(output_name, dir_vector[i], "/")
  output_name

}


