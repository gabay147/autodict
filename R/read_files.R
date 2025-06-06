#' Read files from a directory recursively
#'
#' @param path The folder path to iterate through recursively. Defaults to the
#'  current working directory
#' @param file_types a list of file types to search for. Defaults to "csv" and
#'  "xlsx". Supported file types: csv, xlsx
#'
#' @return a list of data frames, with each data frame the data  from a file
#'    in the path
#' @export
#' @importFrom fs dir_ls path_file
#' @importFrom purrr map
#' @importFrom readr read_csv cols col_character
#' @importFrom readxl read_xlsx
get_files_from_path <- function(path = ".", file_types = c("csv", "xlsx")) {
  # Get path
  folder_path <- path # assign the path as user input

  csv_data <- c()
  xlsx_data <- c()

  # Get file paths of all CSV or XLSX in directory (recursive)
  for(x in file_types) {
    if(x == "csv") {
      file_paths_csv <- dir_ls(
        path = folder_path,
        recurse = TRUE,
        glob = "*.csv")
      csv_data <- map(
        file_paths_csv,
        ~ read_csv(.x, col_types = cols(.default = col_character())))
      names(csv_data) <- path_file(file_paths_csv)
    }
    else if(x == "xlsx") {
      file_paths_xlsx <- dir_ls(
        path = folder_path,
        recurse = TRUE,
        glob = "*.xlsx")
      xlsx_data <- map(
        file_paths_xlsx,
        ~ read_xlsx(.x, col_types = "text"))
      names(xlsx_data) <- path_file(file_paths_xlsx)
    }
  }

  return(c(csv_data, xlsx_data))
}
