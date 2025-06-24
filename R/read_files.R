#' Read files from a directory recursively
#'
#' @param path The folder path to iterate through recursively. Defaults to the
#'  current working directory
#' @param file_types a list of file types to search for. Defaults to "csv" and
#'  "xlsx". Supported file types: csv, xlsx
#' @param keep_dir A boolean to determine whether to keep the full path of the
#' file as its name.
#'
#' @return a list of data frames, with each data frame the data  from a file
#'    in the path
#' @importFrom fs dir_ls path_file path_dir
#' @importFrom purrr map
#' @importFrom readr read_csv cols col_character
#' @importFrom readxl read_xlsx
#' @export
get_files_from_path <- function(path = ".", file_types = c("csv", "xlsx"), keep_dir = TRUE, debug = FALSE) {

  # Get path
  folder_path <- path # assign the path as user input

  # cat("Original folder path before logic check: ", folder_path, "\n")
  csv_data <- list()
  xlsx_data <- list()
  error_list <- list()

  # Get file paths of all CSV or XLSX in directory (recursive)
  for(x in file_types) {
    if(x == "csv") {
      file_paths_csv <- dir_ls(
        path = folder_path,
        recurse = TRUE,
        glob = "*.csv") |> print()
      csv_data <- map(file_paths_csv, function(file) {
        tryCatch({
          read_csv(file, col_types = cols(.default = col_character()))
        }, error = function(e) {
          error_list <<- c(error_list, file)
          return(NULL)
        })
      })
      if (keep_dir) {
        names(csv_data) <- fs::path_rel(file_paths_csv, start = folder_path)
      }
      else {
        names(csv_data) <- path_file(file_paths_csv)
      }

      #End CSV block
    }
    else if(x == "xlsx") {
      # Gets a list of XLSX files in the directory
      file_paths_xlsx <- dir_ls(
        path = folder_path,
        recurse = TRUE,
        glob = "*xlsx"
      )
      if (debug) {message(names(file_paths_xlsx))}

      # Build a list of data frames from each XLSX file
      for (file_path in file_paths_xlsx) {
        sheets <- tryCatch({
          readxl::excel_sheets(file_path)
        }, error = function(e) {
          message("Failed to read sheets from: ", file_path)
          error_list <- c(error_list, file_path)
          return(NULL)
        })

        if (is.null(sheets)) next

        for (sheet in sheets) {
          df <- tryCatch({
            readxl::read_xlsx(file_path, sheet = sheet, col_types = "text")
          }, error = function(e) {
            message("Failed to read sheet '", sheet, "' in: ", file_path)
            error_list <- c(error_list, paste0(file_path, "::", sheet))
            return(NULL)
          })

          if (is.null(df)) next

          path_base <- if (keep_dir) {
            fs::path_rel(file_path, start = folder_path)
          } else {
            fs::path_file(file_path)
          }

          df_name <- paste0(path_base, "_", sheet)
          xlsx_data[[df_name]] <- df
        }
      }
      if (debug) {message("\nlength of xlsx_data: ", length(xlsx_data))}
      if (debug) {message("xlsx_data names: ", names(xlsx_data))}

      # end XLSX block
      }

    else{message("File format not supported!")}
  }

  return(c(csv_data, xlsx_data))
}
