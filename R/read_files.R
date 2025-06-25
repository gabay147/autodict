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
  error_list <- tibble::tibble(
    file = character(),
    sheet = character(),
    path = character(),
    error_msg = character()
  )

  # Get file paths of all CSV or XLSX in directory (recursive)
  for(x in file_types) {
    if(x == "csv") {
      file_paths_csv <- dir_ls(
        path = folder_path,
        recurse = TRUE,
        glob = "*.csv") |> print()
      csv_data <- map(file_paths_csv, function(file) {
        tryCatch({
          df <- read_csv(file, col_types = cols(.default = col_character()))
          if (nrow(readr::problems(df)) > 0) {
            print(readr::problems(df))
            stop("CSV has parsing problems")
          }
        }, error = function(e) {
          error_list <<- dplyr::bind_rows(
            error_list,
            tibble::tibble(
              file = basename(file),
              sheet = NA,
              path = file,
              error_msg = conditionMessage(e)
            )
          )
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
          error_list <<- dplyr::bind_rows(
            error_list,
            tibble::tibble(
              file = basename(file_path),
              sheet = NA,
              path = file_path,
              error_msg = conditionMessage(e)
            )
          )
          #print(error_list)
          return(character(0))
        })

        if (is.null(sheets)) next

        for (sheet in sheets) {
          df <- tryCatch({
            readxl::read_xlsx(file_path, sheet = sheet, col_types = "text")
          }, error = function(e) {
            message("Failed to read sheet '", sheet, "' in: ", file_path)
            error_list <<- dplyr::bind_rows(
              error_list,
              tibble::tibble(
                file = basename(file_path),
                sheet = sheet,
                path = file_path,
                error_msg = conditionMessage(e)
              )
            )
            #print(error_list)
            return(NULL)
          })

          if (is.null(df)) next

          # Construct path_base for naming
          # if keep_dir, the path_base will be the relative path of the file
          # starting at argument folder_path. Otherwise, path_base will simply
          # be the file name.
          path_base <- if (keep_dir) {
            fs::path_rel(file_path, start = folder_path)
          } else {
            fs::path_file(file_path)
          }

          # If sheets length is greater than 1, concatenate sheet name.
          # Otherwise, simply don't concatenate to avoid redundant naming.
          if (length(sheets) > 1) {
            df_name <- paste0(path_base, "-", sheet)
          } else {
            df_name <- path_base
          }

          xlsx_data[[df_name]] <- df
        }
      }
      if (debug) {message("\nlength of xlsx_data: ", length(xlsx_data))}
      if (debug) {message("xlsx_data names: ", names(xlsx_data))}

      # end XLSX block
      }

    else{message("File format not supported!")}
  }

  message("Done reading files! Final error_list: ")
  print(error_list)

  return(list(
    data = c(csv_data, xlsx_data),
    errors = error_list
  ))
}
