#' Function to save the data dictionary as an excel workbook
#' This function takes in "data" - the list of data frames obtained from
#' get_files_from_path. Each individual data frame is processed for meta data
#' and saved as a worksheet to be compiled into a complete workbook
#'
#' @param data The complete data, often as a list of data frames
#' @param source_path The path for the data, where the data will be
#'
#' @returns the file path of that the file was saved to
#' @export
save_dict_xlsx <- function(data, source_path = "data") {

  ifelse(!dir.exists("Dictionaries"), dir.create("Dictionaries"), "Directory Exists")

  output_name <- paste0(
    "Dictionaries/",
    stringr::str_replace_all(source_path, "/", "_"),
    "_dictionary.xlsx"
  )
  output_path <- file.path(getwd(), output_name)

  wb <- openxlsx::createWorkbook()
  existing_titles <- character()

  # Loop through each data frame name and extract metadata
  for (i in seq_along(data)) {
    df <- data[[i]]
    df_name <- names(data)[i]

    base_title <- stringr::str_replace_all(df_name, "[/\\?*\\[\\]:]", "_")
    base_title <- stringr::str_remove(base_title, "\\.csv$|\\.xlsx$")
    base_title <- substr(base_title, 1, 28)

    sheet_title <- make.unique(c(existing_titles, base_title))[length(existing_titles) + 1]
    existing_titles <- c(existing_titles, sheet_title)

    # Get basic information
    column_names <- names(df)
    n_rows <- nrow(df)
    percent_missing <- round(colMeans(is.na(df)) * 100, 2)

    # Extract detailed information for each column
    metadata_list <- list()

    for (col_name in column_names) {
      metadata_list[[col_name]] <- tibble::tibble(
        `Variable Name` = col_name,
        `Data Type` = extract_data_type(df, col_name),
        `Rows` = n_rows,
        `Example` = get_example(df, col_name),
        `Percent Missing` = percent_missing[col_name],
        `Description` = ""  # Empty for manual entry later
      )
    }

    # Combine all columns into a single data frame
    metadata_df <- dplyr::bind_rows(metadata_list)

    # Add worksheet (sheet names must be ≤ 31 characters and unique)
    openxlsx::addWorksheet(wb, sheet_title)
    openxlsx::writeData(wb, sheet = sheet_title, x = metadata_df)
  }

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  return(invisible(output_path))
}

#' Function to save the data dictionary as a directory of CSVs
#' This function takes in "data" - the list of data frames obtained from
#' get_files_from_path. Each individual data frame is processed for meta data
#' and saved as a CSV in a directory that mirrors the directory of its source.
#'
#' @param data The complete data, often as a list of data frames
#' @param source_path The path for the data, where the data will be
#'
#' @returns Nothing, but saves a series of csv files (.csv).
#' @export
save_dict_csv <- function(data, source_path = "data") {

  original_wd <- getwd()

  names(data)

  ifelse(!dir.exists("Dictionaries"), dir.create("Dictionaries"), "Directory Exists")

  setwd("Dictionaries")

  for (i in seq_along(data)) {
    print(data[i])
    data_name <- fs::path_file(names(data[i]))

    data_dir <- fs::path_dir(names(data[i]))
    data_dir_split <- stringr::str_split_1(data_dir, "/")

    # Create directories
    for (j in seq_along(data_dir_split)) {

      ifelse(
        !dir.exists(data_dir_split[j]),
        dir.create(data_dir_split[j]),
        "Directory Exists")
      setwd(data_dir_split[j])
    }

    df <- data[[i]]
    df_name <- names(data)[i]

    # Get basic information
    column_names <- names(df)
    n_rows <- nrow(df)
    percent_missing <- round(colMeans(is.na(df)) * 100, 2)

    # Extract detailed information for each column
    metadata_list <- list()

    for (col_name in column_names) {
      metadata_list[[col_name]] <- tibble::tibble(
        `Variable Name` = col_name,
        `Data Type` = extract_data_type(df, col_name),
        `Rows` = n_rows,
        `Example` = get_example(df, col_name),
        `Percent Missing` = percent_missing[col_name],
        `Description` = ""  # Empty for manual entry later
      )
    }

    # Combine all columns into a single data frame
    metadata_df <- dplyr::bind_rows(metadata_list)

    print(stringr::str_c("writing to: ", data_dir, "/", data_name))
    write.csv(metadata_df, data_name)

    setwd(original_wd)
    setwd("Dictionaries")
  }

  setwd(original_wd)
  print("Done saving!")
}
