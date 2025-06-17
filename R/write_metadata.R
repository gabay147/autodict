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

    generate_meta_data(df)

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
#'
#' @returns Nothing, but saves a series of csv files (.csv).
#' @export
save_dict_csv <- function(data) {

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

    metadata_df <- generate_meta_data(df)

    print(stringr::str_c("writing to: ", data_dir, "/", data_name))
    write.csv(metadata_df, data_name)

    setwd(original_wd)
    setwd("Dictionaries")
  }

  setwd(original_wd)
  print("Done saving!")
}

#' desc
#'
#' @param data a list of named data frames, likely obtained from
#' get_files_from_path()
#'
#' @return what it returns
#' @export
save_dict <- function(data, export_as = "csv", destination_path = ".") {

  safe_names <- make.unique(vapply(names(data), normalize_name, character(1)))

  output_path <- fs::path(destination_path, "Dictionary")

  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }

  if (export_as == "csv") {
    for (i in seq_along(data)) {
      df <- data[[i]]
      df_name <- names(data)[i]

      metadata_df <- generate_meta_data(df)

      export_path <- file.path(output_path, stringr::str_c(safe_names[i], ".csv"))

      message(export_path)

      write.csv(metadata_df, export_path, row.names = FALSE)
    }
  }
  else if (export_as == "xlsx") {
    wb <- openxlsx::createWorkbook()
    existing_titles <- character()

    for (i in seq_along(data)) {
      df <- data[[i]]
      df_name <- names(data)[i]

      base_title <- stringr::str_replace_all(df_name, "[/\\?*\\[\\]:]", "_")
      base_title <- stringr::str_remove(base_title, "\\.csv$|\\.xlsx$")
      base_title <- substr(base_title, 1, 28)

      sheet_title <- make.unique(c(existing_titles, base_title))[length(existing_titles) + 1]
      existing_titles <- c(existing_titles, sheet_title)

      metadata_df <- generate_meta_data(df)

      export_path <- file.path(output_path, stringr::str_c(safe_names[i], ".xlsx"))

      openxlsx::addWorksheet(wb, sheet_title)
      openxlsx::writeData(wb, sheet = sheet_title, x = metadata_df)
    }
    openxlsx::saveWorkbook(wb, export_path, overwrite = TRUE)
    return(invisible(output_path))
  }
  else (
    warning("File type not supported!")
  )
}
