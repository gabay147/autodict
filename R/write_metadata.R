#' Save dictionary as XSLX (Deprecated)
#' Please use [save_dict()] instead.
#'
#' @param data The complete data, often as a list of data frames
#' @param source_path The path for the data, where the data will be
#'
#' @returns the file path of that the file was saved to
#' @seealso [save_dict()]
#' @export
save_dict_xlsx <- function(data, source_path = "data") {
  .Deprecated("save_dict")
  save_dict(data, export_as = "xlsx")
}

#' Save dictionary as CSV (Deprecated)
#' Please use [save_dict()] instead.
#'
#' @param data The complete data, often as a list of data frames
#'
#' @returns Nothing, but saves a series of csv files (.csv).
#' @seealso [save_dict()]
#' @export
save_dict_csv <- function(data) {
  .Deprecated("save_dict")
  save_dict(data, export_as = "csv")
}

#' Function to write the data dictionary to files. Can save the
#' dictionary as a series of CSV files or a single XLSX file with multiple
#' worksheets. `data` is a named list of data frames, most likely returned from
#' get_files_from_path. This function will always generate the folder
#' "Dictionary" wherever destination_path is. For example, by default,
#' `destination_path = "."` (the current working directory), so a new folder
#' "Dictionary" will be added to the working directory.
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

      #message(export_path)

      write.csv(metadata_df, export_path, row.names = FALSE)
    }
    return(invisible(export_path))
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
    return(invisible(export_path))
  }
  else (
    warning("File type not supported!")
  )
}
