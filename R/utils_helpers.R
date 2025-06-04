#' After determining data needs, update function to manually set data types.
#' currently dependent on the saved data type of columns in the data frame,
#' which upon reading, is currently overridden as character.
#'
#' @param df The dataframe to examine
#' @param col_name The name of the column to examine
#'
#' @returns A character string that describes the datatype of the column
extract_data_type <- function(df, col_name) {
  col_data <- df[[col_name]]
  if (is.numeric(col_data)) {
    if (is.integer(col_data)) return("integer")
    else return("numeric")
  } else if (is.character(col_data)) {
    return("character")
  } else if (is.factor(col_data)) {
    return("factor")
  } else if (is.logical(col_data)) {
    return("logical")
  } else if (inherits(col_data, "Date")) {
    return("Date")
  } else if (inherits(col_data, "POSIXt")) {
    return("POSIXt")
  } else {
    return(class(col_data)[1])
  }
}

#' Gets a non-NA example of the data stored in the column (if applicable)
#'
#' @param df The data frame to examine
#' @param col_name The column name in the data frame to examine
#'
#' @returns a string of the first non-NA value. If all values are NA, returns
#'  "All NA"
get_example <- function(df, col_name) {
  col_data <- df[[col_name]]
  non_na_values <- col_data[!is.na(col_data)]
  if (length(non_na_values) > 0) {
    return(as.character(non_na_values[1]))
  } else {
    return("All NA")
  }
}
