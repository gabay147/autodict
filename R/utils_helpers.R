#' After determining data needs, update function to manually set data types.
#' currently dependent on the saved data type of columns in the data frame,
#' which upon reading, is currently overridden as character.
#'
#' @param df The dataframe to examine
#' @param col_name The name of the column to examine
#'
#' @return A character string that describes the datatype of the column
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
#' @return a string of the first non-NA value. If all values are NA, returns "All NA"
get_example <- function(df, col_name) {
  col_data <- df[[col_name]]
  non_na_values <- col_data[!is.na(col_data)]
  if (length(non_na_values) > 0) {
    return(as.character(non_na_values[1]))
  } else {
    return("All NA")
  }
}

#' Standardizes item ids to match in length, adds zeroes mostly
#'
#' @param item_ids A list of values within a column of a data frame to be standardized for comparison
#' @param target_length The desired length of the ID
#'
#' @return Standardized IDs with 0's padding to maintain uniform length
#' @export
standardize_item_id <- function(item_ids, target_length = NULL) {
  # Convert to character first
  ids_char <- as.character(item_ids)

  # Remove any existing leading/trailing whitespace
  ids_char <- trimws(ids_char)

  # If target_length not specified, use the maximum length found
  if (is.null(target_length)) {
    # Only consider numeric-looking IDs for length calculation
    numeric_ids <- ids_char[grepl("^[0-9]+$", ids_char)]
    if (length(numeric_ids) > 0) {
      target_length <- max(nchar(numeric_ids), na.rm = TRUE)
    } else {
      # If no numeric IDs found, don't pad
      return(ids_char)
    }
  }

  # Pad numeric IDs with leading zeros
  standardized <- ifelse(
    grepl("^[0-9]+$", ids_char),  # If it's purely numeric
    sprintf(paste0("%0", target_length, "s"), ids_char),  # Pad with zeros
    ids_char  # Keep non-numeric IDs as-is
  )

  return(standardized)
}

#' Previews the standardization of variable names, including a comparison between the original and standardized lengths.
#' To be used in tendem with standardize_item_id
#'
#' @param item_ids A list of values to be standardize
#' @param target_length The desired length ot standardization to preview
#'
#' @return Nothing - Instead provides console output with details
#' @export
preview_standardization <- function(item_ids, target_length = NULL) {
  original_sample <- head(unique(item_ids), 10)
  standardized_sample <- head(unique(standardize_item_id(item_ids, target_length)), 10)

  cat("STANDARDIZATION PREVIEW:\n")
  cat("Target length:", ifelse(is.null(target_length), "Auto-detect", target_length), "\n\n")

  preview_df <- data.frame(
    Original = original_sample,
    Standardized = standardized_sample[1:length(original_sample)]
  )

  print(preview_df)

  # Show length statistics
  cat("\nLENGTH STATISTICS:\n")
  original_lengths <- nchar(as.character(item_ids))
  standardized_lengths <- nchar(standardize_item_id(item_ids, target_length))

  cat("Original lengths - Min:", min(original_lengths, na.rm = TRUE),
      "Max:", max(original_lengths, na.rm = TRUE), "\n")
  cat("Standardized lengths - Min:", min(standardized_lengths, na.rm = TRUE),
      "Max:", max(standardized_lengths, na.rm = TRUE), "\n")
}
