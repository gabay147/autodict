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

#' Takes in a data frame and scraped descriptive metadata from the data set.
#' Used primarily in [save_dict()]
#'
#' @param df The single data frame to gather metadata about
#'
#' @seealso [save_dict()]
#' @return a list of metadata about a single data frame
#' @export
generate_meta_data <- function(df) {
  # Get basic information
  column_names <- names(df)
  n_rows <- nrow(df)

  if (ncol(df) > 1) {
    percent_missing <- round(colMeans(is.na(df)) * 100, 2)
  } else if (ncol(df) == 1) {
    percent_missing <- setNames(
      round(mean(is.na(df[[1]])) * 100, 2),
      names(df)
    )
  } else {
    percent_missing <- numeric(0)
  }

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

  return(metadata_df)
}

#' A function that generates a test directory with test files in the desired
#' directory. Likely to be used in a `usethis` test environment with a `withr`
#' tempdir.
#'
#' @return invisible(nested_path)
#' @export
generate_test_data_dir <- function(base_dir, test_errors = FALSE) {
  # Generate test directory within tempdir() ----
  nested_path <- file.path("some", "nested", "directory")

  dir.create(nested_path, recursive = TRUE)

  expect_true(dir.exists("some/nested/directory"),
              info = "The directory should have been created - subsequent file
                generation for test will fail!",
              label = "Check for directory existence")

  # Generate test files within test sub directories ----

  test_dir <- c(".", "some", "nested", "directory")

  ## Create files for each test subdirectory ----
  # 2 data frames are generated and will be saved as both CSV (2 files) and
  # XLSX (1 file, 2 worksheets)
  for (i in seq_along(test_dir)) {

    # The relative partial path to save files to
    partial_path <- do.call(file.path, as.list(test_dir[1:i]))

    # message(stringr::str_c("Partial path: ", partial_path))

    ## Generate data frames ----
    df1 <- tibble::tibble(
      id = c(1:10),
      score = runif(10, min = 0, max = 100),
      category = sample(c("low", "medium", "high"), 10, replace = TRUE),
      group = sample(LETTERS[1:3], 10, replace = TRUE),
      is_new = sample(c(TRUE, FALSE), size = 10, replace = TRUE, prob = c(0.7, 0.3))
    )

    # message("generated df1")

    df2 <- tibble::tibble(
      id = c(11:20),
      score = rnorm(10, mean = 50, sd = 1),
      category = sample(c("low", "medium", "high"), 10, replace = TRUE),
      group = sample(LETTERS[1:3], 10, replace = TRUE),
      is_new = sample(c(TRUE, FALSE), size = 10, replace = TRUE, prob = c(0.3, 0.7))
    )

    # message("generated df2")

    ## Write CSV files ----
    write.csv(df1, file = stringr::str_c(partial_path, "/df1.csv"))
    write.csv(df2, file = stringr::str_c(partial_path, "/df2.csv"))

    expect_true(file.exists(stringr::str_c(partial_path, "/df1.csv")),
                info = "File df1 should have been created!",
                label = "Check for file existence: df1.csv")
    expect_true(file.exists(stringr::str_c(partial_path, "/df2.csv")),
                info = "File df2 should have been created!",
                label = "Check for file existence: df2.csv")

    ## Write XLSX file with multiple sheets ----
    wb <- openxlsx::createWorkbook("Data Frames")
    openxlsx::addWorksheet(wb, "df1")
    openxlsx::addWorksheet(wb, "df2")

    openxlsx::writeData(wb, sheet = 1, df1)
    openxlsx::writeData(wb, sheet = 2, df2)
    openxlsx::saveWorkbook(wb, stringr::str_c(partial_path, "/data frames.xlsx"), overwrite = TRUE)

    expect_true(file.exists(stringr::str_c(partial_path, "/Data Frames.xlsx")),
                info = "File Data Frames should have been created!",
                label = "Check for file existence: Data Frames.xlsx")

    ## Write XLSX file with single sheet ----

    df3 <- dplyr::bind_rows(df1, df2)
    openxlsx::write.xlsx(df3, stringr::str_c(partial_path, "/combined.xlsx"), overwrite = TRUE)

    expect_true(file.exists(stringr::str_c(partial_path, "/combined.xlsx")),
                info = "File combined.xlsx should have been created!",
                label = "Check for file existence: combined.xlsx")

    # Test malformed files that will throw an error on reading
    if(test_errors) {
      writeBin(as.raw(c(0x00, 0xFF, 0xFE, 0x41, 0x42)), stringr::str_c(partial_path, "/Invalid.csv"))

      writeLines(c(
        'col1,col2',
        'value1,"value2',
        'value3\tvalue4'
      ), stringr::str_c(partial_path, "/bad_delim.csv"))

      writeLines("This is not an Excel file.", stringr::str_c(partial_path, "/fake_xlsx.xlsx"))
    }
  }

  write("This is just a text file! Nyahahaha!", file = "./some/nested/red_herring.txt")
  return(invisible(nested_path))
}

#' A function to normalize a name from a path - used in cases where the path of
#' a file is used in the generation of a unique file name that also shows its
#' source directory. Primarily used in [save_dict()]
#'
#' @param name A single string file name that may contain file path conventions
#' - desired to be made safe for file naming.
#'
#' @return the normalized name
#' - Slashes are replaced with an underscore
#' - Extensions .csv and .xlsx are removed
#' @export
normalize_name <- function(name) {
  name <- stringr::str_replace_all(name, "[/\\\\]", "_")
  name <- stringr::str_replace(name, "\\.csv$|\\.xlsx$", "")
  return(name)
}
