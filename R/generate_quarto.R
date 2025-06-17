#' Function for generating a Quarto page for a single metadata-format data
#' frame. For batch processing, see `generate_pages()`
#'
#' @param input_df A named list that contains the single data frame to render.
#' @param output_location A path denoting the destination for the rendered file.
#' @param render Whether to render the quarto file or not
#' @param metadata_format The expected file format of the metadata file
#'
#' @export
generate_page <- function(
    input_df,
    output_location,
    render = FALSE,
    metadata_format = c("csv") # Currently unused - included for future support of other file types
) {
  # Currently unused - included for future support of other file types
  metadata_format <- match.arg(metadata_format)

  # Check arguments
  if (!is.list(input_df) || is.null(names(input_df)) || length(input_df) != 1) {
    stop("`input_df` must be a named list containing exactly one data frame.")
  }
  if (!inherits(input_df[[1]], "data.frame")) {
    stop("The element in `input_df` must be a data frame.")
  }
  if (!dir.exists(output_location)) {
    message("output_location does not exist! Attempting to generate...")
    created <- tryCatch({
      dir.create(output_location, recursive = TRUE)
    }, error = function(e) {
      stop("Failed to create directory at ", output_location, ": ", e$message)
    })
    if (!isTRUE(created)) {
      stop("Attempted to create directory at ", output_location, " but dir.create() returned FALSE!")
    }
  }

  # Validate name is not null
  name <- names(input_df)
  if (is.null(name) || name == "") {
    stop("`input_df` must have a non-empty name.")
  }

  # Build path
  safe_name <- stringr::str_replace_all(name, "/", "_")
  qmd_path <- file.path(output_location, paste0(safe_name, ".qmd"))


  # Quarto content
  qmd <- c(
    "---",
    paste0("title: \"Data Dictionary: ", stringr::str_replace_all(name, "_", "/"), ".csv\""),
    "format: html",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(readr)",
    "library(dplyr)",
    "library(knitr)",
    "library(tibble)",
    "library(DT)",
    "library(autodict)",
    "```",
    "",
    "```{r load-metadata}",
    paste0("metadata <- read.csv(\"../", name, "\")"),
    "",
    "datatable(metadata)",
    "```"
  )

  # Write the file
  write_result <- tryCatch({
    writeLines(qmd, qmd_path)
    TRUE
  }, error = function(e) {
    warning("Failed to write Quarto file at ", qmd_path, ": ", e$message)
    FALSE
  })

  # Optionally render
  if (render && write_result) {
    tryCatch({
      quarto::quarto_render(qmd_path)
    }, error = function(e) {
      warning("Quarto render failed for: ", qmd_path, "\n", e$message)
    })
  }
}



#' A batch-processing extension of `generate_page()`. Instead of a single data
#' frame, this function takes in a directory path and parses through it
#' recursively, generating data dictionary Quarto pages for each metadata file.
#'
#' @param input_dir input path to parse through
#' @param output_dir Desired destination folder for all pages
#' @param render render quarto or not (if FALSE, only generates the quarto
#' markdown file)
#' @param metadata_format the format of the files being read
#' @param keep_dir whether to keep the file path in the name of dfs
#'
#' @return nothing - generates quarto pages
#' @export
generate_pages <- function(
    input_dir,
    output_dir = "dictionary_pages",
    render = FALSE,
    metadata_format = c("csv"),
    keep_dir = TRUE
) {
  metadata_format <- match.arg(metadata_format)

  if(!dir.exists(input_dir)) {
    stop("The input_dir does not exist: ", input_dir)
  }

  ifelse(!dir.exists(output_dir), dir.create(output_dir), "Directory Exists")

  # get_files_from_path returns a list of data frames built from the directory
  file_list <- get_files_from_path(input_dir, metadata_format, keep_dir)
  # print(names(file_list))

  for (i in seq_along(file_list)) {
    name <- names(file_list)[i]
    message("Generating page for: ", name)
    generate_page(file_list[i], output_dir, render, metadata_format)
  }
  invisible(NULL)
}
