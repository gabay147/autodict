#' Generates quarto pages from dictionaries in input_dir
#'
#' @param input_dir input path
#' @param output_dir output path
#' @param render render quarto or not
#' @param metadata_format the format of the files being read
#'
#' @return nothing - generates quarto pages
#' @export
generate_dict_pages <- function(
    input_dir,
    output_dir = "dictionary_pages",
    render = FALSE,
    metadata_format = c("csv")
) {
  metadata_format <- match.arg(metadata_format)

  ifelse(!dir.exists(output_dir), dir.create(output_dir), "Directory Exists")

  # get_files_from_path returns a list of data frames built from the directory
  file_list <- get_files_from_path(input_dir, metadata_format, keep_dir = TRUE)
  # print(names(file_list))

  for (i in seq_along(file_list)) {

    name <- stringr::str_remove(names(file_list)[i], ".csv")

    qmd_path <- file.path(output_dir, paste0(stringr::str_replace_all(name, "/", "_"), ".qmd"))


    # Quarto content
    qmd <- c(
      "---",
      paste0("title: \"Data Dictionary: ", stringr::str_replace_all(name, "_", "/"), ".csv", "\""),
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
      paste0("metadata <- read.csv(\"../", names(file_list)[i], "\")"),
      "",
      "datatable(metadata)",
      "```"
    )

    writeLines(qmd, qmd_path)

    if (render) {
      quarto::quarto_render(qmd_path)
    }
  }
}
