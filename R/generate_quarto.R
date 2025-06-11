generate_dict_pages <- function(
    input_dir,
    output_dir = "dictionary_pages",
    render = FALSE,
    metadata_format = c("csv")
) {
  metadata_format <- match.arg(metadata_format)

  ifelse(!dir.exists(output_dir), dir.create(output_dir), "Directory Exists")

  # get_files_from_path returns a list of data frames built from the directory
  file_list <- get_files_from_path(input_dir, metadata_format)
  print(names(file_list))

  for (i in seq_along(file_list)) {

    name <- stringr::str_remove(names(file_list)[i], ".csv")

    qmd_path <- file.path(output_dir, paste0(name, ".qmd"))


    # Quarto content
    qmd <- c(
      "---",
      paste0("title: \"Data Dictionary: ", name, "\""),
      "format: html",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(readr)",
      "library(dplyr)",
      "library(knitr)",
      "library(tibble)",
      "```",
      "",
      "```{r load-metadata}",
      # sprintf("metadata <- read_csv(\"%s\")", relative_path),
      "",
      "kable(metadata)",
      "```"
    )

    writeLines(qmd, qmd_path)

    if (render) {
      quarto::quarto_render(qmd_path)
    }
  }
}
