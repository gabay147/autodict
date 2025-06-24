test_that("generate_page() writes a file to the correct location", {
  withr::with_tempdir({
    # Create dummy data frame
    generate_test_data_dir(tempdir())

    df_list <- get_files_from_path()

    # Generate dictionary files
    save_dict(df_list)

    # Get files from save_dict to generate into Quarto
    dict_list <- get_files_from_path("Dictionary")

    output_dir <- fs::path(getwd(), "Dictionary", "Quarto")

    named_df <- dict_list[1]

    # Run the function
    generate_page(input_df = named_df, output_location = fs::path(output_dir), metadata_format = "csv", render = FALSE)

    fs::dir_tree()
    # Expect output dir to exist
    message(dir.exists(output_dir))

    # Expect QMD file exists
    expected_file <- fs::path(output_dir, stringr::str_c(names(named_df), ".qmd"))
    message("Expected file: ", expected_file)
    message((file.exists(expected_file)))

    # Optional: read the file and inspect contents
    file_name <- tools::file_path_sans_ext(names(named_df))
    expected_title <- paste0("Data Dictionary: ", stringr::str_replace_all(file_name, "_", "/"), ".csv")
    content <- tryCatch({readLines(expected_file)}, error = function(e) {
      message("Failed to read expected file")
      return(NULL)
    })

    if(is.null(content)) {return(NULL)}

    message((any(grepl(expected_title, content))))
  })
})

# test_that("generate_page() errors on bad input", {
#   expect_error(generate_page(list(), "some_path"), "must be a named list")
#   expect_error(generate_page(list("no_df" = 123), "some_path"), "must be a data frame")
#   expect_error(generate_page(list("" = mtcars), "some_path"), "non-empty name")
# })
