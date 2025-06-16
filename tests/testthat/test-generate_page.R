test_that("generate_page() writes a file to the correct location", {
  withr::with_tempdir({
    # Create dummy data frame
    df <- data.frame(a = 1:5, b = letters[1:5])
    named_df <- list("test_file.csv" = df)

    # Create output directory
    output_dir <- file.path(tempdir(), "output")

    # Run the function
    generate_page(named_df, output_dir, metadata_format = "csv", render = FALSE)

    # Expect output dir to exist
    expect_true(dir.exists(output_dir))

    # Expect QMD file exists
    expected_file <- file.path(output_dir, "test_file.csv.qmd")
    expect_true(file.exists(expected_file))

    # Optional: read the file and inspect contents
    file_name <- tools::file_path_sans_ext(names(named_df))
    expected_title <- paste0("Data Dictionary: ", stringr::str_replace_all(file_name, "_", "/"), ".csv")
    content <- readLines(expected_file)

    expect_true(any(grepl(expected_title, content)))
  })
})

# test_that("generate_page() errors on bad input", {
#   expect_error(generate_page(list(), "some_path"), "must be a named list")
#   expect_error(generate_page(list("no_df" = 123), "some_path"), "must be a data frame")
#   expect_error(generate_page(list("" = mtcars), "some_path"), "non-empty name")
# })
