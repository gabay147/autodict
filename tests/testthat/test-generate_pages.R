test_that("generate_pages() generates a .qmd file for each input file", {
  withr::with_tempdir({
    # Create input directory
    generate_test_data_dir(tempdir())

    df_list <- get_files_from_path()

    message("\nDF list: ", names(df_list))

    # Generate dictionary files
    save_dict(df_list)

    # Get files from save_dict to generate into Quarto
    input_dir <- fs::path("Dictionary")

    message("\nInput directory: ", input_dir)

    message("Current wd: ", getwd())

    output_dir <- fs::path(getwd(), "Dictionary", "Quarto")
    output_dir <- fs::path_rel(output_dir, start = ".")


    # Run function
    generate_pages(input_dir, output_dir, render = FALSE, metadata_format = c("csv", "xlsx"), keep_dir = FALSE)

    print(fs::dir_ls(output_dir))

    expected_filenames <- stringr::str_replace_all(names(df_list), "[/ ]", "_")
    expected_qmds <- file.path(output_dir, expected_filenames)
    expected_qmds <- fs::path_ext_set(expected_qmds, ".qmd")


    missing <- expected_qmds[!file.exists(expected_qmds)]
    message(paste("Missing files:", paste(missing, collapse = ", ")))
  })
})


