test_that("save_dict writes a CSV and XLSX file", {
  withr::with_tempdir({
    # Generate testing environment infrastructure ----
    generate_test_data_dir(tempdir())
    fs::dir_tree()

    # Read test files
    data_list <- get_files_from_path()

    save_dict(data_list)

    written_files <- list.files(path = "./Dictionary", pattern = "\\.csv$", full.names = TRUE)

    message(length(written_files))

    expect_true(length(written_files) == 12,
                info = "Expected number of files were not created!",
                label = "Checks that expected number of files (csv) were written.")

    save_dict(data_list, export_as = "xlsx")

    written_files <- list.files(path = "./Dictionary", pattern = "\\.xlsx$", full.names = TRUE)

    message(length(written_files))

    expect_true(length(written_files) == 1,
                info = "Expected number of files were not created!",
                label = "Checks that expected number of files (xlsx) were written")

    fs::dir_tree()
  })
})

