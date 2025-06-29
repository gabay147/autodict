test_that("get_files_from_path reads both csv and xlsx files", {
  withr::with_tempdir({

    # Generate test data directory
    generate_test_data_dir(tempdir(), test_errors = TRUE)

    # message(fs::dir_tree())

    # On to testing get_files_from_path()
    read_data1 <- get_files_from_path()
    #expect_length(read_data1$data, 20)

    for (name in names(read_data1$data)) {
      message(name)
    }
    message(read_data1$errors)

    # message("keep_dir = FALSE")
    # message(names(read_data1))

    read_data2 <- get_files_from_path(keep_dir = TRUE)
    #expect_length(read_data2, 20)

    # message("keep_dir = TRUE")
    # message(names(read_data2))
  })
})

