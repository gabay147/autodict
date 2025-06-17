test_that("get_files_from_path reads both csv and xlsx files", {
  withr::with_tempdir({

    # Generate test directory within tempdir() ----
    base_dir <- tempdir()
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

      ## Write XLSX file ----
      wb <- openxlsx::createWorkbook("Data Frames")
      openxlsx::addWorksheet(wb, "df1")
      openxlsx::addWorksheet(wb, "df2")

      openxlsx::writeData(wb, sheet = 1, df1)
      openxlsx::writeData(wb, sheet = 2, df2)
      openxlsx::saveWorkbook(wb, stringr::str_c(partial_path, "/data frames.xlsx"), overwrite = TRUE)

      expect_true(file.exists(stringr::str_c(partial_path, "/Data Frames.xlsx")),
                  info = "File Data Frames should have been created!",
                  label = "Check for file existence: Data Frames.xlsx")
    }

    write("This is just a text file! Nyahahaha!", file = "./some/nested/red_herring.txt")

    # message(fs::dir_tree())

    # On to testing get_files_from_path() ----

    read_data1 <- get_files_from_path()

    expect_length(read_data1, 12)

    # message("keep_dir = FALSE")
    # message(names(read_data1))

    read_data2 <- get_files_from_path(keep_dir = TRUE)
    expect_length(read_data2, 12)

    # message("keep_dir = TRUE")
    # message(names(read_data2))
  })
})

