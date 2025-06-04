test_that("save_data_dictionary writes a valid Excel file", {
  # Create mock data
  sample_data <- list(
    "example1.csv" = tibble::tibble(id = c(1, 2, NA), name = c("Alice", "Bob", NA)),
    "another_file.xlsx" = tibble::tibble(score = c(95, NA, 82), passed = c(TRUE, FALSE, NA))
  )

  # Set fake source path to simulate directory
  source_path <- "some/nested/folder"

  # Expected file path
  expected_name <- "some_nested_folder_dictionary.xlsx"
  expected_path <- file.path(getwd(), expected_name)

  # Delete old file if it exists
  if (file.exists(expected_path)) unlink(expected_path)

  # Run the function
  save_data_dictionary(sample_data, source_path = source_path)

  print(stringr::str_c("expected path: ", expected_path))
  print(stringr::str_c("File exists: ", file.exists(expected_path)))
  # Check file was created
  expect_true(file.exists(expected_path))

  # Load workbook and check contents

  wb <- openxlsx::loadWorkbook(expected_path)
  sheets <- openxlsx::getSheetNames(expected_path)
  expect_true(any(grepl("example1", sheets)))
  expect_true(any(grepl("another_file", sheets)))

  # Clean up
  unlink(expected_path)
})

