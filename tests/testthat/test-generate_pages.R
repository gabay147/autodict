test_that("generate_pages() generates a .qmd file for each input file", {
  withr::with_tempdir({
    # Create input directory
    input_dir <- file.path(tempdir(), "input")
    dir.create(input_dir)

    # Create multiple metadata CSVs
    df1 <- data.frame(id = 1:3, value = c("A", "B", "C"))
    df2 <- data.frame(name = c("X", "Y", "Z"), score = 1:3)

    readr::write_csv(df1, file.path(input_dir, "meta1.csv"))
    print(file.exists(file.path(input_dir, "input_meta.csv.qmd")))
    readr::write_csv(df2, file.path(input_dir, "meta2.csv"))

    # Output directory
    output_dir <- file.path(tempdir(), "qmds")

    # Run function
    generate_pages(input_dir, output_dir, render = FALSE, metadata_format = "csv")

    # Check output files
    expect_true(file.exists(file.path(output_dir, "input_meta1.csv.qmd")))
    expect_true(file.exists(file.path(output_dir, "input_meta2.csv.qmd")))
  })
})


