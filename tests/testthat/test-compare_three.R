test_that("compare_three creates expected table", {
  test_df_1 <- tibble::tibble(
    nameL = c("Bob", "Gabay", "Anne"),
    nameF = c("Billy", "Jon", "Raggedy"),
    age = c(16, 17, 18)
  )
  test_df_2 <- tibble::tibble(
    LastName = c("Jon", "Jill", "Jack"),
    FirstName = c("Bo", "Bo", "Bo"),
    age = c(19, 20, 21)
  )
  test_df_3 <- tibble::tibble(
    FirstName = c("Tab", "Adam", "Jon"),
    nameL = c("Gabay", "gabay", "Gabby"),
    age = c(45, 31, 28)
  )

  expected <- list(
    dataset1_name = "Dataset1",
    dataset2_name = "Dataset2",
    dataset3_name = "Dataset3",
    unique_in_dataset1 = 3,
    unique_in_dataset2 = 3,
    unique_in_dataset3 = 3,
    common_between_1_and_2 = 0,
    common_between_1_and_3 = 1,
    common_between_2_and_3 = 0,
    common_all_datasets = 0,
    only_in_dataset1 = 2,
    only_in_dataset2 = 3,
    only_in_dataset3 = 2,
    jaccard_12 = 0,
    jaccard_13 = 0.2,
    jaccard_23 = 0,
    jaccard_all = 0,
    overlap_pct_dataset1 = 0,
    overlap_pct_dataset2 = 0,
    overlap_pct_dataset3 = 0,
    sample_common_all = character(0),
    sample_only_1 = c("Bob", "Anne"),
    sample_only_2 = c("Jon", "Jill", "Jack"),
    sample_only_3 = c("gabay", "Gabby")
  )

  expect_equal(compare_three(test_df_1$nameL, test_df_2$LastName, test_df_3$nameL), expected)
})
