#' Compares the column names of data frames or files in df_list for similarities.
#' Generates a data frame showing which variable lives where
#'
#' @param df_list The list of data frames to compare column names from
#' @param df_names The desired names for the data frames. Defaults as NULL
#'
#' @return A data frame that compares all variables across data frames in df_list
#' @export
compare_column_names <- function(df_list, df_names = NULL) {
  # If no names provided, auto-name them
  if (is.null(df_names)) {
    df_names <- paste0("df", seq_along(df_list))
  }
  names(df_list) <- df_names

  # Get list of column names for each data frame
  name_lists <- lapply(df_list, names)

  # Create a data frame showing which variable is in which data frame
  all_vars <- unique(unlist(name_lists))
  presence_matrix <- sapply(name_lists, function(x) all_vars %in% x)
  presence_df <- as.data.frame(presence_matrix)
  presence_df$Variable <- all_vars

  # Rearrange columns
  presence_df <- presence_df[, c("Variable", df_names)]

  return(presence_df)
}

#' Compares two lists for identical values. Can be used to search for identical
#' observations between data frames.
#'
#' @param ids1 The first column/list to compare
#' @param ids2 The second column/list to compare
#' @param names A list of length 2 of desired names for the data sets
#'
#' @return a list containing:
#' - Dataset names
#' - num of unique values in each
#' - num of common items in each
#' - num of values solely in each
#' - jaccard similarity
#' - overlap percent in both
#' - Sample of a common value
#' - Sample of unique values in each
#' @export
compare_two <- function(ids1, ids2,
                        names = c("Dataset1", "Dataset2")) {
  # Standardize IDs for each vector
  std_ids1 <- standardize_item_id(ids1)
  std_ids2 <- standardize_item_id(ids2)

  # Get unique IDs (ignoring NA values)
  unique_ids1 <- unique(std_ids1[!is.na(std_ids1)])
  unique_ids2 <- unique(std_ids2[!is.na(std_ids2)])

  # Pairwise intersections
  intersection <- intersect(unique_ids1, unique_ids2)
  only_in_1 <- setdiff(unique_ids1, unique_ids2)
  only_in_2 <- setdiff(unique_ids2, unique_ids1)

  results <- list(
    dataset1_name = names[1],
    dataset2_name = names[2],
    unique_in_dataset1 = length(unique_ids1),
    unique_in_dataset2 = length(unique_ids2),
    common_items = length(intersection),
    only_in_dataset1 = length(only_in_1),
    only_in_dataset2 = length(only_in_2),
    jaccard_similarity = length(intersection) / length(union(unique_ids1, unique_ids2)),
    overlap_pct_dataset1 = length(intersection) / length(unique_ids1) * 100,
    overlap_pct_dataset2 = length(intersection) / length(unique_ids2) * 100,
    sample_common = head(intersection, 10),
    sample_only_1 = head(only_in_1, 5),
    sample_only_2 = head(only_in_2, 5)
  )

  return(results)
}

#' Compares three lists for identical values. Can be used to search for identical
#' observations between data frames.
#'
#' @param ids1 The first column/list to compare
#' @param ids2 The second column/list to compare
#' @param ids3 The third column/list to compare
#' @param names A list of length 3 of desired names for the data sets
#'
#' @return a list containing:
#' - Dataset names
#' - num of unique values in each
#' - num of common items in each
#' - num of values solely in each
#' - jaccard similarity
#' - overlap percent in both
#' - Sample of a common value
#' - Sample of unique values in each
#' @export
compare_three <- function(ids1, ids2, ids3,
                          names = c("Dataset1", "Dataset2", "Dataset3")) {
  # Standardize IDs for each vector
  std_ids1 <- standardize_item_id(ids1)
  std_ids2 <- standardize_item_id(ids2)
  std_ids3 <- standardize_item_id(ids3)

  # Get unique IDs (ignoring NA values)
  unique_ids1 <- unique(std_ids1[!is.na(std_ids1)])
  unique_ids2 <- unique(std_ids2[!is.na(std_ids2)])
  unique_ids3 <- unique(std_ids3[!is.na(std_ids3)])

  # Pairwise intersections
  intersection_12 <- intersect(unique_ids1, unique_ids2)
  intersection_13 <- intersect(unique_ids1, unique_ids3)
  intersection_23 <- intersect(unique_ids2, unique_ids3)

  # Intersection among all three datasets
  intersection_all <- Reduce(intersect, list(unique_ids1, unique_ids2, unique_ids3))

  # Items unique to each dataset (i.e. not present in the other two)
  only_in_1 <- setdiff(unique_ids1, union(unique_ids2, unique_ids3))
  only_in_2 <- setdiff(unique_ids2, union(unique_ids1, unique_ids3))
  only_in_3 <- setdiff(unique_ids3, union(unique_ids1, unique_ids2))

  # Overall union for all three datasets
  union_all <- union(unique_ids1, union(unique_ids2, unique_ids3))

  # Jaccard similarity for pairs and overall
  jaccard_12 <- length(intersection_12) / length(union(unique_ids1, unique_ids2))
  jaccard_13 <- length(intersection_13) / length(union(unique_ids1, unique_ids3))
  jaccard_23 <- length(intersection_23) / length(union(unique_ids2, unique_ids3))
  jaccard_all <- length(intersection_all) / length(union_all)

  # Create results list
  results <- list(
    dataset1_name = names[1],
    dataset2_name = names[2],
    dataset3_name = names[3],
    unique_in_dataset1 = length(unique_ids1),
    unique_in_dataset2 = length(unique_ids2),
    unique_in_dataset3 = length(unique_ids3),
    common_between_1_and_2 = length(intersection_12),
    common_between_1_and_3 = length(intersection_13),
    common_between_2_and_3 = length(intersection_23),
    common_all_datasets = length(intersection_all),
    only_in_dataset1 = length(only_in_1),
    only_in_dataset2 = length(only_in_2),
    only_in_dataset3 = length(only_in_3),
    jaccard_12 = jaccard_12,
    jaccard_13 = jaccard_13,
    jaccard_23 = jaccard_23,
    jaccard_all = jaccard_all,
    overlap_pct_dataset1 = length(intersection_all) / length(unique_ids1) * 100,
    overlap_pct_dataset2 = length(intersection_all) / length(unique_ids2) * 100,
    overlap_pct_dataset3 = length(intersection_all) / length(unique_ids3) * 100,
    sample_common_all = head(intersection_all, 10),
    sample_only_1 = head(only_in_1, 5),
    sample_only_2 = head(only_in_2, 5),
    sample_only_3 = head(only_in_3, 5)
  )

  return(results)
}
