
# autodict

<!-- badges: start -->
<!-- badges: end -->

The goal of autodict is to recursively search through a
a directory for files (.csv or .xlsx) to collect metadata from
to generate a data dictionary table.

## Installation

You can install the development version of autodict like so:

``` r
# Use the remotes package
remotes::install_github("username/packagename")
```

## Example

This is a basic example which shows you the basic workflow for generating data
dictionaries as CSV files:

``` r
library(autodict)
## Read in your files
df_list <- get_files_from_path(path = "Data/All Data", file_types = c("csv", "xlsx"), keep_dir = TRUE)

## Save data dictionaries (CSV)
save_dict_csv(data = df_list)

## You should have a new folder in your directory titled "Dictionaries"
## This is where your exported data will be!
```

## Workflow overview

This package is designed to gather metadata about data files and export them
as separate files (such as CSV or XLSX) to be used to generate data dictionary
pages. To do so, these are the steps you should take:

### 1) Reading data

The `get_files_from_path()` function takes in a character path and parses
through the directory for files of a specified extension to read in. This
will return a **list of data frames,** with each data frame in the list
containing the data pulled from an individual file.

This list will work seamlessly with saving the data in the next step.

`get_files_from_path()` has three arguments, listed below:

- `path`
  + Default = `"."` (the current working directory)
  + Can be changed to point to a specific subdirectory in your project
- `file_types`
  + Default = `c("csv", "xlsx")`
  + Currently, this package only supports these two file extensions.
  + The data should also already be clean!
- `keep_dir`
  + Default = `FALSE`
  + This argument specifies whether the names of the data frames will contain
  the path leading to the file
  + `AllData/GroupA/First_test.csv` vs. `First_test.csv`
  + If you are reading CSV files, `keep_dir = TRUE` is appropriate
  + If you are reading XLSX files, `keep_dir = FALSE` is appropraite
    - This is because this string will be used to name the worksheets that will
    compose the resulting excel workbook. These names have a limited length, so
    they should be as descriptive to the content as possible!

### 2) Saving data dictionaries

There are two functions in this package that accomplish this step. One exports
CSV files, while the other exports XLSX files.

#### Exporting XLSX files

The XLSX export distinguishes the source data frames by containing each in a
separate worksheet that is part of the workbook as a whole. An example snippet
follows:

``` r
## We previously used get_files_from_path and saved the output to df_list.

save_dict_xlsx(data = df_list, source_path = "Data/AllData")
```

The result of this function will be a new folder "Dictionaries" in your working
directory that contains a single XLSX file, within which each worksheet will be
a table of the metadata of the corresponding data frame (or original file).

#### Exporting CSV files

The CSV export distinguishes the source data frames by nesting each exported
file in a new directory that matches the original directory. As CSV files do not
have many worksheets that can be appended to a single file, they do not have the
luxury of having a single export location. The nested directories should be
sufficient in catching any files that have redundant file naming schemes. An
example snippet follows:

``` r
## We will again use the df_list that we got from get_files_from_path

save_dict_csv(data = df_list)
```

Notice that this function call is missing the `source_path` argument that the
other version has. Why is that? Simply put, it's information that this function
doesn't use. As each data frame from `df_list` is being saved as its own file,
in a directory that mirrors its source location, it is unnecessary to use
`source_path` 
