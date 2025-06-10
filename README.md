
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

