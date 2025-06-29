% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_helpers.R
\name{extract_data_type}
\alias{extract_data_type}
\title{After determining data needs, update function to manually set data types.
currently dependent on the saved data type of columns in the data frame,
which upon reading, is currently overridden as character.}
\usage{
extract_data_type(df, col_name)
}
\arguments{
\item{df}{The dataframe to examine}

\item{col_name}{The name of the column to examine}
}
\value{
A character string that describes the datatype of the column
}
\description{
After determining data needs, update function to manually set data types.
currently dependent on the saved data type of columns in the data frame,
which upon reading, is currently overridden as character.
}
