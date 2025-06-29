% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_quarto.R
\name{generate_pages}
\alias{generate_pages}
\title{A batch-processing extension of \code{generate_page()}. Instead of a single data
frame, this function takes in a directory path and parses through it
recursively, generating data dictionary Quarto pages for each metadata file.}
\usage{
generate_pages(
  input_dir,
  output_dir,
  render = FALSE,
  metadata_format = c("csv", "xlsx"),
  keep_dir = TRUE
)
}
\arguments{
\item{input_dir}{input path to parse through}

\item{output_dir}{Desired destination folder for all pages}

\item{render}{render quarto or not (if FALSE, only generates the quarto
markdown file)}

\item{metadata_format}{the format of the files being read}

\item{keep_dir}{whether to keep the file path in the name of dfs}
}
\value{
nothing - generates quarto pages
}
\description{
A batch-processing extension of \code{generate_page()}. Instead of a single data
frame, this function takes in a directory path and parses through it
recursively, generating data dictionary Quarto pages for each metadata file.
}
