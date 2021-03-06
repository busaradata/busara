% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decorate_table.R
\name{decorate_table}
\alias{decorate_table}
\title{A function to display level of missingness in a data frame provided}
\usage{
decorate_table(tab, col_names = col_names)
}
\arguments{
\item{tab}{A data frame or tibble table to be displayed on markdown}

\item{col_names}{A vector of column names as expected to be displayed}
}
\description{
A function to display level of missingness in a data frame provided
}
\examples{
decorate_table(head(mtcars), names(mtcars))

}
