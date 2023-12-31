% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/px_create.R
\name{px_read_meta_csv}
\alias{px_read_meta_csv}
\title{Read metadata csv raw file into a tibble}
\usage{
px_read_meta_csv(path, delim = ";", encoding = "UTF-8")
}
\arguments{
\item{path}{path to csv file}

\item{delim}{delimiter, semicolon by default}

\item{encoding}{utf-8 by default}
}
\value{
returns metadata tibble
}
\description{
This is a wrapper around \code{\link[readr:read_delim]{readr::read_delim()}} which checks that the csv is valid:
}
\details{
\itemize{
\item ; separated
\item must contain the columns: keyword, language, varname, valname and value
}
}
\examples{
try(px_read_meta_csv("metadata.csv"))
try(px_read_meta_csv("inst/extdata/metadata_example.csv"))
}
