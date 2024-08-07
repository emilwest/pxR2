% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/px_parse_metadata.R
\name{splitlist}
\alias{splitlist}
\title{Helper for metadata parser, splits a string to vector, adds quotes and joins back with commas.}
\usage{
splitlist(txt)
}
\arguments{
\item{txt}{Text vector with comma-separated values, e.g. '2021,2022,2023'}
}
\value{
Returns texts vector with added quotes around values, e.g '"\"2021\",\"2022\",\"2023\""'
}
\description{
Helper for metadata parser, splits a string to vector, adds quotes and joins back with commas.
}
\seealso{
\code{\link[=addquotes]{addquotes()}}, \code{\link[=px_parse_metadata]{px_parse_metadata()}}
}
\keyword{internal}
