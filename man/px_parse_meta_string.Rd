% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{px_parse_meta_string}
\alias{px_parse_meta_string}
\title{Parse a single px-formatted string.}
\usage{
px_parse_meta_string(line, debug = FALSE)
}
\arguments{
\item{line}{A string in PX-format with at least a keyword and value.}

\item{debug}{Boolean, set to true if you want to debug the parser step-by-step.}
}
\value{
Returns a parsed list with four elements: keyword, language, subkeys and values.
}
\description{
This function parses a string in px-format and returns an error if the format
is malformatted.
}
\examples{
px_parse_meta_string("VALUENOTE[sv](\"Norway\")=\"Break in time series\";")
px_parse_meta_string("NOTE=\"Preliminary data\";")
px_parse_meta_string("CODEPAGE=\"UTF-8\";")
}
