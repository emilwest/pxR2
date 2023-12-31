% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{specs}
\alias{specs}
\title{PX format specification}
\format{
\subsection{\code{specs}}{

A data frame with 86 rows and 12 columns:
\describe{
\item{Keyword}{PX keyword in uppercase.}
\item{Mandatory}{TRUE/FALSE. Indicates if keyword must be included in PX-file.}
\item{Multiplicity}{How many times the keyword can occur.}
\item{Introduction_year}{When keyword was introduced.}
\item{Language_dependent}{TRUE/FALSE. Whether the keyword can be translated for multiple languages.}
\item{Default_value}{The default value used.}
\item{Quoted}{Whether the variable value should/need to be enclosed in quotes or not.}
\item{Type}{The value type for the keyword, for example integer, text, etc.}
\item{Multiline}{Whether a keyword can span over multiple lines.}
\item{Multivalue}{Whether a keyword can have more than one value.}
\item{Note}{Description of keyword.}
}
}
}
\source{
\url{https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf}
}
\usage{
specs
}
\description{
A dataset describing the PX format file specification.
It is sorted in the recommended keyword order of appearance.

This dataset contains additional metadata, like whether a keyword allows for multiple values
or if it can be quoted.
}
\keyword{datasets}
