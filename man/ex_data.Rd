% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ex_data}
\alias{ex_data}
\title{Example dataset}
\format{
\subsection{\code{ex_data}}{

A data frame with 27 rows and 5 columns:
\describe{
\item{sex}{Sex}
\item{age}{Random age groups}
\item{time}{Three years, 2021-2023}
\item{value}{A numeric column holding the actual values, including NA if it exists}
\item{pxvalue}{A character column containing special px values, for example:
. (not applicable), - (nil/exactly zero), ... (DATASYMBOL3), etc.
}
}
}
}
\usage{
ex_data
}
\description{
An example dataset with randomly generated values that can be used to create a px-object.
The data contains three variables (sex, age and time) and two special variables (value and pxvalue)

When a px object is created with this dataset, pxvalue and value are combined into one.
The column value must always exist while pxvalue can be omitted if there aren't any special px values to include.
}
\keyword{datasets}
