% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/px_meta_utils.R
\name{sort_metadata_keywords}
\alias{sort_metadata_keywords}
\title{Sorts metadata keywords according to recommended sort order according to
PX format specification.}
\usage{
sort_metadata_keywords(.metadata_df)
}
\arguments{
\item{.metadata_df}{Metadata tibble}
}
\value{
Returns sorted metadata tibble by keyword
}
\description{
Sorts metadata keywords according to recommended sort order according to
PX format specification.
}
\examples{
sort_metadata_keywords(meta_example)
}
