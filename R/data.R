
#' Example dataset
#'
#' @description
#' An example dataset with randomly generated values that can be used to create a px-object.
#' The data contains three variables (sex, age and time) and two special variables (value and pxvalue)
#'
#' When a px object is created with this dataset, pxvalue and value are combined into one.
#' The column value must always exist while pxvalue can be omitted if there aren't any special px values to include.
#'
#'
#' @format ## `ex_data`
#' A data frame with 27 rows and 5 columns:
#' \describe{
#'   \item{sex}{Sex}
#'   \item{age}{Random age groups}
#'   \item{time}{Three years, 2021-2023}
#'   \item{value}{A numeric column holding the actual values, including NA if it exists}
#'   \item{pxvalue}{A character column containing special px values, for example:
#'   . (not applicable), - (nil/exactly zero), ... (DATASYMBOL3), etc.
#'   }
#' }
"ex_data"



#' Example metadata tibble
#'
#' @description
#' An example metadata tibble needed for creating a px-object.
#' The metadata tibble accepts all PX keywords according to the ´specs´ except for
#' CELLNOTE(X) and DATANOTECELL.
#'
#' For more information on available px keyword, check out the dataset `specs` or
#' `show_keyword_help(keyword)`.
#'
#'
#' @format ## `meta_example`
#' A data frame with 18 rows and 5 columns:
#' \describe{
#'   \item{keyword}{PX keyword in uppercase.}
#'   \item{language}{Language. If only one language exists it can be omitted,
#'   otherwise language should be defined where a translation is needed.}
#'   \item{varname}{The variable name. Used if a keyword refers to a specific variable.
#'   For example, NOTE("age") is a footnote for the varname "age".}
#'   \item{valname}{The value name. Used if a keyword refers to a specific variable value,
#'   for example VALUENOTE("age","0-15") is a footnote for the age group 0-15 years.}
#'   \item{value}{The value to show for each keyword.}
#' }
"meta_example"


#' Example multilingual metadata tibble
#'
#' @description
#' An example metadata tibble needed for creating a px-object.
#' This data contains two languages: English and Swedish.
#' The default language is English as defined in LANGUAGE.
#' Swedish is defined in LANGUAGES after English.
#'
#' A csv file with ;-delimiters can also be used to represent the same information as this tibble.
#' Note that TIMEVAL value is empty since these values will be filled in dynamically from a dataframe.
#' VALUES are also dynamically generated from a dataframe in `px_create`.
#'
#'
#' The metadata tibble accepts all PX keywords according to the ´specs´ except for
#' CELLNOTE(X) and DATANOTECELL.
#'
#' For more information on available px keyword, check out the dataset `specs` or
#' `show_keyword_help(keyword)`.
#'
#'
#' @format ## `metadata_example_multilingual`
#' A data frame with 33 rows and 5 columns:
#' \describe{
#'   \item{keyword}{PX keyword in uppercase.}
#'   \item{language}{Language. If only one language exists it can be omitted,
#'   otherwise language should be defined where a translation is needed.}
#'   \item{varname}{The variable name. Used if a keyword refers to a specific variable.
#'   For example, NOTE("age") is a footnote for the varname "age".}
#'   \item{valname}{The value name. Used if a keyword refers to a specific variable value,
#'   for example VALUENOTE("age","0-15") is a footnote for the age group 0-15 years.}
#'   \item{value}{The value to show for each keyword.}
#' }
"metadata_example_multilingual"



#' PX format specification
#'
#' @description
#' A dataset describing the PX format file specification.
#' It is sorted in the recommended keyword order of appearance.
#'
#' This dataset contains additional metadata, like whether a keyword allows for multiple values
#' or if it can be quoted.
#'
#'
#'
#' @format ## `specs`
#' A data frame with 86 rows and 12 columns:
#' \describe{
#'   \item{Keyword}{PX keyword in uppercase.}
#'   \item{Mandatory}{TRUE/FALSE. Indicates if keyword must be included in PX-file.}
#'   \item{Multiplicity}{How many times the keyword can occur.}
#'   \item{Introduction_year}{When keyword was introduced.}
#'   \item{Language_dependent}{TRUE/FALSE. Whether the keyword can be translated for multiple languages.}
#'   \item{Default_value}{The default value used.}
#'   \item{Quoted}{Whether the variable value should/need to be enclosed in quotes or not.}
#'   \item{Type}{The value type for the keyword, for example integer, text, etc.}
#'   \item{Multiline}{Whether a keyword can span over multiple lines.}
#'   \item{Multivalue}{Whether a keyword can have more than one value.}
#'   \item{Note}{Description of keyword.}
#' }
#' @source <https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf>
#' @export
"specs"

