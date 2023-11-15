# utility functions around metadata tibble



#' Sorts metadata keywords according to recommended sort order according to
#' PX format specification.
#'
#' @param .metadata_df Metadata tibble
#'
#' @return Returns sorted metadata tibble by keyword
#' @export
#'
#' @examples
#' sort_metadata_keywords(meta_example)
sort_metadata_keywords <- function(.metadata_df) {
  sortorder <- specs$Keyword
  .metadata_df |>
    mutate(keyword = factor(keyword, levels = sortorder)) |>
    arrange(keyword)
}





#' Show keyword help information
#'
#' This function provides details on a keyword according to the PX-file format specification and reads from `specs`.
#' It displays:
#'
#' - is the keyword mandatory/language dependent?
#' - can values of the keyword span over multiple lines? (Multiline)
#' - type (text/integer/other)
#' - max character length allowed for values
#' - default value (if any)
#' - a keyword description
#'
#' @param keyword PX keyword
#'
#' @return returns formatted string of help information
#' @export
#'
#' @examples
#' show_keyword_help("TIMEVAL")
#' show_keyword_help("NOTE")
#' show_keyword_help("CELLNOTE")
#' show_keyword_help("LANGUAGE")
#' show_keyword_help("CODES")
#' show_keyword_help("DATA")
#'
#' @seealso [specs] for PX file format specification tibble
#' or <https://www.scb.se/globalassets/vara-tjanster/px-programmen/px-file_format_specification_2013.pdf> for original documentation by Statistics Sweden.
show_keyword_help <- function(keyword) {
  specs |>
    filter(Keyword == toupper(keyword)) |>
    str_glue_data(
      "Keyword: {Keyword} ({ifelse(Mandatory, 'Mandatory', 'Not mandatory')}, {ifelse(Language_dependent, 'Language dependent', 'Not language dependent')})",
      "\nType: {Type}",
      "\nMultiline: {Multiline}",
      "\nMax length: {Length}",
      "\nDefault value: {Default_value}",
      "\n-----------------------------",
      "\nKeyword description:",
      "\n\n{Note}"
    )
}


