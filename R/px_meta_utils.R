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





show_keyword_help <- function(keyword) {
  specs |>
    filter(Keyword == keyword) |>
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

# show_keyword_help("CODEPAGE")





