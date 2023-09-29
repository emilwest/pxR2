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

