# utility functions around metadata tibble



sort_metadata_keywords <- function(.metadata_df) {
  sortorder <- specs$Keyword
  .metadata_df |>
    mutate(keyword = factor(keyword, levels = sortorder)) |>
    arrange(keyword)
}

