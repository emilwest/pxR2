

px_write <- function(.px_object, file) {
  data <- .px_object$data
  meta <- .px_object$metadata
  stubvec <- meta |> get_value_by_keyword("STUB") |> split_commas()
  decimals <- meta |> get_value_by_keyword("DECIMALS") |> as.numeric()

  # extract numbers from df only
  m <- data |>
    dplyr::select(-all_of(stubvec)) |>
    as.matrix()
  colnames(m) <- NULL
  m <- round(m, decimals)

  # one line per row
  datalines <- apply(format(m), 1, paste, collapse=" ")

  # metadata part
  meta_lines <- pull(px_parse_metadata(meta), s)

  # construct px file
  px <- c(meta_lines, "DATA=", datalines, ";")
  readr::write_lines(px, file = file)
}


# px_write(px_obj, file = "test2.px")




