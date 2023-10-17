

#' Write px object to a PX file
#'
#' @param .px_object px object as a list of a metadata tibble with a data tibble.
#' @param file destination file, ending in .px
#'
#' @return saves to file
#' @export
#'
#' @examples
px_write <- function(.px_object, file) {
  data <- .px_object$data
  meta <- .px_object$metadata
  # format data according to stub and heading
  data <- convert_data_to_final(meta, data)

  main_lang <- meta |> get_value_by_keyword("LANGUAGE")
  x <- meta |> get_value_by_keyword("STUB")
  if (length(x) > 1) {
    stubvec <- x[main_lang] |> split_commas()
  } else {
    stubvec <- x[1] |> split_commas()
  }



  # extract numbers from df only
  m <- data |>
    dplyr::select(-all_of(stubvec)) |>
    as.matrix()
  colnames(m) <- NULL
  m <- ifelse(is.na(m), addquotes(".."), m)

  # one line per row
  datalines <- apply(format(m), 1, paste, collapse=" ")

  # metadata part
  meta_lines <- pull(px_parse_metadata(meta), s)

  # construct px file
  px <- c(meta_lines, "DATA=", datalines, ";")
  readr::write_lines(px, file = file)
}


# px_write(px_obj, file = "test2.px")




