
#
# .px_object <- pxR2::px_read(ex_data)
# .px_object$metadata$metadata <-
#   px_meta_update_value(.px_object$metadata$metadata , "keyword", "MATRIX", "TEST01") |>
#   px_meta_update_value("keyword", "SUBJECT-CODE", "A") |>
#   px_meta_update_value("keyword", "SUBJECT-AREA", "AA") |>
#   px_meta_update_value("keyword", "TITLE", "Hej") |>
#   px_meta_update_value("keyword", "CONTENTS", "Hej") |>
#   px_meta_update_value("keyword", "UNITS", "Number")


#' Write px object to a PX file
#'
#' @param .px_object px object as a list of a metadata tibble with a data tibble.
#' @param file destination file, ending in .px
#'
#' @return saves to file
#' @export
#'
#' @examples
#' \dontrun{
#' px_obj <- px_create(tt,
#' stub = "sex,age",
#' heading = "time",
#' time_variable = "time",
#' time_scale = "annual",
#' matrix = "TEST01",
#' subject_area = "Forestry",
#' subject_code = "F",
#' units = "Number",
#' contents = "Number of trees",
#' decimals = 1,
#' language = "en"
#' )
#'
#' px_write(px_obf, "./test.px")
#' }

px_write <- function(.px_object, file) {
  data <- .px_object$data
  meta_full <- .px_object$metadata
  meta <- meta_full$metadata
  # format data according to stub and heading
  data <- convert_data_to_final(meta, data)


  main_lang <- px_get_main_language(meta)
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
  meta_lines <- px_parse_metadata(meta_full)

  # construct px file
  px <- c(meta_lines, "DATA=", datalines, ";")
  readr::write_lines(px, file = file)
}


# px_write(.px_object, file = "test2.px")

