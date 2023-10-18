
# todo: add encoding detection in c++
px_extract_meta_from_file <- function(file) {
  dplyr::tibble(dplyr::bind_rows(px_parse_meta_file(file)))
}

#?pxR2::px_parse_meta_string()
# ?px_create()
px_read <- function(file,
                    encoding="guess"
) {

  if (encoding == "guess") {
    guess <- readr::guess_encoding(file)
    encoding <- guess[1,1][[1]]
    print(paste("Guessed encoding:", encoding))
  } else {
    print(paste("Encoding:", encoding))
  }

  meta <- px_extract_meta_from_file(file)

 #  y <- readr::read_file(file = file, locale = readr::locale(encoding = encoding))
 #  y1 <- stringr::str_split_1(y, "DATA=\\r\\n")
 #  meta <- y1[1] # metadata-part
 #
 #
 # # mat <- y1[2] # matrix/data-part

  return(meta)
}

# px_extract_meta_from_file("inst/extdata/WORK02.px")
#
# x <- px_read("inst/extdata/WORK02.px", encoding = "UTF-8")

#
# str(x)
# devtools::load_all()
# px_parse_meta_string(x)



#' x <- px_read("inst/extdata/WORK02.px")
#' x[1]
#'
#' splitted <- str_split_1(x, ";\\r\\n")
#' splitted[1]
#'
#' splitted[68] |>
#'   str_split_1("\",\"")
#'
#'
#'
#' # CELLNOTE: given by values and variable order of STUB and HEADING
#' # DATANOTECELL: given by CODES and variable order of STUB and HEADING

#' # ------------------------------------------------------------------------------
#'
#' # write own parser based on the VB.net parser state machine from PCAxis.Core
#' # https://github.com/statisticssweden/PCAxis.Core/blob/master/PCAxis.Core/Parsers/PXFileParser.vb
#' # https://github.com/mikaelhg/pxspec/tree/master
