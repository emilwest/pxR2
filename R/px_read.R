



# todo: add encoding detection in c++
px_extract_meta_from_file <- function(file) {
  dplyr::tibble(dplyr::bind_rows(px_parse_meta_file(file)))
}

#?pxR2::px_parse_meta_string()
# ?px_create()




init_empty_px_object <- function() {
  metadata <- list(
    metadata = px_meta_init_empty(),
    variable_codes = tibble::tibble(),
    value_codes = tibble::tibble()
  )

  px_obj <- list(metadata = metadata, data = tibble::tibble())
  structure(px_obj, class = "pxR2")
}



# Creates empty mandatory variables if they dont exist
init_mandatory_vars <- function() {
  m <- px_meta_init_empty()

  mandatory_vars <- pxR2::specs %>%
    dplyr::filter(Mandatory == TRUE) %>%
    dplyr::filter(Keyword != "DATA") %>%
    dplyr::filter(Keyword != "VALUES") %>%
    dplyr::select(keyword=Keyword)

  m <- m |>
    bind_rows(mandatory_vars)
  m[m$keyword=="DECIMALS", 5] <- "1"

  return(m)
}


# .data <- ex_data

pxR2::meta_example

create_px_object_from_dataframe <- function(.data) {
  assertthat::assert_that(is.data.frame(.data))
  assertthat::assert_that(any(names(.data) %in% "value"))

  px <- init_empty_px_object()
  px$metadata$metadata <- init_mandatory_vars()
  px$data <- .data

  special_cols <- c("value", "pxvalue")
  available_colnames <- setdiff(colnames(px$data), special_cols)

  if (length(available_colnames) > 1) {
    # split available colnames into two chunks by no special order
    stubheading <- split(available_colnames, cut(seq_along(available_colnames), 2, labels = FALSE))

    stub <- str_c(stubheading[[1]], collapse = ",")
    heading <- str_c(stubheading[[2]], collapse = ",")

    px$metadata$metadata[px$metadata$metadata$keyword=="STUB", 5] <- stub
    px$metadata$metadata[px$metadata$metadata$keyword=="HEADING", 5] <- heading
  } else if (length(available_colnames) == 1) {
    px$metadata$metadata[px$metadata$metadata$keyword=="STUB", 5] <- available_colnames
  } else {
    stop("At least one variable is needed for STUB or HEADING.")
  }

  px$metadata$metadata <- px_add_values_from_data(.data = .data, .metadata_df = px$metadata$metadata)

  # add other default keywords
  px$metadata$metadata <- px$metadata$metadata |>
    px_meta_add_keyword("SHOWDECIMALS", value = as.character(1)) |>
    px_meta_add_keyword("LANGUAGE", value = "en") |>
    px_meta_add_keyword("CHARSET", value = "ANSI") |>
    px_meta_add_keyword("AXIS-VERSION", value = "2013") |>
    px_meta_add_keyword("CODEPAGE", value = "UTF-8") |>
    px_meta_add_keyword("CREATION-DATE", value =  format(Sys.time(), "%Y%m%d %H:%M")) |>
    px_meta_add_keyword("LAST-UPDATED", value = format(Sys.time(), "%Y%m%d %H:%M")) |>
    sort_metadata_keywords()



  px
}

create_px_object_from_dataframe(ex_data)

dplyr::storms |>
  select(name, year, status) |>
  distinct() |>
  count(status, year, name = "value") |>
  arrange(year, status) |>
  create_px_object_from_dataframe()



is_px_file <- function(input) {
  assertthat::assert_that(is.character(input))
  return(tolower(stringr::str_sub(input, start = -2)) == "px")
}

create_px_object_from_px_file <- function(file,
                                          encoding="guess") {


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


px_read <- function(input) {

  if (is.data.frame(input)) {
    create_px_object_from_dataframe()
  } else if (is_px_file(input)) {
    # create_px_object_from_px_file(input) #todo
  } else {
    stop("Please provide input as either a dataframe/tibble or a path to a px-file ending in .px or .PX")
  }

  structure(px_obj, class = "pxR2")
}

#
# px_read(tibble::tibble())
#
# is_px_file("hej.px")
# is_px_file(3)


#'
#' # CELLNOTE: given by values and variable order of STUB and HEADING
#' # DATANOTECELL: given by CODES and variable order of STUB and HEADING

#' # ------------------------------------------------------------------------------
#'
#' # write own parser based on the VB.net parser state machine from PCAxis.Core
#' # https://github.com/statisticssweden/PCAxis.Core/blob/master/PCAxis.Core/Parsers/PXFileParser.vb
#' # https://github.com/mikaelhg/pxspec/tree/master
