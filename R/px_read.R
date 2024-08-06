
# helper
px_get_variables <- function(.meta_df) {
  stub <- get_value_by_keyword(px$metadata$metadata, "STUB")
  heading <- get_value_by_keyword(px$metadata$metadata, "HEADING")
  x <- c(stub, heading)
  x[!is.na(x)]
}



px_add_values_from_data <- function(.metadata_df, .data) {
  vals_to_add <- px_get_values_from_data(.data)
  px_meta_compare_varnames(.metadata_df_new = vals_to_add, .metadata_df = .metadata_df)

  .metadata_df %>%
    dplyr::rows_insert(vals_to_add, by = c("keyword","language", "varname", "valname"))
}

# px_add_values_from_data(new_meta, ex_data)


is_expected_number_of_levels <- function(.data, silent = TRUE) {
  special_cols <- c("value", "pxvalue")
  expected_lengths <- .data |>
    dplyr::select(-any_of(special_cols)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ as.character(.x) |> as.factor())) |>
    purrr::map(levels) |>
    purrr::map(length) |>
    base::unlist()

  n_expected <- prod(expected_lengths)
  actual <- nrow(.data)

  detaljer <- expected_lengths |>
    tibble::enframe() |>
    stringr::str_glue_data("({name} = {value})") |>
    stringr::str_c(collapse = " \u00d7 ") |>
    stringr::str_c(" = ", n_expected)

  if (n_expected != actual) {
    if (!silent) {
      warning(stringr::str_glue("Number of rows does not match expected number based on the combination of all factor levels.
                  Expected: {n_expected} [{detaljer}]
                  Actual: {actual}
                  "))
    }
    return(FALSE)
  } else {
    if (!silent) {
      print(stringr::str_glue("Success. Number of rows matches expected number based on the combination of all factor levels.
                  Expected: {n_expected} [{detaljer}]
                  Actual: {actual}
                  "))
    }
    return(TRUE)
  }
}

complete_missing_levels_in_data <- function(.data) {
  special_cols <- c("value", "pxvalue")

  not_value <- setdiff(colnames(.data), special_cols)

  ej_faktorer <- purrr::map(.data, is.factor) |> purrr::keep(~ .x == FALSE) |> names()
  ej_faktorer <- setdiff(ej_faktorer, special_cols)

  if (length(ej_faktorer) > 0) {
    .data <- .data |>
      dplyr::mutate(dplyr::across(all_of(ej_faktorer), factor))
  }

  res <- .data |>
    tidyr::complete(!!!dplyr::syms(not_value)) |>
    dplyr::group_by(dplyr::across(-value)) |>
    # sum repeating categories
    dplyr::summarise(value = sum(value, na.rm = TRUE)) |>
    dplyr::ungroup()

  if (!is_expected_number_of_levels(res, silent = F)) {
    warning("Completing missing levels in data failed, double check the data and complete manually")
  }

  return(res)
}


# todo: add encoding detection in c++
px_extract_meta_from_file <- function(file) {
  dplyr::tibble(dplyr::bind_rows(px_parse_meta_file(file)))
}

#?pxR2::px_parse_meta_string()
# ?px_create()




init_empty_px_object <- function() {
  metadata <- list(
    metadata = px_meta_init_empty(),
    variable_codes = dplyr::tibble(variablecode=character(),
                                   language=character(),
                                   variabletype=character(),
                                   variable=character()
                                   ),
    value_codes = dplyr::tibble(variablecode=character(),
                                language=character(),
                                value=character(),
                                code=character()
    )
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

#' Create px object from data frame
#'
#' @param .data data frame or tibble
#'
#' @return returns a px object of class pxR2, ie a list of dataframes containing metadata and data
#'
#' @examples
#' pxR2::create_px_object_from_dataframe(ex_data)
#'
#' @keywords internal
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



  # add values from data
  vals_to_add <- px_get_values_from_data(.data)
  px_meta_compare_varnames(.metadata_df_new = vals_to_add, .metadata_df =  px$metadata$metadata)

  px$metadata$metadata <- px$metadata$metadata |>
    dplyr::rows_insert(vals_to_add, by = c("keyword","language", "varname", "valname"))

  #px$metadata$metadata <- pxR2::px_add_values_from_data(.metadata_df = px$metadata$metadata, .data = .data)

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


  # variable codes are constructed from available variables in stub/heading.
  # when creating from a dataframe the first time,
  # the variablecode and and variable label are the same.
  px$metadata$variable_codes <- px$metadata$variable_codes |>
    dplyr::bind_rows(
      available_colnames |>
        tibble::enframe(value = "variablecode", name="language") |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character),
                      language=NA,
                      variable=variablecode)

    )

  # generate value codes, by default codes = values.
  px$metadata$value_codes <- px$metadata$value_codes |>
    dplyr::bind_rows(
      px$data |>
        dplyr::select(dplyr::all_of(available_colnames)) |>
        purrr::map_if(.p = is.factor, .f = levels, .else = unique) |>
        purrr::map(as.character) |>
        tibble::enframe(name = "variablecode", value = "value") |>
        tidyr::unnest(value) |>
        mutate(code = value)
    )

  # compare all values in value_codes to number of rows in data.
  # since the px data is a cube with all combinations of variable-values,
  # the number of rows in data must match the product of all variable values.
  if (!is_expected_number_of_levels(px$data)) {
    print(stringr::str_glue("Detected missing levels in data, completing missing levels."))
    px$data <- complete_missing_levels_in_data(px$data)
  }
  px
}




# create_px_object_from_dataframe(ex_data)
#
# px <- dplyr::storms |>
#   select(name, year, status) |>
#   distinct() |>
#   count(status, year, name = "value") |>
#   arrange(year, status) |>
#   create_px_object_from_dataframe()




is_px_file <- function(input) {
  assertthat::assert_that(is.character(input))
  return(tolower(stringr::str_sub(input, start = -2)) == "px")
}

create_px_object_from_px_file <- function(file,
                                          encoding="guess",
                                          only_meta=FALSE,
                                          debug=FALSE
                                          ) {


  if (encoding == "guess") {
    guess <- readr::guess_encoding(file)
    encoding <- guess[1,1][[1]]
    print(paste("Guessed encoding:", encoding))
  } else {
    print(paste("Encoding:", encoding))
  }


  px <- px_parse(file, only_meta, debug)
  meta <- px$meta |>
    dplyr::bind_rows() |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                \(x) iconv(x, from = encoding, to = "UTF-8"))) |>
    # convert to list-vectors
    dplyr::mutate(subkeys = map(subkeys, unsplitlist),
           values = map(values, unsplitlist)
    ) |>
    tidyr::unnest_wider(subkeys, names_sep = "_")

  # todo om subkeys fortsätter utöver 2 måste detta fixas
  names(meta) <- c("keyword", "language", "varname", "valname", "value")

  tval <- get_value_by_keyword(meta, "TIMEVAL")[[1]]

  # extrahera ut timeval till subkey
  # todo fixa detta i metaparsern?
  if (length(tval) > 1) {
    tval_get <- get_value_by_keyword(meta, "TIMEVAL")[[1]]
    tval <- tval_get[1] |>
      # (?<=\\(): Asserts that the match must be preceded by an opening parenthesis (.
      # .+?: Matches any character (except a newline) one or more times, but as few times as possible (non-greedy).
      # (?=\\)): Asserts that the match must be followed by a closing parenthesis ).
      str_extract("(?<=\\().+?(?=\\))")


    meta[meta$keyword=="TIMEVAL", ]$valname <- tval
    meta[meta$keyword=="TIMEVAL", ]$value[[1]] <- tval_get[-1]
  }


  # ----------------------------------------------------------------------------
  # VARIABLE CODES
  variable_codes <- meta |>
    dplyr::filter(keyword == "VARIABLECODE")

  if (nrow(variable_codes) > 0) {
    variable_codes <- variable_codes |>
      dplyr::select(variablecode = value, language=language, variable=varname) |>
      tidyr::unnest(variablecode) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  } else {
    # TODO fixa multilingual
    varnames <- c(get_value_by_keyword(xx, "STUB"), get_value_by_keyword(xx, "HEADING")) |>
      unlist()

    # if variablecode does not exist, it gets the same value as the variable label
    variable_codes <- tibble::enframe(varnames) |>
      mdplyr::mutate(variablecode = value, language = NA, variable = variablecode) |>
      dplyr::select(variablecode, language, variable) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  }


  # check if variable-type exists
  variabletype_df <- meta |>
    filter(keyword == "VARIABLE-TYPE")
  if (nrow(variabletype_df) > 0) {
    variabletype_df <- variabletype_df |>
      tidyr::unnest(value) |>
      dplyr::select(variable = varname, variabletype = value)
    variable_codes <- variable_codes |>
      dplyr::left_join(variabletype_df, by = join_by(variable))


    missing_variabletypes <- variable_codes[is.na(variable_codes$variabletype),]
    if (nrow(missing_variabletypes) > 0) {

      warningtext <- missing_variabletypes |>
        stringr::str_glue_data("VARIABLECODE: {variablecode}. Variable label: {variable}.
                             Consider adding VARIABLE-TYPE(\"{variablecode}\")=\"value\"; where value is one of V, T, C or G.
                             ")
      warning(
        str_glue("Detected missing VARIABLE-TYPE for the following variable:
             {warningtext}
             V=value, T=time, C=contents, G=geographical. See official PX documentation for more info.
             ")
      )

    }
  } else {
    variable_codes <- variable_codes |>
      dplyr::mutate(variabletype = NA)
  }
  variable_codes <- variable_codes |>
    dplyr::select(variablecode, language, variabletype, variable)

  # ----------------------------------------------------------------------------
  # VALUE CODES

  value_codes <- meta |>
    dplyr::filter(keyword %in% c("CODES", "VALUES")) |>
    tidyr::pivot_wider(names_from=keyword) |>
    dplyr::left_join(variable_codes, by = join_by(varname == variable, language)) |>
    dplyr::select(variablecode, language, value=VALUES, code=CODES) |>
    dplyr::distinct() |>
    tidyr::unnest(cols = c(code, value)) |>
    distinct()




  px_obj <- init_empty_px_object()
  px_obj$metadata$metadata <- meta
  px_obj$metadata$variable_codes <- variable_codes
  px_obj$metadata$value_codes <- value_codes
  px_obj$data <- px$datavec #TODO


  return(px_obj)
}

#
xx <- create_px_object_from_px_file("inst/extdata/TEMP02.px")
#xx <- create_px_object_from_px_file("inst/extdata/WORK02.px")$meta
#xx <- create_px_object_from_px_file("\\\\ivo.local\\Users\\Home$\\emwe\\Downloads\\scb.px")$meta

init_empty_px_object()$metadata$value_codes
init_empty_px_object()$metadata$variable_codes


# variablecode <chr>, language <chr>, variabletype <chr>, variable <chr>
# variablecode <chr>, language <chr>, value <chr>, code <chr>
# value_codes <- xx$metadata$metadata |>
#   dplyr::filter(keyword %in% c("CODES", "VALUES")) |>
#   tidyr::pivot_wider(names_from=keyword) |>
#   dplyr::left_join(xx$metadata$variable_codes, by = join_by(varname == variable, language)) |>
#   dplyr::select(variablecode, language, value=VALUES, code=CODES) |>
#   dplyr::distinct() |>
#   tidyr::unnest(cols = c(code, value)) |>
#   distinct()





#' Create px object from dataframe or px file
#'
#' @param input either a datarframe/tibble or a path to a px file ending in `.px` or `.PX`
#'
#' @return returns a px object of class pxR2
#' @export
#'
px_read <- function(input) {

  if (is.data.frame(input)) {
    create_px_object_from_dataframe(input)
  } else if (is_px_file(input)) {
    # create_px_object_from_px_file(input) #todo
  } else {
    stop("Please provide input as either a dataframe/tibble or a path to a px-file ending in .px or .PX")
  }

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
