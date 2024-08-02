

#' Helper for metadata parser, adds quotes around values in a string
#'
#' @param txt Text string to add quotes around
#'
#' @return Text string with added quotes
#'
#' @seealso [splitlist()] for adding quotes to each element in a vector and joining to single string, and
#'  [px_parse_metadata()] for parsing metadata to text
#' @keywords internal
addquotes <- function(txt) {
  Q <- '"'
  stringr::str_c(Q, txt, Q)
}


#' Helper for metadata parser, splits a string to vector, adds quotes and joins back with commas.
#'
#' @param txt Text vector with comma-separated values, e.g. '2021,2022,2023'
#'
#' @return Returns texts vector with added quotes around values, e.g '"\"2021\",\"2022\",\"2023\""'
#'
#' @seealso [addquotes()], [px_parse_metadata()]
#' @keywords internal
splitlist <- function(txt) {
  txt[is.na(txt)] <- NA_character_
  assertthat::assert_that(is.character(txt), msg = "Input must be a character vector")
  assertthat::assert_that(length(txt)>0, msg = "Length of charcter vector must be at least one")

  splitta <- function(x) {
    if (!is.na(x)) {
      stringr::str_split_1(x, ",") |>
        addquotes() |>
        stringr::str_c(collapse=",")
    } else {
      return(NA_character_)
    }
  }
  unlist(map(txt, splitta))
}


# .metadata <- .px_object$metadata

#' Parse px metadata to px-formatted text.
#'
#' @param .metadata Metadata as input to be converted to px-formatted text.
#' @param full_dataframe If TRUE, return entire dataframe. Useful for debugging. Default FALSE.
#'
#' @return Px-formatted text
#' @keywords internal
px_parse_metadata <- function(.metadata, full_dataframe = FALSE) {

  Q <- '"'
  E <- ";"
  e <- "="
  comma <- ","
  oq <- "("
  eq <- ")"
  b1 <- "["
  b2 <- "]"

  .metadata_df <- .metadata$metadata
  variable_codes <- .metadata$variable_codes
  value_codes <- .metadata$value_codes

  # check if any values are empty
  na_values <- is.na(.metadata_df$value)
  if (any(na_values)) {
    k <- as.character(.metadata_df[na_values,]$keyword)
    k_mandatory <- k[is_mandatory(k)]
    k_other <- k[!is_mandatory(k)]

    error_message <- "The following keywords have missing values, please add values to them in metadata first and try again.:"
    if (length(k_mandatory)>0) {
      error_message <- stringr::str_c(error_message, 'Mandatory: {stringr::str_c(k_mandatory, collapse = ", ")}.', sep = "\n")
    }
    if (length(k_other)>0) {
      error_message <- stringr::str_c(error_message, 'Other keywords: {stringr::str_c(k_other, collapse = ", ")}.', sep = "\n")
    }

    stop(stringr::str_glue(error_message))
  }

  main_lang <- px_get_main_language(.metadata_df)
  langs <- px_get_languages(.metadata_df)

  tmp <- .metadata_df |>
    sort_metadata_keywords() |>
    dplyr::mutate(language = factor(language, levels = langs)) |>
    dplyr::arrange(keyword, language)

  tmp <- tmp |>
    dplyr::left_join(specs |> dplyr::select(Keyword, Multivalue, Multiline, Quoted), by = c("keyword" = "Keyword")) |>
    dplyr::mutate(value_parsed_tmp = ifelse(Multivalue == TRUE & keyword != "VALUES",
                                            splitlist(value), value),
                  value_parsed = ifelse(Multivalue == FALSE & keyword != "VALUES" & Quoted == TRUE, addquotes(value_parsed_tmp), value_parsed_tmp)
    ) |>
    dplyr::select(-value_parsed_tmp) |>
    # KEYWORD[lang] or KEYWORD. KEYWORD[lang] only necessary if its not the main language
    dplyr::mutate(keyword_parsed = ifelse(!is.na(language) & language != main_lang,
                                          stringr::str_c(keyword, b1, language, b2),
                                          keyword)) |>
    # CHARSET="ANSI";
    dplyr::mutate(s = ifelse(is.na(varname) & is.na(valname),
                             stringr::str_c(keyword_parsed, e, value_parsed, E),
                             keyword_parsed
    )) |>
    dplyr::mutate(
      # NOTE("age")="...";
      s = ifelse(!is.na(varname) & is.na(valname),
                 stringr::str_c(keyword_parsed, oq, Q, varname, Q, eq, e, value_parsed, E),
                 s

      )) |>
    dplyr::mutate(
      # VALUENOTE("age", "0-15")="...";
      s = ifelse(!is.na(varname) & !is.na(valname),
                 stringr::str_c(keyword_parsed, oq, Q, varname, Q, comma, Q, valname, Q, eq, e, value_parsed, E),
                 s
      )
    ) |>
    dplyr::mutate(
      # TIMEVAL(”time”)=TLIST(A1), ”1994”, ”1995”,"1996”
      s = ifelse(keyword == "TIMEVAL" & language == main_lang,
                 stringr::str_c(keyword_parsed, oq, Q, varname, Q, eq, "=TLIST(", valname, "), ", value_parsed, E),
                 s

      )) |>
    # TIMEVAL can only occur once, only for main lang
    dplyr::filter(!(keyword == "TIMEVAL" & language != main_lang))


  # parse variable_codes
  variable_codes_parsed <- variable_codes |>
    dplyr::mutate(keyword = "VARIABLECODE") |>
    # VARIABLECODE[lang] only necessary if its not the main language
    dplyr::mutate(keyword_parsed = ifelse(!is.na(language) & language != main_lang,
                                          stringr::str_c(keyword, b1, language, b2),
                                          keyword)) |>
    dplyr::mutate(
      # VARIABLECODE("variable")="variablecode";
      s = stringr::str_c(keyword_parsed, oq, Q, variable, Q, eq, e, Q, variablecode, Q, E)
      ) |>
    dplyr::select(keyword, s)

  # parse CODES
  value_codes_parsed <- value_codes |>
    dplyr::mutate(keyword = "CODES") |>
    dplyr::left_join(variable_codes, by = dplyr::join_by(variablecode, language)) |>
    dplyr::group_by(keyword, variablecode, language, variable) |>
    dplyr::summarise(code = stringr::str_c(addquotes(code), collapse = ",")) |>
    dplyr::ungroup() |>
    # CODES[lang] only necessary if its not the main language
    dplyr::mutate(keyword_parsed = ifelse(!is.na(language) & language != main_lang,
                                          stringr::str_c(keyword, b1, language, b2),
                                          keyword)) |>
    dplyr::mutate(
      # CODES("variable")="code1","code2", "...", "codeN";
      s = stringr::str_c(keyword_parsed, oq, Q, variable, Q, eq, e, code, E)
    ) |>
    dplyr::select(keyword, s)


  # finally, sort keywords
  tmp <- tmp |>
    dplyr::bind_rows(variable_codes_parsed) |>
    dplyr::bind_rows(value_codes_parsed) |>
    sort_metadata_keywords()


  if (full_dataframe) {
    tmp
  } else {
    tmp$s
  }
}

#
# #
# # View(px_parse_metadata(metadata_example_multilingual))
# Q <- '"'
# E <- ";"
# e <- "="
# comma <- ","
# oq <- "("
# eq <- ")"
# b1 <- "["
# b2 <- "]"
# metadata_example_multilingual |>
#   dplyr::left_join(specs |> dplyr::select(Keyword, Multivalue, Quoted), by = c("keyword" = "Keyword")) |>
#   filter(Multivalue == TRUE & keyword != "VALUES") |>
#   pull(value) |>
#   splitlist()
#
#
# metadata_example_multilingual |>
#   sort_metadata_keywords() |>
#   dplyr::left_join(specs |> dplyr::select(Keyword, Multivalue, Multiline, Quoted), by = c("keyword" = "Keyword")) |>
#   dplyr::mutate(value_parsed_tmp = ifelse(Multivalue == TRUE & keyword != "VALUES",
#                                           splitlist(value), value),
#                 value_parsed = ifelse(Multivalue == FALSE & keyword != "VALUES" & Quoted == TRUE, addquotes(value_parsed_tmp), value_parsed_tmp)
#   ) |>
#   dplyr::select(-value_parsed_tmp) |>
#   # KEYWORD["en"] or KEYWORD
#   dplyr::mutate(keyword_parsed = ifelse(!is.na(language), stringr::str_c(keyword, b1, Q, language, Q, b2), keyword)) |>
#   # CHARSET="ANSI";
#   dplyr::mutate(s = ifelse(is.na(varname) & is.na(valname),
#                            stringr::str_c(keyword_parsed, e, value_parsed, E),
#                            keyword_parsed
#   )) |>
#   dplyr::mutate(
#     # NOTE("age")="...";
#     s = ifelse(!is.na(varname) & is.na(valname),
#                stringr::str_c(keyword_parsed, oq, Q, varname, Q, eq, e, value_parsed, E),
#                s
#
#     )) |>
#   dplyr::mutate(
#     # VALUENOTE("age", "0-15")="...";
#     s = ifelse(!is.na(varname) & !is.na(valname),
#                stringr::str_c(keyword_parsed, oq, Q, varname, Q, comma, Q, valname, Q, eq, e, value_parsed, E),
#                s
#     )
#   ) |>
#   dplyr::mutate(
#     # TIMEVAL(”time”)=TLIST(A1), ”1994”, ”1995”,"1996”
#     s = ifelse(keyword == "TIMEVAL",
#                stringr::str_c(keyword_parsed, oq, Q, varname, Q, eq, "=TLIST(", valname, "), ", value_parsed, E),
#                s
#
#     ))




#
#
# Q <- '"'
# E <- ";"
# e <- "="
# comma <- ","
# oq <- "("
# eq <- ")"
# b1 <- "["
# b2 <- "]"
#
# main_lang <- px_get_main_language(metadata_example_multilingual)
# langs <- px_get_languages(metadata_example_multilingual)
#
# tmp <- metadata_example_multilingual |>
#   sort_metadata_keywords() |>
#   dplyr::left_join(specs |> dplyr::select(Keyword, Multivalue, Multiline, Quoted), by = c("keyword" = "Keyword")) |>
#   dplyr::mutate(value_parsed_tmp = ifelse(Multivalue == TRUE & keyword != "VALUES",
#                                           splitlist(value), value),
#                 value_parsed = ifelse(Multivalue == FALSE & keyword != "VALUES" & Quoted == TRUE, addquotes(value_parsed_tmp), value_parsed_tmp)
#   ) |>
#   dplyr::select(-value_parsed_tmp) |>
#   # KEYWORD[lang] or KEYWORD. KEYWORD[lang] only necessary if its not the main language
#   dplyr::mutate(keyword_parsed = ifelse(!is.na(language) & language != main_lang,
#                                         stringr::str_c(keyword, b1, language, b2),
#                                         keyword)) |>
#   # CHARSET="ANSI";
#   dplyr::mutate(s = ifelse(is.na(varname) & is.na(valname),
#                            stringr::str_c(keyword_parsed, e, value_parsed, E),
#                            keyword_parsed
#   )) |>
#   dplyr::mutate(
#     # NOTE("age")="...";
#     s = ifelse(!is.na(varname) & is.na(valname),
#                stringr::str_c(keyword_parsed, oq, Q, varname, Q, eq, e, value_parsed, E),
#                s
#
#     )) |>
#   dplyr::mutate(
#     # VALUENOTE("age", "0-15")="...";
#     s = ifelse(!is.na(varname) & !is.na(valname),
#                stringr::str_c(keyword_parsed, oq, Q, varname, Q, comma, Q, valname, Q, eq, e, value_parsed, E),
#                s
#     )
#   ) |>
#   dplyr::mutate(
#     # TIMEVAL(”time”)=TLIST(A1), ”1994”, ”1995”,"1996”
#     s = ifelse(keyword == "TIMEVAL" & language == main_lang,
#                stringr::str_c(keyword_parsed, oq, Q, varname, Q, eq, "=TLIST(", valname, "), ", value_parsed, E),
#                s
#
#     )) |>
#   # TIMEVAL can only occur once, only for main lang
#   filter(!(keyword == "TIMEVAL" & language != main_lang))
#
#
# tmp |>
#   sort_metadata_keywords() |>
#   mutate(language = factor(language, levels = langs)) |>
#   arrange(keyword, language) |> View()
#
#   arrange(language)
#
# if (full_dataframe) {
#   tmp
# } else {
#   tmp |>
#     dplyr::select(s)
# }
