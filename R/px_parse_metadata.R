

#' Helper for metadata parser, adds quotes around values in a string
#'
#' @param txt Text string to add quotes around
#'
#' @return Text string with added quotes
#' @export
#'
#' @seealso [splitlist()] for adding quotes to each element in a vector and joining to single string, and
#'  [px_parse_metadata()] for parsing metadata to text
#'
#' @examples
#' addquotes('2021')
#' addquotes(c("2021","2022","2023"))
#'
addquotes <- function(txt) {
  Q <- '"'
  stringr::str_c(Q, txt, Q)
}


#' Helper for metadata parser, splits a string to vector, adds quotes and joins back with commas.
#'
#' @param txt Text vector with comma-separated values, e.g. '2021,2022,2023'
#'
#' @return Returns texts vector with added quotes around values, e.g '"\"2021\",\"2022\",\"2023\""'
#' @export
#'
#' @seealso [addquotes()], [px_parse_metadata()]
#'
#' @examples
#' splitlist('2021,2022,2023')
#' splitlist(c('2021,2022,2023', 'hej, hej'))
#' splitlist(c(NA, 'hej, hej'))
#'
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


#' Parse px metadata tibble to px-formatted text.
#'
#' @param .metadata_df Metadata tibble as input to be converted to px-formatted text.
#' @param full_dataframe If TRUE, return entire dataframe. Useful for debugging. Default FALSE.
#'
#' @return Px-formatted text
#' @export
#'
#' @examples
#'
#' px_parse_metadata(meta_example)
#' px_parse_metadata(metadata_example_multilingual)
#'
px_parse_metadata <- function(.metadata_df, full_dataframe = FALSE) {

  Q <- '"'
  E <- ";"
  e <- "="
  comma <- ","
  oq <- "("
  eq <- ")"
  b1 <- "["
  b2 <- "]"

  main_lang <- px_get_main_language(.metadata_df)
  langs <- px_get_languages(.metadata_df)

  tmp <- .metadata_df |>
    sort_metadata_keywords() |>
    mutate(language = factor(language, levels = langs)) |>
    arrange(keyword, language)

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
    filter(!(keyword == "TIMEVAL" & language != main_lang))


  if (full_dataframe) {
    tmp
  } else {
    tmp |>
      dplyr::select(s)
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
