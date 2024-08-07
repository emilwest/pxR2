
#' Helper function to extract LANGUAGES from metadata tibble as a character vector.
#' If not defined, NA_character_ is returned.
#'
#' @param .metadata_df  Metadata tibble
#'
#' @return Returns character vector of languages or NA_character_ if undefined.
#'
#' @examples
#' \dontrun{
#' px_get_languages(meta_example)
#' px_get_languages(metadata_example_multilingual)
#' }
#' @keywords internal
px_get_languages <- function(.metadata_df) {
  langs <- get_value_by_keyword(.metadata_df, "LANGUAGES")
  if (length(langs) > 0) {
    split_commas(langs)
  } else {
    NA_character_
  }
}


#' Helper function to extract main language from metadata tibble, if it exists.
#' If LANGUAGE keyword exists, that is assumed to be the main language.
#' If LANGUAGE keyword doesn't exist, but LANGUAGES does, the first language of LANGUAGES is assumed to be the main language.
#'
#' @param .metadata_df Metadata tibble.
#'
#' @return Returns the main language as character vector.
#'
#' @examples
#' \dontrun{
#' px_get_main_language(meta_example)
#' px_get_main_language(metadata_example_multilingual)
#'
#' px_meta_init_unempty(keyword="CHARSET", NA, NA, NA, value = "ANSI") |>
#' px_get_main_language()
#'
#' px_meta_init_unempty(keyword="LANGUAGES", NA, NA, NA, value = "sv,en") |>
#' px_get_main_language()
#'
#' px_meta_init_unempty(keyword="LANGUAGE", NA, NA, NA, value = "en") |>
#' px_get_main_language()
#' }
#' @keywords internal
px_get_main_language <- function(.metadata_df) {
  main_lang <- get_value_by_keyword(.metadata_df, "LANGUAGE") |> unname()
  if (length(main_lang)==0) {
    # if LANGUAGE not specified, try LANGUAGES
    main_lang <- px_get_languages(.metadata_df)
  }
  return(main_lang[1])
}




#' Extract values from dataframe/tibble into metadata tibble.
#'
#'
#' @param .data The dataframe or tibble to extract unique variable values from.
#' @param as_list If TRUE, all unique variable values are returned as a list.
#'
#' @return Returns metadata tibble or list with extracted unique variable values
#'
#' @examples
#' \dontrun{
#' px_get_values_from_data(ex_data)
#' px_get_values_from_data(ex_data, as_list = TRUE)
#' }
#' @keywords internal
px_get_values_from_data <- function(.data, as_list = FALSE) {
  assertthat::assert_that(any(names(.data) %in% "value"),
                          msg = "No column named value found in data frame, please add it.")

  special_cols <- c("value", "pxvalue")

  levs <- .data %>%
    dplyr::select(-any_of(special_cols)) %>%
    map(unique)

  assertthat::assert_that(length(levs)>0, msg = "Number of variables in dataframe is not greater than 0.")


  if (as_list) {
    return(levs)
  } else {
    levs %>%
      map(addquotes) |>
      map(str_c, collapse = ",") %>%
      dplyr::as_tibble() %>%
      tidyr::pivot_longer(everything(), names_to = "varname", values_to = "value") %>%
      dplyr::mutate(keyword = "VALUES", language = NA_character_, valname = NA_character_)
  }
}




get_special_colnames <- function() {
  c("value", "pxvalue")
}


add_timevals_from_data_to_value <- function(.metadata_df, .data, time_variable) {
  timevals <- .data |> dplyr::select(dplyr::all_of(time_variable)) |> dplyr::pull() |> unique() |> str_c(collapse = ",")
  .metadata_df |>
    dplyr::mutate(value = ifelse(keyword=="TIMEVAL", timevals, value))
}

#' Convert data in long format into same format according to STUB and HEADING.
#' The dataframe column names are assumed to come from the main language defined in the LANGUAGE keyword.
#'
#' @param .metadata_df Metadata tibble
#' @param .data Data tibble in long format
#'
#' @return Returns tibble in wide format according to order of STUB and HEADING
#'
#' @keywords internal
convert_data_to_final <- function(.metadata_df, .data) {
  main_lang <- get_value_by_keyword(.metadata_df, "LANGUAGE") |> split_commas()
  .metadata_df <- .metadata_df |>  filter(language == main_lang | is.na(language))
  stub <- get_value_by_keyword(.metadata_df, "STUB") |> split_commas() |> unlist()
  heading <- get_value_by_keyword(.metadata_df, "HEADING") |> split_commas() |> unlist()
  ordning <- c(stub, heading)
  # print(.metadata_df)
  # print(ordning)

  decimals <- get_value_by_keyword(.metadata_df, "DECIMALS")

  d <- dplyr::mutate(.data, value = round(value, decimals))
  if(any(names(d)=="pxvalue")) {
    d <- d |>
      dplyr::mutate(value = dplyr::coalesce(addquotes(pxvalue), as.character(value))) |>
      dplyr::select(-pxvalue)
  }


  d |>
    dplyr::select(all_of(ordning), value) |>
    tidyr::pivot_wider(names_from = all_of(heading), values_from = value)
}

# main_lang <- get_value_by_keyword(metadata_example_multilingual, "LANGUAGE")
# metadata_example_multilingual |> filter(language == main_lang | is.na(language))
# stub <- get_value_by_keyword(.metadata_df, "STUB") |> split_commas()
# heading <- get_value_by_keyword(.metadata_df, "HEADING") |> split_commas()
# ordning <- c(stub, heading)

# convert_data_to_final(new_meta, ex_data)


#' Get value from metadata tibble by keyword
#'
#' @param .metadata_df metadata tibble to extract from
#' @param .key keyword you want to get value from
#' @param main_lang if TRUE, only return value for main language (default false)
#'
#' @return returns named vector of value(s) matching the keyword. If multilingual, the vector is named by the language.
#' If the keyword is of type integer, it will be returned as integer.
#'
#' @examples
#' \dontrun{
#' get_value_by_keyword(meta_example, "STUB")
#' get_value_by_keyword(metadata_example_multilingual, "STUB")
#' get_value_by_keyword(metadata_example_multilingual, "STUB", main_lang = TRUE)
#' get_value_by_keyword(metadata_example_multilingual, "TIMEVAL")
#' get_value_by_keyword(metadata_example_multilingual, "TIMEVAL", main_lang = TRUE)
#' get_value_by_keyword(metadata_example_multilingual, "DECIMALS")
#' }
#' @keywords internal
get_value_by_keyword <- function(.metadata_df, .key, main_lang=FALSE) {
  a <- .metadata_df %>%
    dplyr::filter(keyword==.key)
  b <- specs |>
    dplyr::select(keyword = Keyword, Type)
  c <- b |> dplyr::inner_join(a, by = "keyword")
  c <- c |> dplyr::mutate(value = ifelse(Type == "integer", as.integer(value), value))
  langs <- c$language
  vals <- c$value
  res <- set_names(vals, langs)
  if (main_lang) {
    lang <- px_get_main_language(.metadata_df)
    if (!is.na(lang)) {
      res[lang]
    }
  } else {
    res
  }

}




#' Check if keyword exists in metadata tibble
#'
#' @param .metadata_df metadata tibble
#' @param .key keyword to check if it exists
#'
#' @return returns TRUE if keyword exists
#'
#' @examples
#' \dontrun{
#' keyword_exists(meta_example, "DECIMALS")
#' keyword_exists(meta_example, "SHOWDECIMALS")
#' }
#' @keywords internal
keyword_exists <- function(.metadata_df, .key) {
  a <- get_value_by_keyword(.metadata_df, .key)
  ifelse(length(a)==0, FALSE, TRUE)
}




# x <- px_meta_add_keyword(px_meta_init_empty(),
# keyword = c("SHOWDECIMALS", "DECIMALS"), value = c("3","6"))
# get_final_rounding(x)

# get_final_rounding <- function(.metadata_df) {
#   decimals <- get_value_by_keyword(.metadata_df, "DECIMALS")
#   res <- decimals
#
#   if(keyword_exists(.metadata_df, "SHOWDECIMALS")) {
#     showdecimals <- get_value_by_keyword(.metadata_df, "SHOWDECIMALS")
#     if (showdecimals > 6) {
#       warning("SHOWDECIMALS > 6, must be between 0-6 and <= DECIMALS. Setting SHOWDECIMALS=6.")
#       showdecimals <- 6
#     }
#     res <- ifelse(showdecimals <= decimals & showdecimals <= 6, showdecimals, decimals)
#   }
#   return(unname(res))
# }


#' Split commas in for example stub
#'
#' @param .values single string with commas
#'
#' @return splitted vector
#'
#' @examples
#' \dontrun{
#' get_value_by_keyword(meta_example, "STUB") |> split_commas()
#' }
#' @keywords internal
split_commas <- function(.values) {
  .values |>
        str_split_1(",") |>
        str_squish()
}




#' Generate dynamic TITLE based on LANGUAGE, STUB and HEADING
#'
#' @param .metadata_df metadata tibble to extract from
#'
#' @return returns single string as dynamically generated title.
#'
#' @examples
#' \dontrun{
#' px_generate_dynamic_title(meta_example)
#' px_generate_dynamic_title(metadata_example_multilingual)
#' }
#' @keywords internal
px_generate_dynamic_title <- function(.metadata_df) {
  multilingual <- FALSE

  if (length(get_value_by_keyword(.metadata_df, "LANGUAGES")) != 0) {
    multilingual <- TRUE
    default_lang <- get_value_by_keyword(.metadata_df, "LANGUAGES") |>
      str_split_1(",") %>%
      str_squish() |>
      set_names()
  } else {
    default_lang <- get_value_by_keyword(.metadata_df, "LANGUAGE") |>
      set_names()
  }

  contents <- get_value_by_keyword(.metadata_df, "CONTENTS")

  stub <- get_value_by_keyword(.metadata_df, "STUB") %>%
    map_chr(~ .x |>
              str_split_1(",") %>%
              str_c(collapse = ", ") %>%
              str_squish()
    )
  heading <- get_value_by_keyword(.metadata_df, "HEADING")

  translation_matrix <-
    dplyr::tribble(
      ~language, ~by, ~and,
      "sv", "efter", "och",
      "en", "by", "and"
    )

  if (multilingual) {
    a <- bind_rows(list(stub=stub, heading=heading, contents=contents), .id = "id") |>
      pivot_longer(cols = -id, names_to = "language") |>
      pivot_wider(names_from = id, values_from = value) |>
      dplyr::left_join(translation_matrix, by = "language") |>
      dplyr::mutate(title = str_c(contents, " ", by, " ", stub, " ", and, " ", heading, "."))
  } else {
    a <- bind_rows(list(language = default_lang, stub=stub, heading=heading, contents=contents), .id = "id") |>
      dplyr::left_join(translation_matrix, by = "language") |>
      dplyr::mutate(title = str_c(contents, " ", by, " ", stub, " ", and, " ", heading, "."))
  }


  a <- dplyr::mutate(a, keyword = "TITLE")
  return(list(keyword = a$keyword, language = a$language, value = a$title))

}
