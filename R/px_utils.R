
px_get_values_from_data <- function(.data, as_list = FALSE) {
  assertthat::assert_that(any(names(.data) %in% "value"),
                          msg = "No column named value found in data frame, please add it.")

  levs <- .data %>%
    dplyr::select(-value) %>%
    map(unique)

  assertthat::assert_that(length(levs)>0, msg = "Number of variables in dataframe is not greater than 0.")


  if (as_list) {
    return(levs)
  } else {
    levs %>%
      map(str_c, collapse = ",") %>%
      dplyr::as_tibble() %>%
      tidyr::pivot_longer(everything(), names_to = "varname", values_to = "value") %>%
      dplyr::mutate(keyword = "VALUES", language = NA_character_, valname = NA_character_)
  }
}


px_add_values_from_data <- function(.metadata_df, .data) {
  vals_to_add <- px_get_values_from_data(.data)
  px_meta_compare_varnames(.metadata_df_new = vals_to_add, .metadata_df = .metadata_df)

  .metadata_df %>%
    rows_insert(vals_to_add, by = c("keyword","language", "varname", "valname"))
}

#px_add_values_from_data(new_meta, ex_data)



add_timevals_from_data_to_value <- function(.metadata_df, .data, time_variable) {
  timevals <- .data |> dplyr::select(dplyr::all_of(time_variable)) |> dplyr::pull() |> unique() |> str_c(collapse = ",")
  .metadata_df |>
    dplyr::mutate(value = ifelse(keyword=="TIMEVAL", timevals, value))
}

#' Convert data in long format into same format according to STUB and HEADING
#'
#' @param .metadata_df Metadata tibble
#' @param .data Data tibble in long format
#'
#' @return Returns tibble in wide format according to order of STUB and HEADING
#' @export
#'
#' @examples
convert_data_to_final <- function(.metadata_df, .data) {
  stub <- get_value_by_keyword(.metadata_df, "STUB") |> split_commas()
  heading <- get_value_by_keyword(.metadata_df, "HEADING") |> split_commas()
  ordning <- c(stub, heading)

  .data |>
    dplyr::select(all_of(ordning), value) |>
    tidyr::pivot_wider(names_from = all_of(heading), values_from = value)
}


# convert_data_to_final(new_meta, ex_data)


#' Get value from metadata tibble by keyword
#'
#' @param .metadata_df metadata tibble to extract from
#' @param .key keyword you want to get value from
#'
#' @return returns value(s) matching the keyword
#' @export
#'
#' @examples
#' get_value_by_keyword(meta_example, "STUB")
get_value_by_keyword <- function(.metadata_df, .key) {
  .metadata_df %>%
    dplyr::filter(keyword==.key) %>%
    dplyr::pull(value)
}


#' Split commas in for example stub
#'
#' @param .values single string with commas
#'
#' @return splitted vector
#' @export
#'
#' @examples
#' get_value_by_keyword(meta_example, "STUB") |> split_commas()
split_commas <- function(.values) {
  .values |>
    str_split_1(",") %>%
    str_squish()
}




#' Generate dynamic title based on LANGUAGE, STUB and HEADING
#'
#' @param .metadata_df metadata tibble to extract from
#'
#' @return returns single string as dynamically generated title.
#' @export
#'
#' @examples
px_generate_dynamic_title <- function(.metadata_df) {
  default_lang <- get_value_by_keyword(.metadata_df, "LANGUAGE")
  contents <- get_value_by_keyword(.metadata_df, "CONTENTS")
  stub <- get_value_by_keyword(.metadata_df, "STUB") %>%
    str_split_1(",") %>%
    str_c(collapse = ", ") %>%
    str_squish()
  heading <- get_value_by_keyword(.metadata_df, "HEADING")

  if (default_lang == "sv") {
    by <- "efter"
    and <- "och"
  } else {
    by <- "by"
    and <- "and"
  }

  str_c(contents, " ", by, " ", stub, " ", and, " ", heading, ".")
}
