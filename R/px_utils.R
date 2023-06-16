
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

convert_data_to_final <- function(.metadata_df, .data) {
  stub <- .metadata_df %>%
    dplyr::filter(keyword=="STUB") %>%
    dplyr::pull(value) %>%
    str_split_1(",") %>%
    str_c(collapse = ",") %>%
    str_squish()
  heading <- .metadata_df %>%
    dplyr::filter(keyword=="HEADING") %>%
    dplyr::pull(value)
  ordning <- c(stub,heading) |> str_split(",") |> unlist()
  print(ordning)

  .data |>
    dplyr::select(all_of(ordning), value) |>
    tidyr::pivot_wider(names_from = all_of(heading), values_from = value)

}

# convert_data_to_final(new_meta, ex_data)



px_generate_dynamic_title <- function(.metadata_df) {
  default_lang <- .metadata_df %>%
    dplyr::filter(keyword=="LANGUAGE") %>%
    dplyr::pull(value)
  contents <- .metadata_df %>%
    dplyr::filter(keyword=="CONTENTS") %>%
    dplyr::pull(value)
  stub <- .metadata_df %>%
    dplyr::filter(keyword=="STUB") %>%
    dplyr::pull(value) %>%
    str_split_1(",") %>%
    str_c(collapse = ", ") %>%
    str_squish()
  heading <- .metadata_df %>%
    dplyr::filter(keyword=="HEADING") %>%
    dplyr::pull(value)

  if (default_lang == "sv") {
    by <- "efter"
    and <- "och"
  } else {
    by <- "by"
    and <- "and"
  }

  str_c(contents, " ", by, " ", stub, " ", and, " ", heading, ".")

}
