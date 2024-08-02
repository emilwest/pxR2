

#' Initialize empty px metadata tibble
#'
#' @return Returns empty px metadata tibble
#'
#' @examples
#' \dontrun{
#' px_meta_init_empty()
#' }
#' @keywords internal
px_meta_init_empty <- function() {
  dplyr::tibble(keyword=character(),  language=character(), varname=character(),  valname=character(),  value=character())
}

#' Initialize nonempty px metadata tibble with arguments
#'
#' @param keyword Mandatory. PX keyword in capital letters.
#' @param language Language code, for example 'en', 'sv', etc.
#' @param varname Name of variable you want to comment.
#' @param valname Name of the value you want to comment.
#' @param value Mandatory. The text you want to add.
#'
#' @return Filled px metadata tibble
#'
#' @examples
#' \dontrun{
#' px_meta_init_unempty("VALUENOTE", "", "age", "15-20 years", "Preliminary figures")
#' px_meta_init_unempty("valuenote", "", "age", "15-20 years", "Preliminary figures")
#' px_meta_init_unempty("STUB", "", "", "", "age,sex")
#' # will throw error if keyword is not valid according to PX Specifications
#' }
#' @keywords internal
px_meta_init_unempty <- function(keyword,
                                 language,
                                 varname,
                                 valname,
                                 value) {
  #assertthat::assert_that(is.character(value))
  toadd <- dplyr::tibble(keyword=toupper(keyword),  language=language, varname=varname,  valname=valname,  value=value)
  px_meta_check_if_keywords_are_valid(toadd)
  return(toadd)
}


#' Add new row of keywords to metadata tibble
#'
#' @param metadata Metadata tibble to add to.
#' @param keyword Mandatory. PX keyword in capital letters.
#' @param language Language code, for example 'en', 'sv', etc.
#' @param varname Name of variable you want to comment.
#' @param valname Name of the value you want to comment.
#' @param value Mandatory. The text you want to add.
#'
#' @return Metadata tibble
#'
#' @examples
#' \dontrun{
#' px_meta_add_keyword(meta_example, "VALUENOTE", NA, "typ", "typ value", "Preliminary figures")
#' px_meta_add_keyword(meta_example, "VALUENOTE", NA, NA, NA, c("age","sex"))
#' }
#' @keywords internal
px_meta_add_keyword <- function(
    metadata,
  keyword,
  language=NA,
  varname=NA,
  valname=NA,
  value
) {

  to_add <- px_meta_init_unempty(keyword=keyword,
                                 language = language,
                                 varname = varname,
                                 valname = valname,
                                 value =value
  )

  if (!is.na(varname)) {
    px_meta_compare_varnames(to_add, metadata)
  }
  if (!is.na(valname)) {
    #TODO lägg in check här
  }

  tryCatch(
    metadata |>
      dplyr::rows_insert(to_add, by = c("keyword","language", "varname", "valname"),)
    ,
    error=function(e) {
      message('The keyword-value combination already exist in metadata')
      print(e)
    }
  )

}


px_meta_update_value <- function(metadata,
               target_column = c("keyword","language", "varname", "valname"),
               target_value,
               new_value) {
  match.arg(target_column)

  metadata |>
    mutate(value = ifelse(!!sym(target_column) == target_value,
                          new_value,
                          value
    ))

}


#' Add time value information to px metadata tibble
#'
#' @param .metadata_df  Metadata tibble to add to.
#' @param time_variable Name of variable containing time info, for example 'time', 'år', etc.
#' @param time_scale Choose from 'annual', 'halfyear', 'quarterly', 'monthly', 'weekly'.
#' Choose annual format if the time variable is formatted as CCYY (C for century, Y for year).
#' Choose halfyear format if the time variable is formatted as CCYYH (H is 1 or 2).
#' Choose quarterly format if the time variable is formatted as CCYYQ (Q is 1-4).
#' Choose monthly format if the time variable is formatted as CCYYMM (M is 1-12).
#' Choose weekly format if the time variable is formatted as CCYYWW (M is 1-52).
#'
#' @return Metadata tibble
#'
#' @examples
#' \dontrun{
#' meta_example |>
#' dplyr::filter(keyword != "TIMEVAL") |>
#' px_meta_add_timeval("år", "annual")
#' }
#' @keywords internal
px_meta_add_timeval <- function(.metadata_df,
                        time_variable,
                        time_scale # A1/annual, H1/halfyear, Q1/quarterly, M1 monthly, W1/weekly
) {


  time_scale <- dplyr::case_when(
    time_scale == "annual" ~ "A1",
    time_scale == "halfyear" ~ "H1",
    time_scale == "quarterly" ~ "Q1",
    time_scale == "monthly" ~ "M1",
    time_scale == "weekly" ~ "W1"

    )

  px_meta_add_keyword(metadata = .metadata_df,
                      keyword = "TIMEVAL",
                      varname = time_variable,
                      valname = time_scale,
                      value = NA_character_)
}




