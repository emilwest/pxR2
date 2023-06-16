

px_meta_init_empty <- function() {
  dplyr::tibble(keyword=character(),  language=character(), varname=character(),  valname=character(),  value=character())
}

px_meta_init_unempty <- function(keyword,language,varname, valname, value) {
  dplyr::tibble(keyword=keyword,  language=language, varname=varname,  valname=valname,  value=value)
}


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

  # denna check körs alltid
  px_meta_check_if_keywords_are_valid(to_add)

  if (!is.na(varname)) {
    px_meta_compare_varnames(to_add, metadata)
  }
  if (!is.na(valname)) {
    #TODO lägg in check här
  }

  assertthat::assert_that(is.character(value))

  tryCatch(
    metadata %>%
      dplyr::rows_insert(to_add, by = c("keyword","language", "varname", "valname"))
    ,
    error=function(e) {
      message('metadata already exist')
      print(e)
    }
  )

}


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




