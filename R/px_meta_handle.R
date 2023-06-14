

px_meta_init_empty <- function() {
  tibble(keyword=character(),  language=character(), varname=character(),  valname=character(),  value=character())
}

px_meta_init_unempty <- function(keyword,language,varname, valname, value) {
  tibble(keyword=keyword,  language=language, varname=varname,  valname=valname,  value=value)
}


metadata <- px_meta_init_empty()

metadata %>%
  rows_insert(px_meta_init_unempty("h","d","d","s","s"), by = c("keyword","language", "varname", "valname")) %>%
  rows_insert(px_meta_init_unempty("h","d","d","s2","s"), by = c("keyword","language", "varname", "valname"))


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
      rows_insert(to_add, by = c("keyword","language", "varname", "valname"))
    ,
    error=function(e) {
      message('metadata already exist')
      print(e)
    }
  )

}
x

px_meta_add_keyword(x,
                    keyword = "ELIMINATION",
                    language = "se",
                    varname = "län",
                    valname = "s",
                    value = "s")

px_meta_add_keyword(x,
                    keyword = "MATRIX",
                    value = "s")
ex_data

new_meta <- px_meta_init_empty()
new_meta <- px_meta_add_keyword(new_meta, "STUB", value = "sex,age")
new_meta <- px_meta_add_keyword(new_meta, "HEADING", value = "time")


px_meta_add_timeval <- function(.metadata_df,
                        time_variable,
                        time_scale # A1/annual, H1/halfyear, Q1/quarterly, M1 monthly, W1/weekly
) {

  time_scale <- case_when(
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



# stub, # variables to display along the rows
# heading, # variables to display along the columns
# time_variable=NULL, # mandatory if time variable exist (TLIST)
# matrix, # mandatory
# subject_area, # mandatory
# subject_code, # mandatory
# units, # mandatory
# contents, # mandatory (the title)
# decimals = 1, # mandatory
# showdecimals = 1,
# language = "en",
# charset = "ANSI",
# axis_version = "2013",
# codepage = "iso-8859-15",
# creation_date = format(Sys.time(), "%Y%m%d %H:%M"),
# last_updated = format(Sys.time(), "%Y%m%d %H:%M"),
# contact = NULL,
# source = NULL,
# note = NULL

