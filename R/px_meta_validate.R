


specs <- read_delim("data/px_format_specification", delim = " ", col_types = cols(.default = "c"))

specs %>% map(unique)

specs %>%
  filter(Language_dependent=="Yes") %>%
  select(Keyword) %>% view


specs %>%
  filter(Multiplicity == "1")


px_meta_check_mandatory <- function(.metadata_df) {
  mandatory_vars <- specs %>%
    filter(Mandatory=="Yes") %>%
    filter(Keyword!="DATA") %>%
    select(Keyword)

  missing <- mandatory_vars %>%
    anti_join(.metadata_df, by = c("Keyword" = "keyword"))
  missing_in_text <- str_c(pull(missing), collapse = ", ")

  assertthat::assert_that(nrow(missing)== 0,
                          msg = str_c("The following mandatory keywords are missing in metadata: ",
                                      missing_in_text,
                                      ". Please add them to the metadata."))

}


px_meta_check_mandatory(x)

x

px_meta_check_if_language_dependent_ok <- function(.metadata_df) {
  lang_dep <- specs %>%
    filter(Language_dependent=="Yes") %>%
    select(Keyword)

  should_not_have_language <- .metadata_df %>%
    anti_join(lang_dep, by = c("keyword" = "Keyword")) %>%
    filter(!is.na(language))
  should_not_have_language_text <- str_c(pull(should_not_have_language), collapse = ", ")

  assertthat::assert_that(nrow(should_not_have_language)== 0,
                          msg = str_c("The following keywords have values in the language column although they are language dependent: ",
                                      should_not_have_language_text,
                                      ". Change them to NA"))

}

px_meta_check_if_language_dependent_ok(x)


px_meta_check_if_keywords_are_valid <- function(.metadata_df) {
  test <- .metadata_df %>%
    select(keyword) %>%
    anti_join(specs %>% select(keyword = Keyword), by = "keyword")
  if (nrow(test) > 0) {
    unkown_keywords <- str_c(pull(test), collapse = ", ")
  }

  assertthat::assert_that(nrow(test)== 0,
                          msg = str_c("There are keywords in the metadata
                                      that are not in PX-file format specification AXIS-VERSION 2013.
                                      Unknown keywords: ",
                                      unkown_keywords,
                                      ".\nConsider removing it or check the spelling."
                                      ))

}

x %>%
  bind_rows(tibble(keyword = "HEJ")) %>%
  px_meta_check_if_keywords_are_valid()

px_meta_check_if_keywords_are_valid(x)


px_meta_validate <- function(.metadata_df) {
  px_meta_check_if_keywords_are_valid(.metadata_df)
  px_meta_check_if_language_dependent_ok(.metadata_df)
  px_meta_check_mandatory(.metadata_df)
}

px_meta_validate(x)

get_varnames <- function(.metadata_df) {
  .metadata_df %>%
    filter(keyword %in% c("STUB", "HEADING")) %>%
    pull(value) %>%
    str_split(",") %>%
    unlist()
}

varnames_in_meta <- x %>% get_varnames()
varnames_found <- x %>%
  select(varname) %>%
  drop_na() %>%
  pull()

setdiff(varnames_found, varnames_in_meta)
varnames_in_meta


px_meta_compare_varnames <- function(.metadata_df_new, .metadata_df) {
  varnames_in_meta <- .metadata_df %>% get_varnames()
  varnames_found <- .metadata_df_new %>%
    select(varname) %>%
    drop_na() %>%
    pull()

  l <- setdiff(varnames_found, varnames_in_meta)
  xx <- str_c(varnames_in_meta, collapse = ", ")

  assertthat::assert_that(length(l)==0,
                          msg = str_c("At least one variable name in the varname column is not found in STUB and HEADING: ",
                          l,
                          ".\nEither add the variable name to the STUB or HEADING or check the spelling of the varname.",
                          "\n Valid varnames: ", xx
                          )
                          )
}

px_meta_compare_varnames(x, x)
