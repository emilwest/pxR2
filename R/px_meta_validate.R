
is_mandatory <- function(keyword) {
  mandatory_vars <- specs %>%
    dplyr::filter(Mandatory == TRUE) %>%
    dplyr::filter(Keyword != "DATA") %>%
    dplyr::pull(Keyword)

  keyword %in% mandatory_vars
}


px_meta_check_mandatory <- function(.metadata_df, exclude = NULL) {
  mandatory_vars <- specs %>%
    dplyr::filter(Mandatory == TRUE) %>%
    dplyr::filter(Keyword != "DATA") %>%
    dplyr::select(Keyword)

  if (!is.null(exclude)) {
    mandatory_vars <- mandatory_vars |>
      filter(!(Keyword %in% exclude))
  }

  missing <- mandatory_vars %>%
    dplyr::anti_join(.metadata_df, by = c("Keyword" = "keyword"))
  missing_in_text <- str_c(pull(missing), collapse = ", ")
  missing_in_text_help <- str_c("show_keyword_help(\"",pull(missing), "\")", collapse = ", ")

  assertthat::assert_that(nrow(missing)== 0,
                          msg = str_c("The following mandatory keywords are missing in metadata: ",
                                      missing_in_text,
                                      ". Please add them to the metadata. \n Type ", missing_in_text_help, " to get help.")
                          )

}


px_meta_check_if_language_dependent_ok <- function(.metadata_df) {
  lang_dep <- specs %>%
    dplyr::filter(Language_dependent == TRUE) %>%
    dplyr::select(Keyword)

  should_not_have_language <- .metadata_df %>%
    dplyr::anti_join(lang_dep, by = c("keyword" = "Keyword")) %>%
    dplyr::filter(!is.na(language))
  should_not_have_language_text <- str_c(pull(should_not_have_language), collapse = ", ")

  assertthat::assert_that(nrow(should_not_have_language)== 0,
                          msg = str_c("The following keywords have values in the language column although they are language dependent: ",
                                      should_not_have_language_text,
                                      ". Change them to NA"))

}

px_meta_check_if_keywords_are_valid <- function(.metadata_df) {
  test <- .metadata_df %>%
    dplyr::select(keyword) %>%
    dplyr::anti_join(specs %>% select(keyword = Keyword), by = "keyword")
  if (nrow(test) > 0) {
    unkown_keywords <- str_c(pull(test), collapse = ", ")
  }

  assertthat::assert_that(nrow(test)== 0,
                          msg = str_c("There are keywords in the metadata that are not in PX-file format specification AXIS-VERSION 2013.
                                      Unknown keywords: ",
                                      unkown_keywords,
                                      ".\nConsider removing it or check the spelling."
                                      ))

}
#
# x %>%
#   bind_rows(tibble(keyword = "HEJ")) %>%
#   px_meta_check_if_keywords_are_valid()
#
# px_meta_check_if_keywords_are_valid(x)


px_meta_validate <- function(.metadata_df) {
  px_meta_check_if_keywords_are_valid(.metadata_df)
  px_meta_check_if_language_dependent_ok(.metadata_df)
  px_meta_check_mandatory(.metadata_df)
}

# px_meta_validate(x)

# get varnames as defined in stub and heading
get_varnames <- function(.metadata_df) {
  .metadata_df %>%
    dplyr::filter(keyword %in% c("STUB", "HEADING")) %>%
    dplyr::pull(value) %>%
    str_split(",") %>%
    unlist()
}

#varnames_in_meta <- x %>% get_varnames()
# varnames_found <- x %>%
#   dplyr::select(varname) %>%
#   drop_na() %>%
#   pull()
#
# setdiff(varnames_found, varnames_in_meta)
# varnames_in_meta


px_meta_compare_varnames <- function(.metadata_df_new, .metadata_df) {
  varnames_in_meta <- .metadata_df %>% get_varnames()
  if (all(is.na(varnames_in_meta))) stop("No varnames found in metadata, please add them to STUB or HEADING")
  varnames_found <- .metadata_df_new %>%
    dplyr::select(varname) %>%
    drop_na() %>%
    dplyr::pull()

  l <- setdiff(varnames_found, varnames_in_meta)
  xx <- str_c(varnames_in_meta, collapse = ", ")

  assertthat::assert_that(length(l)==0,
                          msg = str_c("At least one variable name in the varname column is not found in STUB and HEADING: ",
                          l,
                          ".\nEither add the variable name to the STUB or HEADING or check the spelling of the varname.",
                          "\nThe variable names are case-sensitive.",
                          "\n Valid varnames: ", xx
                          )
                          )
}

#px_meta_compare_varnames(x, x)




# px_meta_check_mandatory(pxR2::meta_example, exclude = c("VALUES", "TITLE"))
# px_meta_check_if_language_dependent_ok(meta_example)
# px_meta_check_if_keywords_are_valid(meta_example)
#
# px_meta_validate(meta_example)
#
#
#
# meta <- px_read_meta_csv("MARIA03.csv")
# xx <- try(px_meta_compare_varnames(meta, meta))
# try(px_meta_check_mandatory(meta, exclude = c("VALUES", "TITLE")))
# try(px_meta_check_if_language_dependent_ok(meta))
#
#
# get_varnames(meta)

