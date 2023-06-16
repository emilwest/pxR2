
test_that("error throws if any mandatory variables are missing", {
  testthat::expect_error(px_meta_check_mandatory(meta_example))
})

test_that("language dependent keywords are valid", {
  testthat::expect_true(px_meta_check_if_language_dependent_ok(meta_example))
})

test_that("px keywords are valid according to specification", {
  testthat::expect_true(px_meta_check_if_keywords_are_valid(meta_example))
})

test_that("error throws if any PX keyword is invalid according to specification", {
  testthat::expect_error(meta_example %>%
                          dplyr::bind_rows(tibble(keyword = "HEJ")) %>%
                          px_meta_check_if_keywords_are_valid()
                        )
})


test_that("error throws if any px validation check fails", {
  testthat::expect_error(px_meta_validate(meta_example))
})


test_that("error is thrown if any variable name in varname column of .px_metadata_new is not found in STUB or HEADING of .px_metadata", {
  testthat::expect_error(px_meta_compare_varnames(px_meta_init_unempty("ELIMINATIOn","d","hej","s","s"), meta_example))
})


test_that("variable name in varname column of .px_metadata_new is found in STUB or HEADING of .px_metadata", {
  testthat::expect_true(px_meta_compare_varnames(px_meta_init_unempty("ELIMINATIOn","d","län","s","s"), meta_example))
})




