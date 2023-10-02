

test_that("empty metadata tibble is returned", {
  expect_equal(px_meta_init_empty(),  dplyr::tibble(keyword=character(),  language=character(), varname=character(),  valname=character(),  value=character()))
})


test_that("same tibble is returned", {
  expect_equal(px_meta_init_unempty("VALUENOTE", "sv", "län", "Östergötland", "hej"),  dplyr::tibble(keyword="VALUENOTE",  language="sv", varname="län",  valname = "Östergötland",  value = "hej"))
})


test_that("keyword rows can be inserted into metadata", {
  expect_no_error(
    px_meta_add_keyword(meta_example,
                        keyword = "ELIMINATION",
                        language = "se",
                        varname = "län",
                        valname = "s",
                        value = "s")

  )
})

test_that("keyword rows with only keyword+value can be inserted into metadata", {
  expect_no_error(
    px_meta_add_keyword(meta_example,
                        keyword = "MATRIX",
                        value = "s")

  )
})

test_that("error is thrown if keyword is invalid", {
  expect_error(
    px_meta_add_keyword(meta_example,
                        keyword = "x",
                        value = "s")

  )
})


test_that("annual timeval can be added", {
  expect_no_error(
    px_meta_init_empty() |>
      px_meta_add_keyword("STUB", value = "sex,age") |>
      px_meta_add_keyword("HEADING", value = "time") |>
      px_meta_add_timeval(time_variable = "time", time_scale = "annual")
  )
})


test_that("quarterly timeval can be added", {
  expect_no_error(
    px_meta_init_empty() |>
      px_meta_add_keyword("STUB", value = "sex,age") |>
      px_meta_add_keyword("HEADING", value = "time") |>
      px_meta_add_timeval(time_variable = "time", time_scale = "quarterly")
  )
})

test_that("monthly timeval can be added", {
  expect_no_error(
    px_meta_init_empty() |>
      px_meta_add_keyword("STUB", value = "sex,age") |>
      px_meta_add_keyword("HEADING", value = "time") |>
      px_meta_add_timeval(time_variable = "time", time_scale = "monthly")
  )
})

test_that("weekly timeval can be added", {
  expect_no_error(
    px_meta_init_empty() |>
      px_meta_add_keyword("STUB", value = "sex,age") |>
      px_meta_add_keyword("HEADING", value = "time") |>
      px_meta_add_timeval(time_variable = "time", time_scale = "weekly")
  )
})



