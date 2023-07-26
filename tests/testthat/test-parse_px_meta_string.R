test_that("keyword logic works", {
  expect_true(
    parse_px_meta_string("CHARSET=\"ANSI\";"),
    parse_px_meta_string("CHARSET[en]=\"ANSI\";")
  )
})

