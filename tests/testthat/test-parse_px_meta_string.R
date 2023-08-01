test_that("simplest keyword logic works", {
  expect_equal(px_parse_meta_string("CHARSET=\"ANSI\";"),
               list("keyword" = "CHARSET",
                    "language" = "",
                    "subkeys" = character(0),
                    "values" = "ANSI"
                    )
               )
  expect_equal(px_parse_meta_string("NOTE[en]=\"Hej\";"),
               list("keyword" = "NOTE",
                    "language" = "en",
                    "subkeys" = character(0),
                    "values" = "Hej"
               )
               )
})

test_that("error returns for invalid keyword terminators", {
  expect_error(px_parse_meta_string("CHARSET;"))
  expect_error(px_parse_meta_string("CHARSET]"))
  expect_error(px_parse_meta_string("CHARSET)"))
})

# ----------------
# <language tag>

test_that("error returns for malformated language tag", {
  expect_error(px_parse_meta_string("CHARSET[en=\"ANSI\";"))
  expect_error(px_parse_meta_string("CHARSET[en)=\"ANSI\";"))
  expect_error(px_parse_meta_string("CHARSET[en\")=\"ANSI\";"))
})

test_that("error returns if langage tag contains whitespace", {
  expect_error(px_parse_meta_string("CHARSET[ en]=\"ANSI\";"))
})

# </language tag>
# ----------------
# ----------------
# <illegal characters between values>

test_that("error returns if there is an END_SUBKEY between values", {
  expect_error(px_parse_meta_string("CODES(\"age\")=\"0-19\",\"20-39\")\"40-100\";") )
})

test_that("error returns if there is a - between values for non-TIMEVAL", {
  expect_error(px_parse_meta_string("CODES(\"age\")=\"0-19\"-\"40-100\";"))
})

# </illegal characters between values>
# ----------------
# ----------------
# <non-closing quotes>

test_that("error returns if there is no closing quote on last value", {
  expect_error(px_parse_meta_string("STUB=\"age\",\"sex\",\"gender;"))
  expect_error(px_parse_meta_string("TIMEVAL(\"år\")=TLIST(A1),\"1968\",\"1969\",\"1970;"))
  expect_error(px_parse_meta_string("TIMEVAL(\"år\")=TLIST(A1),\"1970;"))
})

test_that("error returns if there is no closing/beginning quotes between two values", {
  expect_error(px_parse_meta_string("TIMEVAL(\"år\")=TLIST(A1),\"1968,\"1969\";"))
})


# </non-closing quotes>
# ----------------

# lägg till dessa
# px_parse_meta_string("CELLNOTE(\"kön\", \"*\", \"*\", \"ålder\")=\"Data not applicable;hej;")
# px_parse_meta_string("KEYS(\"age\")=\"VALUES;")
