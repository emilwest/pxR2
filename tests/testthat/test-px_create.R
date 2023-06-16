

test_that("px object can be created from example data", {
  expect_no_error(
    px_create(.data = ex_data,
              stub = "sex,age",
              heading = "time",
              time_variable = "time",
              time_scale="annual",
              matrix = "TEST01",
              subject_area = "Test",
              subject_code = "T",
              units = "Antal timmar",
              contents = "Genomsnitt antal tittartimmar av The Simpsons",
              decimals = 1,
              language = "en"
    )
    )
})
