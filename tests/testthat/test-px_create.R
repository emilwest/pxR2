

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




test_that("px object can be created with special column pxvalue", {
  expect_no_error(
    {
      tt <- ex_data
      tt$value[1:10] <- NA
      tt$value

      tt <- tt |>
        mutate(pxvalue = c(rep(NA,10), rep(".", 10), rep("-", 7)))


      px_obj <- px_create(tt,
                          stub = "sex,age",
                          heading = "time",
                          time_variable = "time",
                          time_scale = "annual",
                          matrix = "TEST01",
                          subject_area = "Forestry",
                          subject_code = "F",
                          units = "Number",
                          contents = "Number of trees",
                          decimals = 1,
                          language = "en"
      )
    }


  )
})



# test_that("multilingual px object can be created from example data", {
#   expect_no_error(
#     px_create(tt, meta_csv_path = "inst/extdata/metadata_example_multilingual.csv")
#
#   )
# })




