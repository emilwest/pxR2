# intern fil som används för package building

# install.packages("devtools")
#install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
library(devtools)
library(usethis)
# https://r-pkgs.org/whole-game.html


use_git()
devtools::load_all()
hello()
check()
package?readr
package?ggplot2
use_gpl_license()
devtools::document()

# library(pxR2)
# helloe()
use_testthat()
use_test("helloe") # skapar testfil för funktionen helloe

library(testthat)

usethis::use_data_raw("metadata_example_multilingual")

devtools::load_all()
devtools::document()
devtools::test()
devtools::build()

?pxR2::px_parse_metadata
?pxR2::px_create
?pxR2::px_create()
# use_package("stringr")
use_github()
use_readme_rmd()

usethis::use_test("px_create")
usethis::use_test("px_meta_handle")
usethis::use_test("px_meta_validate")
usethis::use_test("px_meta_validate")
usethis::use_test("parse_px_meta_string")
usethis::use_rcpp()

#parse_px_meta_string()

usethis::use_logo(img = "man/figures/pxR2_original.png")

usethis::use_tidy_dependencies()

usethis::use_package("dplyr", type = "Imports", min_version = "1.1.2")

?dplyr


# ---------------------
# example

ex_data2 <- ex_data |> mutate(sex = factor(sex, levels = c("Female", "Male", "Total"), labels = c("Female, hej", "Male, hej2", "Total, total3")))


px_obj <- px_create(ex_data2,
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

px_obj
px_parse_metadata(px_obj$metadata)

px_write(px_obj, file = "test4.px")
px_write(px_obj)



ex_data
px_obj <- px_create(ex_data,
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

px_obj





px_parse_metadata(px_obj$metadata)
#
# # extract numbers from df only
# data <- px_obj$data
#
# if(any(names(data)=="pxvalue"))
#
# data |>
#   mutate(value = coalesce(addquotes(pxvalue), as.character(value))) |> View()
#   select(-pxvalue) |>
#   tidyr::pivot_wider(names_from = time, values_from = value)
#
#
# meta <- px_obj$metadata
# # format data according to stub and heading
#
#
# d <- convert_data_to_final(meta, data)
#
# m <- d |>
#   dplyr::select(-c(sex,age)) |>
#   as.matrix()
#
# m
#
# format(m)
#
# m <- ifelse(is.na(m), addquotes(".."), m)
# m
# x <- apply(format(m), 1, paste, collapse=" ")
# x[8]
#
#
# m[is.na(m),]

px_write(px_obj, file = "test4.px")
px_write(px_obj)


b <- px_create(tt,
               meta_csv_path = "inst/extdata/metadata_example_multilingual.csv")

b
px_parse_metadata(b$metadata) |> View()

x <- b$metadata |>
  get_value_by_keyword("STUB")
x[1] |> split_commas()

px_obj$metadata |>
  get_value_by_keyword("STUB") |>
  split_commas()

px_write(b, file = "test_multilingual.px")
View(b)
b$metadata |> View()

# devtools::install_github('StatisticsGreenland/pxmake',
#                          ref = '167-metamake-from-dataset'
# )
#
# library(tidyverse)
# library(pxmake)
# packageVersion("pxmake")
#
# ##
# set.seed(1)
# ex_data <- tibble(sex = c("Female", "Male", "Total"),
#                   age = c("0-15", "16-25", "26-50"),
#                   time = c("2021", "2022", "2023"),
#                   value = c(NA, NA, NA)
# ) %>%
#   complete(sex, age, time) %>%
#   mutate(value = rnorm(27, mean = 20, sd = 5))
#
# ex_data
#
# a <- ex_data |>
#   dplyr::rename(year=time)
#
# metamake(a, out_path = "HEJ.xlsx")
#a <- pxmake::metamake(ex_data)
#a
#
# # this works
# df <-
#   world_bank_pop %>%
#   filter(indicator == "SP.POP.TOTL") %>%
#   filter(country %in% c("DNK", "SWE", "GRL")) %>%
#   select(-indicator) %>%
#   pivot_longer(cols = -country, names_to = "year")
#
# df
# metamake(df, out_path = 'population.xlsx')
