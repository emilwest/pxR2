## code to prepare `ex_data` dataset goes here

set.seed(1)
ex_data <- tibble(sex = c("Female", "Male", "Total"),
                  age = c("0-15", "16-25", "26-50"),
                  time = c("2021", "2022", "2023"),
                  value = c(NA, NA, NA)
) %>%
  complete(sex, age, time) %>%
  mutate(value = rnorm(27, mean = 20, sd = 5))

usethis::use_data(ex_data, overwrite = TRUE)
