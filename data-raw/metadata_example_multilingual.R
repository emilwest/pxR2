## code to prepare `metadata_example_multilingual` dataset goes here

metadata_example_multilingual <- readr::read_delim("inst/extdata/metadata_example_multilingual.csv",
                                                   delim = ";",
                                                   col_types = readr::cols(.default = "c"),
                                                   locale = readr::locale(encoding = "UTF-8")
                                                   )
usethis::use_data(metadata_example_multilingual, overwrite = TRUE)
