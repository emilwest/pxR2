## code to prepare `meta_example` dataset goes here

meta_example <- readr::read_csv2("inst/extdata/metadata_example.csv",  col_types = readr::cols(.default = "c"))
usethis::use_data(meta_example, overwrite = TRUE)
