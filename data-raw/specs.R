## code to prepare `specs` dataset goes here

specs <- readr::read_delim("inst/extdata/px_format_specification", delim = " ", col_types = cols(.default = "c"))
usethis::use_data(specs, overwrite = TRUE)
