# intern fil som används för package building

# install.packages("devtools")
#install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
library(devtools)
library(usethis)
# https://r-pkgs.org/whole-game.html


use_git()
load_all()
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

load_all()
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


usethis::use_logo(img = "man/figures/pxR2_original.png")

usethis::use_tidy_dependencies()

usethis::use_package("dplyr", type = "Imports", min_version = "1.1.2")

?dplyr




