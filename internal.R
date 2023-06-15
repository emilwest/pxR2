# intern fil som används för package building

# install.packages("devtools")
#install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
library(devtools)

# https://r-pkgs.org/whole-game.html

use_git()
load_all()
hello()
check()
package?readr
package?ggplot2
use_gpl_license()
document()

# library(pxR2)
# helloe()
use_testthat()
use_test("helloe") # skapar testfil för funktionen helloe

library(testthat)
load_all()
test()


# use_package("stringr")
use_github()
use_readme_rmd()

usethis::use_test("px_create")
usethis::use_test("px_meta_handle")
usethis::use_test("px_meta_validate")

