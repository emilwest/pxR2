library(tidyverse)
library(Rcpp)

sourceCpp("R/read-file.cpp")

txt <- read_file_cpp2(path = "data/TEMP02.px")

splitted <- str_split(txt, ";")

splitted


