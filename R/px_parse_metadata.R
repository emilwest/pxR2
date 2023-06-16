
# helper
addquotes <- function(txt) {
  Q <- '"'
  str_c(Q, txt, Q)
}

# helper
splitlist <- function(txt) {
  str_split_1(txt, ",") %>%
    addquotes() %>%
    str_c(collapse=",")
}



px_parse_metadata <- function(.metadata_df) {

  Q <- '"'
  E <- ";"
  e <- "="
  comma <- ","
  oq <- "("
  eq <- ")"

  .metadata_df %>%
    dplyr::mutate(value_parsed = map_chr(value, splitlist)) %>%
    dplyr::mutate(s = ifelse(is.na(varname) & is.na(valname),
                      str_c(keyword, e, value_parsed, E),
                      NA
    ),

    s = ifelse(!is.na(varname) & is.na(valname),
               str_c(keyword, oq, Q, varname, Q, eq, e, value_parsed, E),
               s

    ),

    s = ifelse(!is.na(varname) & !is.na(valname),
               str_c(keyword, oq, Q, varname, Q, comma, Q, valname, Q, eq, e, value_parsed, E),
               s


    ),

   # TIMEVAL(”time”)=TLIST(A1), ”1994”, ”1995”,"1996”
    s = ifelse(keyword == "TIMEVAL",
               str_c("TIMEVAL", oq, Q, varname, Q, eq, "=TLIST(", valname, "), ", value_parsed, E),
               s

               )

    ) |>
    dplyr::select(s)

}
