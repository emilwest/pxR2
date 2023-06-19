

#' Helper for metadata parser, adds quotes around values in a string
#'
#' @param txt Text string to add quotes around
#'
#' @return Text string with added quotes
#' @export
#'
#' @seealso [splitlist()] for adding quotes to each element in a vector and joining to single string, and
#'  [px_parse_metadata()] for parsing metadata to text
#'
#' @examples
#' addquotes('2021')
#' addquotes(c("2021","2022","2023"))
#'
addquotes <- function(txt) {
  Q <- '"'
  str_c(Q, txt, Q)
}


#' Helper for metadata parser, splits a string to vector, adds quates and joins back with commas.
#'
#' @param txt Text vector with comma-separated values, e.g. '2021,2022,2023'
#'
#' @return Returns texts vector with added quotes around values, e.g '"\"2021\",\"2022\",\"2023\""'
#' @export
#'
#' @seealso [addquotes()], [px_parse_metadata()]
#'
#' @examples
#' splitlist('2021,2022,2023')
#'
splitlist <- function(txt) {
  str_split_1(txt, ",") %>%
    addquotes() %>%
    str_c(collapse=",")
}


#' Parse px metadata tibble to px-formatted text.
#'
#' @param .metadata_df Metadata tibble as input to be converted to px-formatted text.
#'
#' @return Px-formatted text
#' @export
#'
#' @examples
#'
#' px_parse_metadata(meta_example)
#'
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
