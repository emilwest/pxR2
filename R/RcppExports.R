# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

hej <- function() {
    invisible(.Call(`_pxR2_hej`))
}

px_extract_meta_strings <- function(infilename, debug = FALSE) {
    .Call(`_pxR2_px_extract_meta_strings`, infilename, debug)
}

px_parse_meta_file <- function(infilename, debug = FALSE) {
    .Call(`_pxR2_px_parse_meta_file`, infilename, debug)
}

#' Parse a single px-formatted string.
#'
#' This function parses a string in px-format and returns an error if the format
#' is malformatted.
#'
#' @param line A string in PX-format with at least a keyword and value.
#' @param debug Boolean, set to true if you want to debug the parser step-by-step.
#' @return Returns a parsed list with four elements: keyword, language, subkeys and values.
#' @export
#' @examples
#' px_parse_meta_string("VALUENOTE[sv](\"Norway\")=\"Break in time series\";")
#' px_parse_meta_string("NOTE=\"Preliminary data\";")
#' px_parse_meta_string("CODEPAGE=\"UTF-8\";")
px_parse_meta_string <- function(line, debug = FALSE) {
    .Call(`_pxR2_px_parse_meta_string`, line, debug)
}

