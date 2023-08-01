#include <Rcpp.h>
#include <fstream>
#include "px_parse_meta_string.h"

using namespace Rcpp;

// helper
std::string stringvec_to_string(std::vector<std::string> stringvec) {
  std::string s;
  bool quoted=false;
  if (stringvec.size() > 1) quoted = true;

  for (const auto &piece : stringvec) {
    if (quoted) {
      s =  s + "\"" + piece + "\"";
    } else {
      s =  s + piece;
    }

    if (piece != stringvec.back()) {
      s += ",";
    }

  }
  return s;
}

// helper
std::string extractLastNChars(std::string const &str, unsigned int n) {
  if (str.size() < n) {
    return str;
  }
  return str.substr(str.size() - n);
}

// [[Rcpp::export]]
std::vector<std::string> px_extract_meta_strings(const std::string& infilename, bool debug=false) {

  std::ifstream infile(infilename);
  std::string line;
  std::string sb; // for appending string that continue over multiple lines.
  std::vector<std::string> res;
  std::string lastchar;
  bool multiline = false;
  int linenr = 0;

  while (std::getline(infile, line)) {
    if (line.compare("DATA=") == 0) {
      //std::cout << "found data\n";
      break;
    }

    // check if first char is " and remove it
    if (multiline && line.at(0) == '"') {
      // if lastchar was " on previous line, remove " on next line
      if (lastchar == "\"") {
        line.erase(0,1);
      }
    }

    lastchar = extractLastNChars(line, 1);

    if(lastchar != ";") {
      multiline = true;
      if (lastchar == "\"") {
        line.pop_back(); // removes last char
      }
      sb += line;

      //std::cout << line << '\n';
    }
    else {
      sb += line;
      res.push_back(sb);
      sb.clear();
      multiline = false;
    }
    linenr++;
  }

  if (debug) {
    std::cout << linenr;
  }
  infile.close();
  return res;
}




// [[Rcpp::export]]
Rcpp::List px_parse_meta_file(const std::string& infilename, bool debug=false) {
  std::vector<std::string> meta_strings;
  meta_strings = px_extract_meta_strings(infilename);
  List reslist;

  for(unsigned int i = 0; i < meta_strings.size(); i++) {
    if (debug) {
      std::cout << "line: " << i;
    }
    std::string s = meta_strings[i];

    try {
      List x = px_parse_meta_string(s);
      DataFrame df = DataFrame::create(Named("keyword")=x["keyword"],
                                       Named("language")=x["language"],
                                       Named("subkeys")=stringvec_to_string(x["subkeys"]),
                                       Named("values")=stringvec_to_string(x["values"])
                                       );
      reslist.push_back(df);
    }
    catch (const std::runtime_error &ex) {
      std::cerr << " (an error occured at line): " << i;
      throw ex;
    }
  }

  return reslist;
}




/*** R
# px_parse_meta_file("inst/extdata/TEMP02.px")
x <- px_parse_meta_file("inst/extdata/WORK02.px")
# x
# tibble::tibble(dplyr::bind_rows(x)) |> View()

# x[[1]] |> tibble::as_tibble()
# x[[1]] |> as.data.frame()
# library(tidyverse)
# x[[1]]
# enframe(x[[1]])
# map(x, ~ enframe(.x) |> group_by(name) |> mutate(value=str_c(value, collapse = ",")))
# stack(x[[1]])
#
# y <- x |>
#   map_if(~length(.x)>1, str_c)
#
# y[[1]]
# x[[1]]
# str(x)
# str(y)
#
# purrr::map(x, unlist)
#
# dplyr::bind_rows(x)
*/
