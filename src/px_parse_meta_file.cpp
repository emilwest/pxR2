// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <fstream>
#include "px_parse_meta_string.h"
#include <locale>

using namespace Rcpp;


// [[Rcpp::export]]
void hej() {
  if (const char* loc = std::setlocale(LC_ALL, "sv_SE.UTF-8"))
    std::wprintf(L"New LC_ALL locale: %s\n", loc);

  std::cout << "The default locale is " << std::locale().name() << '\n'
            << "The user's locale is " << std::locale("").name() << '\n';
}


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

//' Gets encoding from px file in CODEPAGE keyword.
//'
//' If CODEPAGE is not found within the first 100 lines
//' or until DATA keyword is found,
//' the encoding will be guessed with charset detection.
//'
//' @param path File path to px file
//'
//
// static std::string get_encoding(const std::string& path) {
//
//   std::string myencoding;
//
//   if (path.substr(path.size() - 3) == ".px") {
//     std::ifstream tr(path);
//     std::string line;
//     int lineCount = 1;
//
//     while (lineCount <= 100 && std::getline(tr, line) && line.find("DATA=") != 0) {
//       if (line.find("CODEPAGE=") == 0) {
//         myencoding = line.substr(line.find("\"") + 1);
//         myencoding = myencoding.substr(0, myencoding.rfind("\""));
//       }
//       lineCount++;
//     }
//   }
//
//   // std::string cs;
//   // const int BUFFER_SIZE = 1024;
//   // char buffer[BUFFER_SIZE];
//   // int size = 0;
//   // std::ifstream fs(path, std::ifstream::binary);
//   //
//   // if (fs.is_open()) {
//   //   Ude::CharsetDetector det;
//   //   size = std::min(BUFFER_SIZE, static_cast<int>(fs.tellg()));
//   //   fs.read(buffer, size);
//   //   det.Feed(buffer, size);
//   //   det.DataEnd();
//   //   cs = det.Charset;
//   // }
//   //
//   // if (cs.empty()) {
//   //   return "default"; // Replace with actual default encoding
//   // }
//   //
//   // // Fix it if it is ASCII; it is probably the codepage of the machine
//   // if (cs.compare("ASCII") == 0) {
//   //   return "default"; // Replace with actual default encoding
//   // }
//   //
//
//   return myencoding;
// }






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


hej()
#px_extract_meta_strings("inst/extdata/WORK02.px")


x <- px_parse_meta_file("\\\\ivo.local\\Users\\Home$\\emwe\\Downloads\\CHIL03.px")
x <- px_parse_meta_file("\\\\ivo.local\\Users\\Home$\\emwe\\Downloads\\scb.px")

# get_encoding("inst/extdata/WORK02.px")
# get_encoding("inst/extdata/TEMP02.px")
# x
# y <- tibble::tibble(dplyr::bind_rows(x))

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
