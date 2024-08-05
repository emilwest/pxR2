#include <Rcpp.h>
#include <fstream>
#include "px_parse_meta_string.h"
using namespace Rcpp;


// helper
std::string extractLastNChars(std::string const &str, unsigned int n) {
  if (str.size() < n) {
    return str;
  }
  return str.substr(str.size() - n);
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
// reads file into vector of strings to avoid opening filestream multiple times
// if only_meta=true, the data-part is ignored and metadata is extracted only
// [[Rcpp::export]]
std::vector<std::string> preprocess_file(const std::string& filePath, bool only_meta=false) {

  std::ifstream file(filePath);

  if (!file.is_open()) {
    stop("Could not open file");
  }
  std::string line;
  std::string sb; // for appending string that continue over multiple lines.

  std::vector<std::string> meta_string;
  std::string lastchar;
  bool multiline = false;
  int linenr = 0;
  bool in_data = false;

  while (std::getline(file, line)) {

    // Skip empty lines
    // line.find_first_not_of(' ') == std::string::npos checks if the line contains only whitespace characters.
    if (line.empty() || line.find_first_not_of(' ') == std::string::npos) {
      continue;
    }

    if (line.compare("DATA=") == 0) {
      if (only_meta) break;
      // Rcpp::Rcout << "found data\n";
      in_data = true;
    }

    // check if first char is " and remove it
    // this indicates that a value has continued into the next line, i.e. multiline.
    // the multiline will be merged into a single line so all intermediate " needs to be removed.
    if (multiline && line.at(0) == '"' && !in_data) {
      // if lastchar was " on previous line, remove " on next line
      if (lastchar == "\"") {
        line.erase(0,1);
      }
    }

    lastchar = extractLastNChars(line, 1);

    if(lastchar != ";" && !in_data) {
      multiline = true;
      if (lastchar == "\"") {
        line.pop_back(); // removes last char
      }
      sb += line;

      //Rcpp::Rcout << line << '\n';
    }
    else {
      sb += line;
      meta_string.push_back(sb);
      sb.clear();
      multiline = false;
    }
    linenr++;
  }

  file.close();
  return meta_string;

}




// 1) read entire file once into std::vector<std::string> to avoid re-opening filestream using preprocess_file()
// 2) parse metadata lines using px_parse_meta_string()
// 3) parse data lines into vector and re-create into a dataframe later
// currently support matrix/cube data
// 4) return result as a list of parsed metadata and data

//' Parse a px file
//'
//' This function read a px file, parsing data and metadata and warnins if
//' the PX file is malformatted.
//'
//' @param filePath path to PX file ending in .px or .PX
//' @param only_meta Boolean, set to true if you only want to extract the metadata.
//' @param debug Boolean, set to true if you want to debug the parser step-by-step.
//' @return Returns a list of metadata and data.
//' @export
// [[Rcpp::export]]
List px_parse(const std::string& filePath, bool only_meta=false, bool debug=false) {

  std::vector<std::string> lines = preprocess_file(filePath, only_meta);
  bool in_data=false;
  List metalist;
  std::vector<std::string> datavec;
  int parsedValues = 0;

  // std::vector<std::string> keywords;
  // std::vector<std::string> languages;
  // std::vector<std::string> subkeys;
  // std::vector<std::string> values;


  for(unsigned int i = 0; i < lines.size(); i++) {
    if (debug) {
      Rcpp::Rcout << "Processing line: " << i << "\n";
    }
    const std::string& s = lines[i];

    if (s == "DATA=") {
      if (only_meta) break;
      if (debug) Rcpp::Rcout << "In data \n";
      in_data=true;
      continue;
    }

    if (!in_data) {
      List x = px_parse_meta_string(s, debug);

      // keywords.push_back(x["keyword"]);
      // languages.push_back(x["language"]);
      // subkeys.push_back(x["subkeys"]);
      // values.push_back(x["values"]);

      DataFrame df = DataFrame::create(Named("keyword")=x["keyword"],
                                       Named("language")=x["language"],
                                       Named("subkeys")=stringvec_to_string(x["subkeys"]),
                                       Named("values")=stringvec_to_string(x["values"])
      );
      metalist.push_back(df);
    } else {
      std::istringstream iss(s);  // Create an input string stream from the string
      std::string token;
      while (iss >> token) {      // Extract tokens delimited by whitespace
        if (token == ";") {
          break; //ignores final ;
        }
        datavec.push_back(token);
        parsedValues++;
      }
    }
  }

  // DataFrame df = DataFrame::create(Named("keyword", keywords),
  //                                  Named("language", languages),
  //                                  Named("subkeys", subkeys),
  //                                  Named("values", values)
  // );

  if (debug) Rcpp::Rcout << "Parsed values: " << parsedValues << "\n";

  return List::create(
    Named("meta") = metalist,
    Named("datavec") = datavec
  );

}




/*** R
x <- preprocess_file("inst\\extdata\\TEMP02.px", only_meta = T)

iconv(x, from = "iso-8859-1",to = "UTF-8")

x <- px_parse("inst\\extdata\\TEMP02.px")
# x$meta |> dplyr::bind_rows() |> tibble::as_tibble() |> View()
x <- px_parse("inst/extdata/WORK02.px")


#px_extract_meta_strings("inst/extdata/WORK02.px")


#x <- px_parse("\\\\ivo.local\\Users\\Home$\\emwe\\Downloads\\CHIL03.px")
#x <- px_parse("\\\\ivo.local\\Users\\Home$\\emwe\\Downloads\\scb.px")
*/
