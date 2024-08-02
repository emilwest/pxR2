#include <Rcpp.h>
#include <fstream>
using namespace Rcpp;



List parseMatrixToStringVector(std::ifstream& file) {
  std::vector<std::string> result;
  int parsedValues = 0;

  if (!file.is_open()) {
    stop("File stream is not open");
  }

  bool indata = false;
  std::string line;
  while (std::getline(file, line)) {
    // Skip empty lines
    // line.find_first_not_of(' ') == std::string::npos checks if the line contains only whitespace characters.
    if (line.empty() || line.find_first_not_of(' ') == std::string::npos) {
      continue;
    }

    if (line.compare("DATA=") == 0) {
       Rcpp::Rcout << "found data\n";
      indata = true;
      continue;
    }
    if (indata) {
      std::stringstream ss(line);
      std::string value;
      // The >> operator reads the next whitespace-delimited token from the stream ss and stores it in value.
      // The extraction operator returns the stream ss itself, which is then evaluated in the context of the while loop.
      // If the extraction is successful, the stream evaluates to true, and the loop continues.
      // If there are no more tokens to read, the stream evaluates to false, and the loop exits.
      while (ss >> value) {
        if (value == ";") {
          // to do: should I do this check?
          // if (file.peek() != EOF) {
          //   stop("; was detected but not before end of file. Make sure ; is at the end of the file.");
          // }
          break; //ignores final ;
        }


        result.push_back(value);
        parsedValues++;
      }
    }
  }


  Rcpp::Rcout << "Parsed values: " << parsedValues << "\n";
  file.close();
  return List::create(
    Named("values") = result,
    Named("count") = parsedValues
  );
}


// helper
std::string extractLastNChars(std::string const &str, unsigned int n) {
  if (str.size() < n) {
    return str;
  }
  return str.substr(str.size() - n);
}



std::vector<std::string> px_extract_meta_strings2(std::ifstream& file, bool debug=false) {
  std::string line;
  std::string sb; // for appending string that continue over multiple lines.
  std::vector<std::string> res;
  std::string lastchar;
  bool multiline = false;
  int linenr = 0;

  if (!file.is_open()) {
    stop("File stream is not open");
  }

  while (std::getline(file, line)) {
    if (line.compare("DATA=") == 0) {
      //Rcpp::Rcout << "found data\n";
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

      //Rcpp::Rcout << line << '\n';
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
    Rcpp::Rcout << linenr;
  }
  return res;
}



// [[Rcpp::export]]
List processFile(const std::string& filePath) {
  std::ifstream file(filePath);
  if (!file.is_open()) {
    stop("Could not open file");
  }
  std::string line;
  std::getline(file, line);
  std::vector<std::string> meta_strings = px_extract_meta_strings2(file);
  std::getline(file, line);
  List result = parseMatrixToStringVector(file);
  Rcpp::Rcout << "DONE! ";
  file.close();

  return List::create(
    Named("meta") = meta_strings,
    Named("datavec") = result["values"],
    Named("count") = result["count"]
  );
}



/*** R
x <- processFile("inst\\extdata\\TEMP02.px")
x
*/
