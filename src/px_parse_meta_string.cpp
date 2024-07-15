
// [[Rcpp::depends(BH)]]

#include <Rcpp.h>
#include <fstream>


//std::locale loc = boost::locale::gen("en_US.UTF-8");

//' Parse a single px-formatted string.
//'
//' This function parses a string in px-format and returns an error if the format
//' is malformatted.
//'
//' @param line A string in PX-format with at least a keyword and value.
//' @param debug Boolean, set to true if you want to debug the parser step-by-step.
//' @return Returns a parsed list with four elements: keyword, language, subkeys and values.
//' @export
//' @examples
//' px_parse_meta_string("VALUENOTE[sv](\"Norway\")=\"Break in time series\";")
//' px_parse_meta_string("NOTE=\"Preliminary data\";")
//' px_parse_meta_string("CODEPAGE=\"UTF-8\";")
// [[Rcpp::export]]
Rcpp::List px_parse_meta_string(const std::string& line, bool debug=false) {

  const char FNUTT = '\"';
  const char START_LANGUAGE = '[';
  const char END_LANGUAGE = ']';
  const char START_SUBKEY = '(';
  const char END_SUBKEY = ')';
  const char START_VALUES = '=';
  const char END_VALUES = ';';
  const char COMMA = ',';
  const char COLON = ':';
  const char MINUS = '-';

  bool hasLanguage = false;
  bool hasSubkeys = false;
  bool InFnutt = false;
  //bool _keepRunning = true;

  std::string keyword, language;
  std::vector<std::string> values, subkeys;

  enum ParserState {
    ReadKeyword = 0,
    ReadLanguage = 1,
    ReadSubkeys = 2,
    ReadValues = 3
  };

  ParserState state = ParserState::ReadKeyword;

  // used for parsing VALUES i.e. everything inside ="";
  enum ValueType {
    NotSet = 0,
    FnuttValue = 1,
    NoFnuttValue = 2,
    ErrorValue = 3
  };

  ValueType valueType = ValueType::NotSet;

  std::string sb; // where we save characters to
  std::string errorhelp; // error help string in the value parser

  //std::cout << line.size();

  // cf https://stackoverflow.com/questions/48545330/c-fastest-way-to-read-file-line-by-line
  //std::ifstream infile(infilename);
  //std::string line;

  for(char c : line) {
    if (c == FNUTT) {
      InFnutt = !InFnutt;
    }

    if (keyword == "DATA") {
      break;
    }

    // switch through the four states:
    // ReadKeyword, ReadLanguage, ReadSubkeys, ReadValues
    switch (state) {

    case ParserState::ReadKeyword:
      //std::cout << "inside readkeyword:" << state << '\n';

      if (debug) {
        // https://www.tidyverse.org/blog/2023/03/cran-checks-compiled-code/#warning-regarding-the-use-of-codesprintfcode-in-cc
        std::cout << "c: " << c << " InFnutt: " << InFnutt <<
          " keyword " << keyword <<
          " language: " << language <<
            " subkeys.size(): " << subkeys.size() <<
              " values.size(): " << values.size() <<
            " state: " << state <<
              '\n';
      }

      switch(c) {
        case FNUTT:
          break;
        case START_LANGUAGE:
          if (hasSubkeys) {
            // Rcpp-exception
            throw Rcpp::exception("Invalid PX file", false);
          }
          keyword = sb;
          sb.clear();
          state = ParserState::ReadLanguage; // new state
          hasLanguage = true;
          break;

        case START_SUBKEY:
          if (!hasLanguage) {
            keyword = sb;
          }
          sb.clear();
          state = ParserState::ReadSubkeys;
          hasSubkeys = true;
          break;

        case START_VALUES:
          if (!(hasSubkeys || hasLanguage)) {
            keyword = sb;
          }
          values.clear();
          sb.clear();
          state = ParserState::ReadValues;
          if (keyword == "DATA") {
            //_keepRunning = false;
          }

          break;

        case END_VALUES:
          throw Rcpp::exception("Invalid PX file, keyword cannot end with ;", false);
        case END_LANGUAGE:
          throw Rcpp::exception("Invalid PX file, keyword cannot end with ]", false);
        case END_SUBKEY:
          throw Rcpp::exception("Invalid PX file, keyword cannot end with )", false);

        default:
          // here is where characters are added to the keyword
          if (InFnutt) {
            sb += c;
          }
          else {
            if (!std::isspace(c)) {
              sb += c;
            }
          }
      }

      break; // end ParserState::ReadKeyword

    case ParserState::ReadLanguage:

      if (debug) {
        std::cout << "c: " << c << " InFnutt: " << InFnutt <<
          " language: " << language <<
            " state: " << state <<
              '\n';
      }

      if (std::isspace(c)) {
        throw Rcpp::exception("Invalid PX file, remove whitespace from inside the language tags []", false);
      }
      switch (c) {
        case START_VALUES:
        case START_SUBKEY:
        case START_LANGUAGE:
        case END_VALUES:
        case END_SUBKEY:
        case FNUTT:
          throw Rcpp::exception("Invalid PX file, language tags must end with ], for example VALUENOTE[sv]", false);
        case END_LANGUAGE:
          language = sb;
          sb.clear();
          state = ParserState::ReadKeyword; // back to readkeyword
          break;
        default:
          sb += c;
      }
      break; // end ParserState::ReadLanguage

    case ParserState::ReadSubkeys:

      if (debug) {
        std::cout << "c: " << c << " InFnutt: " << InFnutt <<
          " subkeys.empty(): " << subkeys.empty() <<
            " state: " << state <<
            '\n';
      }

      switch (c) {
      case FNUTT:
        // if (InFnutt) {
        //   sb += c;
        // }
        break;
      case COMMA:
        if (InFnutt) {
          sb += c;
        }
        else {
          // comma means a new subkeys comes after, so add and clear sb for parsing next subkeys
          if (debug) {
            std::cout << "added: " << sb << " to subkey"
                      << '\n';
          }
          subkeys.push_back(sb);
          sb.clear();
          state = ParserState::ReadSubkeys;
        }
        break;
      case END_SUBKEY:
        if (InFnutt) {
          sb += c;
        }
        else {
          //subkey = sb;

          // kontrollerar så att fall som CELLNOTE("age","gender",)="...";
          // inte lägger till det sista , som subkey
          //if (sb[sb.length()] != ',') {
          if (sb.length() > 0) {
            if (debug) {
              std::cout << "added: " << sb << " to subkey, all subkeys read."
                        << '\n';
            }

            subkeys.push_back(sb);
            sb.clear();
          } else {
            if (debug) {
              std::cout << "did not add: " << sb <<
                " to subkey, since sb is empty. Probably the last character was a , before END_SUBKEY." <<
                  " Only subkeys within quotes will be added." << '\n';
            }
          }
          state = ParserState::ReadKeyword;
        }
        break;
      default:
        if (InFnutt) {
          sb += c;
        }
        else {
          if (!std::isspace(c)) {
            sb += c;
          }
        }
      }
      break; // end ParserState::ReadSubkeys

    case ParserState::ReadValues:

      if (debug) {
        std::cout << "c: " << c << " InFnutt: " << InFnutt <<
          " valueType: " << valueType <<
            " values.empty(): " << values.empty() <<
            '\n';
      }


      // todo hantera ErrorValue
      if (!values.empty() && valueType == ValueType::ErrorValue) {
        // There are errors in values -> Read to ; and send error
        if (debug) {
          std::cout << "ErrorValue detected: ";
        }

        if (c == END_VALUES) {
          // convert values vector to string
          std::string s;
          for (const auto &piece : values) s += piece;
          std::string ss;
          for (const auto &piece : subkeys) ss += piece;

          Rcpp::String error_message = std::string("Failed parsing value at  keyword = '") + keyword +
            std::string("', language = '") + language +
            std::string("', subkey = '") + ss +
            std::string("'.\nCurrent parsed value string: '") + sb +
            std::string("'.\nAll parsed values before: ") + s +
            std::string("'.\nAdditional error message: \n") +
            errorhelp
            ;
          values.clear();
          stop(error_message);
        }
      }
      else {
        if (InFnutt) {
          switch (valueType) {
          // NotSet first by default
          case ValueType::NotSet:
            valueType = ValueType::FnuttValue;
            if (debug) {
              std::cout << "<Set valueType to FnuttValue (1) due to InFnutt true (1)>\n";
            }

            if (c != FNUTT) {
              sb += c;
            }
            break;
          case ValueType::FnuttValue:
            if (c != FNUTT) {

              if (c == END_VALUES && !values.empty() && values.size() > 1) {
                throw Rcpp::exception("Invalid PX file, value opens with \" and ends with ; but does not have a closing \" before ;", false);
              }
              if (keyword == "TIMEVAL" && c == END_VALUES && !values.empty()) {
                throw Rcpp::exception("Invalid PX file, timeval value opens with \" and ends with ; but does not have a closing \" before ;", false);
              }

              // lägg in check här? om values.empty() och
              // c == END_VALUES och
              // sb[sb.length() - 1] != FNUTT, ge error om att closing " saknas?

              sb += c;
            }
            break;
          case ValueType::NoFnuttValue:
            valueType = ValueType::ErrorValue;
            if (debug) {
              std::cout << "<Set valueType to ErrorValue (3) due to InFnutt true (1) despite current ValueType::NoFnuttValue>\n";
            }
            errorhelp = "The parser encountered an error because the current state "
            "says it is inside an open quote \" despite the valuetype indicating it isn't. \n"
            "Please double check the format. \n";

            break;
          case ValueType::ErrorValue:
            break;
          }
        } else {
          if (std::isspace(c) || c == FNUTT) {
            break;
          }
          switch(c) {
          case START_LANGUAGE:
          case START_VALUES:
          case END_LANGUAGE:
            throw Rcpp::exception("Invalid PX file, malformatted values", false);
          case COMMA:
            // comma means a new value comes after, so add and clear sb for parsing next value
            values.push_back(sb);
            sb.clear();
            // Some keywords (e.g. TIMEVAL) may have values of both type
            // FnuttValue and NoFnuttValue. Reset ValueType between values
            // to handle this
            valueType = ValueType::NotSet;
            if (debug) {
              std::cout << "<Resetted valuetype to NotSet (0) due to COMMA>\n";
            }
            break;
          case COLON:
            // Hierarchy value
            sb += c;
            break;
          case MINUS:
            // timevals can also be written as
            // TIMEVAL(”time”)=TLIST(A1, ”1994”-”1996”);
            // instead of
            // TIMEVAL(”time”)=TLIST(A1), ”1994”, ”1995”,"1996”;
            if (keyword == "TIMEVAL") {
              // let it be as is for the moment,
              // the value will be parsed as 1994-1996
              // do the rest of the parsing in R
              sb += c;
            }
            else {
              valueType = ValueType::ErrorValue;
              if (debug) {
                std::cout << "<Set valueType to ErrorValue (3) due to MINUS character in value and not being TIMEVAL>\n";
              }
              errorhelp = "The parser encountered an error because the current state "
              "says it detected a MINUS ('-') between two values, which is not allowed unless the keyword is a TIMEVAL. \n"
              "For example, this is allowed: TIMEVAL(\"time\")=TLIST(A1, \"1994\"-\"1996\"); \n"
              "Solution: replace '-' with ',' or check format. \n";

              std::string ss;
              for (const auto &piece : subkeys) ss += piece;

              Rcpp::String error_message = std::string("Failed parsing value at  keyword = '") + keyword +
                std::string("', language = '") + language +
                std::string("', subkey = '") + ss +
                std::string("'.\nCurrent parsed value string: '") + sb +
                std::string("'.\nAdditional error message: \n") +
                errorhelp
                ;
              Rcpp::stop(error_message); // stop here since no values are added to vector if it contains -
            }
            break;
          case END_SUBKEY:
            if (keyword == "TIMEVAL") {
              // Timeval in interval format
              sb += c;
            }
            else {
              valueType = ValueType::ErrorValue;
              if (debug) {
                std::cout << "<Set valueType to ErrorValue (3) due to detected END_SUBKEY despite keyword not being TIMEVAL>\n";
              }
              errorhelp = "The parser encountered an error because the current state "
              "says it detected an END_SUBKEY (')') between two values, which is not allowed unless the keyword is a TIMEVAL \n."
              "Solution: remove ')' or check format. \n";
            }
            break;
          case END_VALUES:
            // save string to values
            values.push_back(sb);
            sb.clear();
            //state = ParserState::ReadKeyword;
            break;

          default:
            // things with no fnuttar in values
            switch (valueType) {
            case ValueType::NotSet:
              valueType = ValueType::NoFnuttValue;
              if (debug) {
                std::cout << "<Set valuetype to NoFnuttValue (2) due to no FNUTT detected>\n";
              }
              if (values.empty()) {
                sb += c;
              }
              break;
            case ValueType::NoFnuttValue:
              if (values.empty()) {
                sb += c;
              }
              break;
            case ValueType::FnuttValue:
              valueType = ValueType::ErrorValue;
              if (debug) {
                std::cout << "<Set valueType to ErrorValue (3) due to ValueType::FnuttValue (1) despite InFnutt false (0)>\n";
              }
              errorhelp = "The parser encountered an error because the current state "
              "says it is not inside quotes \", but the valuetype says it is. \n."
              "This is probably due to that previous value closed with \" but with no comma (,) afterwards. \n"
              "Solution: A \" and/or , is missing between the values, please add it. \n";

              break;
            case ValueType::ErrorValue:
              break;
            }
          }
        }
      }
      break; // end ParserState::ReadValues
    }
  }

  if (values.size() == 0 && keyword != "DATA") {
    Rcpp::stop("A value could not be found, check that there is not any missing closing quote \" at the end.");
  }
  if (keyword.size() == 0) {
    Rcpp::stop("A keyword could not be found, check the format of the metadata.");
  }
  return Rcpp::List::create(Rcpp::Named("keyword") = keyword,
                                    Rcpp::Named("language") = language,
                                    Rcpp::Named("subkeys") = subkeys,
                                    Rcpp::Named("values") = values
  );
}



// tests
/*** R

# devtools::load_all()
# todo fixa denna
px_parse_meta_string("CELLNOTE[sv](\"kön\", \"*\", \"*\", \"ålder\",)=\"Data not applicable\";")
px_parse_meta_string("CELLNOTE[sv](kön, *, *, ålder,)=\"Data not applicable\";")
px_parse_meta_string("CHARSET=\"ANSI\";")
px_parse_meta_string("CHARSET[en]=\"ANSI\";")

# detta ska returnera DATA så att parsern vet när den ska stanna i loopen sen
px_parse_meta_string("DATA=")


px_parse_meta_string("VALUENOTE[sv](\"Norway\")=\"Break in time series\";")
px_parse_meta_string("VALUENOTE[sv](\"Norway\",\"Oslo\")=\"Break in time series\";", T)
px_parse_meta_string("ELIMINATION(\"kön\")=YES;")

px_parse_meta_string("CELLNOTE(\"kön\", \"*\", \"*\", \"ålder\")=\"Data not applicable\";")
px_parse_meta_string("CELLNOTE(\"kön\", \"*\", \"*\", \"Åland\")=\"Data not applicable\";")
px_parse_meta_string("CELLNOTE[sv](\"kön\", \"*\", \"*\", \"ålder\",)=\"Data not applicable\";")

px_parse_meta_string("STUB=\"age\",\"sex\",\"gender\";", T)
px_parse_meta_string("SOURCE=\"Statistics Sweden#Statistics Finland\";")

px_parse_meta_string("VALUES(\"age\")=\"0-19\",\"20-39\",\"40-100\";",T)
px_parse_meta_string("CODES(\"age\")=\"0-19\",\"20-39\",\"40-100\";")

px_parse_meta_string("KEYS(\"age\")=\"VALUES\"\n\"hej\";")
px_parse_meta_string("CELLNOTE(\"kön\", \"*\", \"*\", \"ålder\")=\"Data not applicable;hej\";")

# ok
px_parse_meta_string("TIMEVAL(\"år\")=TLIST(A1),\"1968\",\"1969\",\"1970\";")
px_parse_meta_string("TIMEVAL(\"år\")=TLIST(A1),\"1968\"-\"1970\";")


*/


// borde faila men gör inte det

/***R
px_parse_meta_string("TIMEVAL(\"år\")=TLIST(A1,\"1968\"-\"1970\")-;")
*/
