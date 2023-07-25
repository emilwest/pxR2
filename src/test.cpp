
#include <Rcpp.h>
#include <fstream>


// [[Rcpp::export]]
bool parse_px(std::string& line) {

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
  bool _keepRunning = true;

  std::string keyword;
  std::string language;
  std::string subkey;
  std::vector<std::string> values;


  enum ParserState {
    ReadKeyword = 0,
    ReadLanguage = 1,
    ReadSubkeys = 2,
    ReadValues = 3
  };

  ParserState state = ParserState::ReadKeyword;

  // used for parsing VALUES i.e. everything inside ="";
  enum ValueType {
    NotSet,
    FnuttValue,
    NoFnuttValue,
    ErrorValue
  };

  ValueType valueType = ValueType::NotSet;

  // std::map<std::string, ValueType> valueTypesMap = {
  //   {"NotSet", ValueType::NotSet},
  //   {"FnuttValue", ValueType::FnuttValue},
  //   {"NoFnuttValue", ValueType::NoFnuttValue},
  //   {"ErrorValue", ValueType::ErrorValue}
  // };


  std::string sb; // where we save characters to

  // cf https://stackoverflow.com/questions/48545330/c-fastest-way-to-read-file-line-by-line
  //std::ifstream infile(infilename);
  //std::string line;

  for(char& c : line) {
    //std::cout << c << state << '\n';

    if (c == FNUTT) {
      InFnutt = !InFnutt;
    }

    // switch through the four states:
    // ReadKeyword, ReadLanguage, ReadSubkeys, ReadValues
    switch (state) {

    case ParserState::ReadKeyword:
      //std::cout << "inside readkeyword:" << state << '\n';
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
            _keepRunning = false;
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
      switch (c) {
      case FNUTT:
        sb += c;
        break;
      case END_SUBKEY:





        if (InFnutt) {
          sb += c;
        }
        else {
          subkey = sb;
          sb.clear();

          // dubbelkollar att senaste tecken inte var , och tar bort detta
          // tex CELLNOTE(\"kön\", \"*\", \"*\", \"ålder\",) blir
          // tex CELLNOTE(\"kön\", \"*\", \"*\", \"ålder\")
          if (!subkey.empty() && subkey[subkey.length() - 1] == ',') {
            subkey = subkey.substr(0, subkey.length() - 1);
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
      // todo hantera ErrorValue
      if (InFnutt) {
        //std::cout << InFnutt;


        switch (valueType) {
          // NotSet first by default
          case ValueType::NotSet:
            valueType = ValueType::FnuttValue;
            if (c != FNUTT) {
              sb += c;
            }
            break;
          case ValueType::FnuttValue:
            if (c != FNUTT) {
              sb += c;
            }
            break;
          case ValueType::NoFnuttValue:
            valueType = ValueType::ErrorValue;
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
          case COLON:
            // Hierarchy value
            sb += c;
            break;
          case MINUS:
            // todo
            if (keyword == "TIMEVAL") {

            }
            else {

            }
          case END_SUBKEY:
            if (keyword == "TIMEVAL") {
              // Timeval in interval format
              sb += c;
            }
            else {
              valueType = ValueType::ErrorValue;
            }
            break;
          case END_VALUES:
            // todo
            // save string to values
            values.push_back(sb);

            // Remove start/ending FNUTT
            if (!subkey.empty() && subkey[0] == FNUTT) {
              subkey = subkey.substr(1);
            }
            if (!subkey.empty() && subkey[subkey.length() - 1] == FNUTT) {
              subkey = subkey.substr(0, subkey.length() - 1);
            }

          default:
            // things with no fnuttar in values
            switch (valueType) {
            case ValueType::NotSet:
              valueType = ValueType::NoFnuttValue;
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
              break;
            }




        }



      }



      //std::cout << c; //debug
      break; // end ParserState::ReadValues


    }



  }


  std::cout << "keyword:" << keyword << '\n';
  std::cout << "language:" << language << '\n';
  std::cout << "subkey:" << subkey << '\n';
  std::cout << "values:";
  for (auto element : values) {
    std::cout << element << " ";
  }
  std::cout << '\n';
  std::cout << "state:" << state << '\n';

  // while (std::getline(infile, line)) {
  //   //std::cout << line << '\n' << '\n';
  //   string::iterator it;
  //   it = line.begin();
  //   cout << * it << " ";
  //   it++;
  //   cout << * it;
  //   // if (line == 'DATA=\n') {
  //   //   break;
  //   // }
  //
  // }
  //infile.close();
  return true;
}



// tests
/*** R
parse_px("CELLNOTE[sv](\"kön\", \"*\", \"*\", \"ålder\",)=\"Data not applicable\";")

parse_px("CHARSET=\"ANSI\";")
parse_px("CHARSET[en]=\"ANSI\";")
parse_px("CHARSET[en=\"ANSI\";")
parse_px("CHARSET[en)=\"ANSI\";")
parse_px("CHARSET[en\")=\"ANSI\";")
parse_px("CHARSET;")
parse_px("CHARSET]")
parse_px("CHARSET)")

parse_px("VALUENOTE[sv](\"Norway\")=\"Break in time series\";")
parse_px("VALUENOTE[sv](\"Norway\",\"Oslo\")=\"Break in time series\";")
parse_px("ELIMINATION(\"kön\")=YES;")

parse_px("CELLNOTE(\"kön\", \"*\", \"*\", \"ålder\")=\"Data not applicable\";")
parse_px("CELLNOTE[sv](\"kön\", \"*\", \"*\", \"ålder\",)=\"Data not applicable\";")
*/
