# Creates new px-object from dataframe with basic metadata

# TODO:
# P1
# - create metadata parser:
#   - DONE add valid keywords
#    - DONE check if supplied keywords+arguments are valid by creating a dataframe control?
#    - DONE modify keywords

# skapa separat df för cellnotes och andra som refererar till variabler
# CELLNOTE: The values are given in the variable order indicated by STUB and HEADING, startingwith STUB
# DATANOTECELL

# - DONE create function for creating matrix from stub+heading
# P2
# - DONE create export function to convert metadata+data as text
# - create function for extracting metadata from px-object inside r and ourside r

# P3:
# - DONE sort keywords according to pcaxis 2013
# - add various examples for creating new px files



#' Read metadata csv raw file into a tibble
#'
#' This is a wrapper around [readr::read_delim()] which checks that the csv is valid:
#'
#' - ; separated
#' - must contain the columns: keyword, language, varname, valname and value
#'
#' @param path path to csv file
#' @param delim delimiter, semicolon by default
#' @param encoding utf-8 by default
#'
#' @return returns metadata tibble
#' @export
#'
#' @examples
#' try(px_read_meta_csv("metadata.csv"))
#' try(px_read_meta_csv("inst/extdata/metadata_example.csv"))
px_read_meta_csv <- function(path, delim = ";", encoding = "UTF-8") {
  assertthat::assert_that(substring(path, nchar(path)-2)=="csv", msg = "The path must be a csv with ; as separator")
  csv <- readr::read_delim(path,
                    delim = delim,
                    col_types = readr::cols(.default = "c"),
                    locale = readr::locale(encoding = encoding)
  )
  col_order <- c("keyword", "language", "varname", "valname", "value")
  assertthat::assert_that(all(names(csv) %in% col_order), msg = "The metadata csv must contain the columns: keyword, language, varname, valname and value")
  dplyr::select(csv, dplyr::all_of(col_order))
}





#' Creates new px object from a tibble/data frame.
#' Creates all necessary metadata and generates VALUE keywords dynamically based on the variable levels in the data frame.
#'
#' @param .data Mandatory. Dataframe/tibble in long format with one column called 'value'.
#' @param meta_csv_path Path to metadata csv (;-delimited)
#' @param codes_csv_path Path to codes csv (;-delimited)
#' @param stub Mandatory. Variables to display along the rows. Supply as a comma separated string, for example 'country,age,'
#' @param heading Mandatory. Variables to display along the columns.
#' @param time_variable Name of the time variable, if it exists.
#' @param time_scale If time_variable exist, you must set the time scale. Choose from 'annual', 'halfyear', 'quarterly', 'monthly' or 'weekly'.
#' Choose annual format if the time variable is formatted as CCYY (C for century, Y for year).
#' Choose halfyear format if the time variable is formatted as CCYYH (H is 1 or 2).
#' Choose quarterly format if the time variable is formatted as CCYYQ (Q is 1-4).
#' Choose monthly format if the time variable is formatted as CCYYMM (M is 1-12).
#' Choose weekly format if the time variable is formatted as CCYYWW (M is 1-52).
#' @param matrix Mandatory. The name of the matrix, should be the same as the file name, typically in uppercase. Max length 20 characters.
#' @param subject_area Mandatory. The name of the subject are the matrix should be categorized under.
#' @param subject_code Mandatory. Code for the subject are. Typically upper case acronym, max length of 20 characters.
#' @param units Mandatory. The specific type of units used in the table in plain text, for example: 'Number of applications', 'Ton', 'Index', etc.
#' @param contents Mandatory. The first part of a title excluding the variables used.
#' For example, if the title is 'Number of cats in Europe by country, age and time', the CONTENT is 'Number of cats in Europe'.
#' The last part of the title, 'by country, age, and time', will be automatically generated and written to TITLE.
#' @param decimals Mandatory. Integer between 0-15 (0-6 if SHOWDECIMALSis not included). Indicates how many decimals will be saved in the px-file.
#' @param showdecimals The number of decimals to be shown in the table, integer between 0-6.
#' Must be the same or smaller than the number stored as indicated by the keyword DECIMALS.
#' If SHOWDECIMALS is not stated in the file the number stated by DECIMALS will be used.
#' @param language The language for the px-file, 'sv' for Swedish, 'en' for English, etc.
#' If the keyword is used, the words for “and” and “by” are read from the text file of that language which is used to generate the TITLE.
#' @param charset CHARSET=”ANSI”; indicates that the texts in the file are written in Windows format.
#' If the keyword is missing it means that the texts in the file are in DOS format.
#' @param axis_version Version number for PC-Axis, deafault is the latest version 2013. Is read and saved but otherwise not used.
#' @param codepage Is used when creating XML format to get correct characters. Default iso8859-1. Max 20 chars
#' @param creation_date Date when file was created. Written in format CCYYMMDD hh:mm, e.g. ”19960612 14:20”. Is shown together with footnotes.
#' @param last_updated Date and time for latest update format CCYYMMDD hh:mm. Example ”19960528 11:35”.
#' @param contact States contact information for the matrix such as a email and is shown in the footnotes.
#' Can for example be written in the form of: name, organization, telephone, fax, e-mail.
#' Several persons can be stated in the same text string and are then divided by the #-sign.
#' @param source States the organization which is responsible for the statistics or the sources used. Is shown with the footnote.
#' If multiple sources are used, use # between each source. For example 'Statistics Sweden#Statistics Finland'.
#' @param note General footnote for the table.
#'
#' @return Returns a list with two entries, one containing the metadata information and one for the data.
#' @export
#'
#' @examples
#' px_obj <- px_create(.data = ex_data,
#'           stub = "sex,age",
#'           heading = "time",
#'           time_variable = "time",
#'           time_scale="annual",
#'           matrix = "TEST01",
#'           subject_area = "Test",
#'           subject_code = "T",
#'           units = "Antal timmar",
#'           contents = "Genomsnitt antal tittartimmar av The Simpsons",
#'           decimals = 1,
#'           language = "en"
#' )
#'
#' px_obj
#'
px_create <- function(
    .data, #
    meta_csv_path = NULL,
    codes_csv_path = NULL,
    stub, # variables to display along the rows
    heading, # variables to display along the columns
    time_variable=NULL, # mandatory if time variable exist (TLIST)
    time_scale=NULL,
    matrix, # mandatory
    subject_area, # mandatory
    subject_code, # mandatory
    units, # mandatory
    contents, # mandatory (the title)
    decimals = 1, # mandatory
    showdecimals = 1,
    language = "en",
    charset = "ANSI",
    axis_version = "2013",
    codepage = "iso-8859-15",
    creation_date = format(Sys.time(), "%Y%m%d %H:%M"),
    last_updated = format(Sys.time(), "%Y%m%d %H:%M"),
    contact = NULL,
    source = NULL,
    note = NULL
    ) {

  if (!is.null(meta_csv_path)) {

    new_meta <- px_read_meta_csv(meta_csv_path)
    # print("in")

  } else {
    new_meta <- px_meta_init_empty()
    new_meta <- px_meta_add_keyword(new_meta, "STUB", value = stub)
    new_meta <- px_meta_add_keyword(new_meta, "HEADING", value = heading)
    new_meta <- px_meta_add_timeval(new_meta, time_variable = time_variable, time_scale = time_scale)
    new_meta <- px_meta_add_keyword(new_meta, "MATRIX", value = matrix)
    new_meta <- px_meta_add_keyword(new_meta, "SUBJECT-AREA", value = subject_area)
    new_meta <- px_meta_add_keyword(new_meta, "SUBJECT-CODE", value = subject_code)
    new_meta <- px_meta_add_keyword(new_meta, "UNITS", value = units)
    new_meta <- px_meta_add_keyword(new_meta, "CONTENTS", value = contents)
    new_meta <- px_meta_add_keyword(new_meta, "DECIMALS", value = as.character(decimals))
    new_meta <- px_meta_add_keyword(new_meta, "SHOWDECIMALS", value = as.character(showdecimals))
    new_meta <- px_meta_add_keyword(new_meta, "LANGUAGE", value = language)
    new_meta <- px_meta_add_keyword(new_meta, "CHARSET", value = charset)
    new_meta <- px_meta_add_keyword(new_meta, "AXIS-VERSION", value = axis_version)
    new_meta <- px_meta_add_keyword(new_meta, "CODEPAGE", value = codepage)
    new_meta <- px_meta_add_keyword(new_meta, "CREATION-DATE", value = creation_date)
    new_meta <- px_meta_add_keyword(new_meta, "LAST-UPDATED", value = last_updated)

    if (!is.null(contact)) new_meta <- px_meta_add_keyword(new_meta, "CONTACT", value = contact)
    if (!is.null(source)) new_meta <- px_meta_add_keyword(new_meta, "SOURCE", value = source)
    if (!is.null(note)) new_meta <- px_meta_add_keyword(new_meta, "NOTE", value = note)
  }
  #print("out")

  # <Dynamically generated>
  # Add VALUES keywords to metadata from .data
  new_meta <- px_add_values_from_data(new_meta, .data)
  #print("out2")

  # add timval values from .data
  if (is.null(time_variable)) {
    l <- length(px_get_languages(new_meta))
    lang <- px_get_main_language(new_meta)
    if (l > 1) {
      time_variable <- new_meta |> filter(keyword=="TIMEVAL" & language == lang) |> pull(varname)
    } else if (l==1) {
      time_variable <- new_meta |> filter(keyword=="TIMEVAL" & (language == lang | is.na(language))) |> pull(varname)
    } else {
      warning("LANGUAGE is missing from px file.")
      time_variable <- new_meta |> filter(keyword=="TIMEVAL" & is.na(language)) |> pull(varname)
    }
  }

  new_meta <- add_timevals_from_data_to_value(new_meta, .data, time_variable)
  #print("ou3")

  # add dynamic title (multilingual)
  dynamic_title <- dplyr::as_tibble(px_generate_dynamic_title(new_meta))
  new_meta <- bind_rows(new_meta, dynamic_title)
  # </Dynamically generated>

  #print("ou4")
  # final checks
  px_meta_validate(new_meta)
  #print("ou5")
  new_meta <- sort_metadata_keywords(new_meta)
  #print("ou55")

  return(list(metadata = new_meta,
              data = .data))
}




#
# px_obj <- px_create(.data = ex_data,
#           stub = "sex,age",
#           heading = "time",
#           time_variable = "time",
#           time_scale="annual",
#           matrix = "TEST01",
#           subject_area = "Test",
#           subject_code = "T",
#           units = "Antal timmar",
#           contents = "Genomsnitt antal tittartimmar av The Simpsons",
#           decimals = 1,
#           language = "en"
# )





