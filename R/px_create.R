# Creates new px-object from dataframe with basic metadata


# TODO:
# P1
# - create metadata parser:
#   - DONE add valid keywords
#    - check if supplied keywords+arguments are valid by creating a dataframe control?
#    - modify keywords

# skapa separat df för cellnotes och andra som refererar till variabler
# CELLNOTE: The values are given in the variable order indicated by STUB and HEADING, startingwith STUB
# DATANOTECELL

# - create function for creating matrix from stub+heading
# P2
# - create export function to convert metadata+data as text
# - create function for extracting metadata from px-object inside r and ourside r

# P3:
# - sort keywords according to pcaxis 2013
# - add various examples for creating new px files


# creates new px object

px_create <- function(
    .data, # mandatory. dataframe in long format with one column called 'value'
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
    note = NULL) {

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

  # Add VALUES keywords to metadata from .data
  new_meta <- px_add_values_from_data(new_meta, .data)

  # add timval values from .data
  new_meta <- add_timevals_from_data_to_value(new_meta, .data, "time")

  new_meta <- px_meta_add_keyword(new_meta, "TITLE", value = px_generate_dynamic_title(new_meta))

  # slutliga checks
  px_meta_validate(new_meta)

  stubvec <-  c(stub) |> str_split(",") |> unlist()
  headingvec <-  c(heading) |> str_split(",") |> unlist()
  stubheading <- c(stubvec, headingvec)

  data <- convert_data_to_final(new_meta, .data)
  print(data)

  return(list(metadata = new_meta,
              data = data))
}



px_obj <- px_create(.data = ex_data,
          stub = "sex,age",
          heading = "time",
          time_variable = "time",
          time_scale="annual",
          matrix = "TEST01",
          subject_area = "Test",
          subject_code = "T",
          units = "Antal timmar",
          contents = "Genomsnitt antal tittartimmar av The Simpsons",
          decimals = 1
)

px_obj




# TODO
px_meta_get_stub <- function(.metadata_df) {
  .metadata_df
}

data_to_matrix <- function(data, stubvec) {
  m <- data |>
    select(-all_of(stubvec)) |>
    as.matrix()
  colnames(m) <- NULL
  m
}

matrix_to_text <- function(m) {
  apply(format(m), 1, paste, collapse=" ")
}

getwd()
xx <- px_obj$data |> data_to_matrix(stubvec)
matrix_text <- xx |> matrix_to_text()


meta_lines <- px_obj$metadata |> px_parse_metadata() |> pull(s)
# skapa px filen med en rad per keyword + matrisen längst ner
c(meta_lines, "DATA=", matrix_text, ";") |> write_lines("text.px")




# TODO
px_write <- function(.px_obj) {
  meta_lines <- px_obj$metadata |> px_parse_metadata() |> pull(s)
  c(meta_lines, "DATA=", matrix_text, ";") |> write_lines("text.px")
}




