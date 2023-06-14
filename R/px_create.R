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



library(stringr)

library(tidyverse)
ex_data <- tibble(sex = c("Female", "Male", "Total"),
                  age = c("0-15", "16-25", "26-50"),
                  time = c("2021", "2022", "2023"),
                  value = c(NA, NA, NA)
) %>%
  complete(sex, age, time) %>%
  mutate(value = rnorm(27, mean = 20, sd = 5))



iris
x <- readr::read_csv2("data/metadata_example.csv",  col_types = cols(.default = "c"))

x
# creates new px object

# Todo behöver TITLE läggas till?
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

  # TODO:
  # add tlist values from timeval to values


  new_meta <- px_meta_add_keyword(new_meta, "TITLE", value = px_generate_dynamic_title(new_meta))

  # slutliga checks
  px_meta_validate(new_meta)

  return(new_meta)
}


px_create(.data = ex_data,
          stub = c("sex", "age"),
          heading = "time",
          time_variable = "time",
          time_scale="annual",
          matrix = "TEST01",
          subject_area = "Test",
          subject_code = "T",
          units = "Antal timmar",
          contents = "Genomsnitt antal tittartimmar av The Simpsons",
          decimals = 1
) %>% view()
  px_parse_metadata()



default_lang <- x %>%
  filter(keyword=="LANGUAGE") %>%
  pull(value)

contents <- x %>%
  filter(keyword=="CONTENTS") %>%
  pull(value)

x %>%
  filter(keyword=="STUB") %>%
  pull(value) %>%
  str_split_1(",") %>%
  str_c(collapse = ", ") %>%
  str_squish()

px_generate_dynamic_title <- function(.metadata_df) {
  default_lang <- .metadata_df %>%
    filter(keyword=="LANGUAGE") %>%
    pull(value)
  contents <- x %>%
    filter(keyword=="CONTENTS") %>%
    pull(value)
  stub <- x %>%
    filter(keyword=="STUB") %>%
    pull(value) %>%
    str_split_1(",") %>%
    str_c(collapse = ", ") %>%
    str_squish()
  heading <- x %>%
    filter(keyword=="HEADING") %>%
    pull(value)

  if (default_lang == "sv") {
    by <- "efter"
    and <- "och"
  } else {
    by <- "by"
    and <- "and"
  }

  str_c(contents, " ", by, " ", stub, " ", and, " ", heading, ".")

}

px_generate_dynamic_title(x)





levs <- ex_data %>%
    select(-value) %>%
    map(unique)

length(levs)

levs_df <- levs %>%
  map(str_c, collapse = ",") %>%
  as_tibble() %>%
  pivot_longer(everything(), names_to = "varname", values_to = "value") %>%
  mutate(keyword = "VALUES")

levs %>% as_tibble()
x

px_get_values_from_data <- function(.data, as_list = FALSE) {
  assertthat::assert_that(any(names(.data) %in% "value"),
                          msg = "No column named value found in data frame, please add it.")

  levs <- .data %>%
    select(-value) %>%
    map(unique)

  assertthat::assert_that(length(levs)>0, msg = "Number of variables in dataframe is not greater than 0.")


  if (as_list) {
    return(levs)
  } else {
    levs %>%
      map(str_c, collapse = ",") %>%
      as_tibble() %>%
      pivot_longer(everything(), names_to = "varname", values_to = "value") %>%
      mutate(keyword = "VALUES", language = NA_character_, valname = NA_character_)
  }
}

x

x %>%
  bind_rows(px_get_values_from_data(ex_data)) %>% tail()

px_get_values_from_data(ex_data)

px_add_values_from_data <- function(.metadata_df, .data) {
  vals_to_add <- px_get_values_from_data(.data)
  px_meta_compare_varnames(.metadata_df_new = vals_to_add, .metadata_df = .metadata_df)

  .metadata_df %>%
    rows_insert(vals_to_add, by = c("keyword","language", "varname", "valname"))
}

px_add_values_from_data(new_meta, ex_data)


px_meta_add_keyword()


names(x)

px_create(.data = ex_data,
          stub = c("sex", "age"),
          heading = "time",
          time_variable = "time",
          matrix = "TEST01",
          subject_area = "Test",
          subject_code = "T",
          units = "Number",
          contents = "Mean number of hours watching The Simpsons",
          decimals = 1
          )


# --------

x %>%
  add_row




# helper
addquotes <- function(txt) {
  Q <- '"'
  str_c(Q, txt, Q)
}

# helper
splitlist <- function(txt) {
  str_split_1(txt, ",") %>%
    addquotes() %>%
    str_c(collapse=",")
}



px_parse_metadata <- function(.metadata_df) {

  Q <- '"'
  E <- ";"
  e <- "="
  comma <- ","
  oq <- "("
  eq <- ")"



  .metadata_df %>%
    mutate(value_parsed = map_chr(value, splitlist)) %>%
    mutate(s = ifelse(is.na(varname) & is.na(valname),
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


    )
    )

}
x

x %>%
  bind_rows(px_add_values_from_data(ex_data)) %>%
  px_parse_metadata() %>%
  select(s) %>% view()

