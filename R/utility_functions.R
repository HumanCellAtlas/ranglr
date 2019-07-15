#' Load a metadata spreadsheet
#'
#' \code{load_spreadsheet} loads a multi-sheet metadata spreadsheet.
#'
#' It removes the top 3 rows as well as removing the 'fill below this line' row.
#'
#' The names of the tibbles are the names of the sheets.
#'
#' This function extends the recommended example at
#' https://readxl.tidyverse.org/articles/articles/readxl-workflows.html.
#'
#' @param path a string.
#' @return a list of tibbles for each sheet in the spreadsheet.
#' @examples
#' load_spreadsheet("~/name_of_spreadsheet.xlsx")
#' @importFrom magrittr %>%
#' @export
load_spreadsheet <- function(path) {
  list_of_tibbles <- path %>%
    readxl::excel_sheets() %>%
    purrr::set_names() %>%
    purrr::map(readxl::read_excel, path = path, skip = 3) %>%
    purrr::map(dplyr::slice, 2:dplyr::n())
  return(list_of_tibbles)
}

#' Summarise a spreadsheet
#'
#' \code{summarise_spreadsheet} provides a useful summary of the data that is
#' currently in a spreadsheet so that a wrangler can get a quick and easy
#' overview.
#'
#' @param tibble_list a list of tibbles outputted by \code{load_spreadsheet}
#' @return a tibble with summary counts
#' @export
summarise_spreadsheet <- function(tibble_list) {
  print("Project Accessions")
  accession_columns <- colnames(tibble_list$Project %>%
                                  dplyr::select(dplyr::contains("accession")))
  print(accession_columns)
  tibble_list$Project %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("accession")),
                     count_accessions) %>%
    print()
}

#' Count accessions
#'
#' \code{count_accessions} counts the number of accessions in a \code{||}
#' delimited list. (Not exported)
#'
#' @param acc_string a string
#' @return an integer
count_accessions <- function(acc_string) {
  if(is.na(acc_string)) {
    return(0)
  } else{
    return(length(stringr::str_split(acc_string, pattern = "\\|\\|",
                                     simplify = T)))
  }
}

#' Get linking fields
#'
#' \code{get_linking_fields} takes a metadata spreadsheet list and determines
#' which fields are used as linking between them.
#'
#' @param sheet_1 a tibble, first sheet to compare
#' @param sheet_2 a tibble, second sheet to compare
#' @return a tibble with columns \code{type_module_1}, \code{type_module_2},
#' \code{linking_field}
#' @export
get_linking_fields <- function(sheet_1, sheet_2) {
  name_sheet_1 <- get_type_module(sheet_1)
  name_sheet_2 <- get_type_module(sheet_2)
  shared_names <- intersect(colnames(sheet_1),
                            colnames(sheet_2))
  #print(shared_names)
  shared_names <- ifelse(is.null(shared_names), NA, shared_names)
  result_tibble <- tibble::as_tibble(list("type_module_1" = name_sheet_1,
                                          "type_module_2" = name_sheet_2,
                                          "linking_field" = shared_names))
  return(result_tibble)
}

#' Get field name
#'
#' \code{get_field_name} gets the specific field name in the long `.`` delimited
#' programmatic name. It splits the field by the `.` and returns the last string.
#' (not exported)
#'
#' @param long_name a string
#' @return field_name
get_field_name <- function (long_name) {
  num_fields <- stringr::str_count(long_name, pattern = "\\.") + 1
  return(stringr::str_split(long_name, "\\.", simplify = T)[num_fields])
}

#' Get type module
#'
#' \code{get_type_module} infers the sheet's name by getting the first part of
#' the type and module fields from the first column name. Assumes that the first
#' column is a typical column name for the spreadsheet.
#' @param sheet a tibble from a tibble_list
#' @return the sheet name as a string
#' @export
get_type_module <- function (sheet) {
  first_colname <- colnames(sheet)[1]
  split_name <- stringr::str_split(first_colname, pattern = "\\.", simplify = T)
  type_module <- paste0(split_name[1], ".", split_name[2])
  return(type_module)
}

#' Get column names per sheet
#'
#' \code{get_col_names} gets all the column names for a sheet and returns a
#' tibble linking the column names to the name of the sheet.
#'
#' @param tibble_list list of tibbles outputted by \code{load_spreadsheet}
#' @return a tibble with two columns, \code{sheet_name} and \code{col_name}
#' @export
get_col_names <- function(tibble_list) {
  sheet_names <- names(tibble_list)
  col_names_list <- lapply(sheet_names,
                           function(x)
                             tibble::as_tibble(list("sheet_name"= x,
                                                    "col_name" = colnames(metadata_spreadsheet[[x]]))))
  col_names_df <- do.call(dplyr::bind_rows, col_names_list)
  return(col_names_df)
}



# TODO: make getter functions for each accession type and then check that each
# accession actually exists in the relevant database.
# check url is valid
# check doi is valid

# get ontology terms
# harvests ontology terms from an existing spreadsheet
