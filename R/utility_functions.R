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

  # Check if supp files present, if not remove the tab
  tab_names <- names(list_of_tibbles)
  if ("Supplementary file" %in% tab_names) {
    if (stringr::str_detect(list_of_tibbles$"Supplementary file"$"supplementary_file.file_core.file_name"[1], "FILL OUT INFORMATION BELOW THIS ROW")) {
      list_of_tibbles["Supplementary file"] <- NULL
    }
  }
  # remove schemas tab
  list_of_tibbles["Schemas"] <- NULL
  return(list_of_tibbles)
}

#' Summarise a spreadsheet
#'
#' \code{summarise_spreadsheet} provides a useful summary of the data that is
#' currently in a spreadsheet so that a wrangler can get a quick and easy
#' overview. This function prints a nicely formatted table as well as returning
#' the tibble from which the table was derived.
#'
#' @param tibble_list a list of tibbles outputted by \code{load_spreadsheet}
#' @return a tibble with summary counts
#' @export
summarise_spreadsheet <- function(tibble_list) {
  accession_columns <- colnames(tibble_list$Project %>%
                                  dplyr::select(dplyr::contains("accession")))
  summary_df <-tibble_list$Project %>%
    dplyr::mutate_at(dplyr::vars(dplyr::contains("accession")),
                     count_accessions)
  summary_df$bundle_count <- count_bundles(tibble_list)
  colnames(summary_df) <- sapply(colnames(summary_df), get_field_name)
  summary_df <- summary_df %>%
    dplyr::select(project_short_name, bundle_count, dplyr::contains("accessions"))
  summary_df %>%
    t() %>%
    knitr::kable() %>%
    kableExtra::kable_styling() %>%
    print()

  return(summary_df)
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

#' Count bundles
#'
#' \code{count_bundles} estimates the number of bundles based on the number of
#' levels in the `process_id` field in the `Sequence file` tab of the
#' spreadsheet.
#'
#' @param tibble_list a list of tibbles outputted by \code{load_spreadsheet}
#' @export
count_bundles <- function(tibble_list){
  lib_preps <- tibble_list$`Sequence file`$process.process_core.process_id
  bundle_count <- length(levels(factor(lib_preps)))
  return(bundle_count)
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
#'
#' @param long_name a string
#' @return field_name
#' @export
get_field_name <- function (long_name) {
  num_fields <- stringr::str_count(long_name, pattern = "\\.") + 1
  return(stringr::str_split(long_name, "\\.", simplify = T)[num_fields])
}

#' Get everything except field name
#'
#' \code{get_not_field_name} gets the long `.` delimited programmatic name.apart
#' from the final field. It splits the field by the `.` and returns everything
#' except the last string.
#'
#' @param long_name a string
#' @return not the field name
#' @export
get_not_field_name <- function (long_name) {
  num_fields <- stringr::str_count(long_name, pattern = "\\.")
  name_without_field <- paste0(stringr::str_split(long_name, "\\.", simplify = T)[1:num_fields], collapse=".")
  return(name_without_field)
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
                                                    "col_name" = colnames(tibble_list[[x]]))))
  col_names_df <- do.call(dplyr::bind_rows, col_names_list)
  return(col_names_df)
}

#' Get filename fields in a spreadsheet
#'
#' \code{get_file_name_fields} gets all the filenames listed in a spreadsheet so
#' that they can a wrangler can check if they have been uploaded to the
#' appropriate s3 bucket for a project.
#'
#' @param tibble_list a list of tibbles outputted from \code{load_spreadsheet}
#' @return a list of filenames
#' @export
# won't be exported when the get_all_filenames function works properly
get_file_name_fields <- function(tibble_list){
  file_name_fields <- get_col_names(tibble_list) %>%
    dplyr::filter(stringr::str_detect(col_name, "file_name$"))
  return(file_name_fields)
}

#' Get filenames in the spreadsheet
#'
#' \code{get_file_names} gets a list of the filenames in the spreadsheet. By
#' default returns all filenames in the spreadsheet.
#'
#' @param tibble_list outputted from \code{load_spreadsheet}
# @param field specific file name field to query
#' @return a vector of string filenames
#' @export
# TODO: add functionality to recognise if one of the fields is empty
# TODO: add optional param to specify field or fields
get_file_names <- function(tibble_list, field = NULL) {
  file_name_fields <- get_file_name_fields(metadata_spreadsheet)
  if (!is.null(field)){
    file_name_fields <- file_name_fields %>%
      filter(col_name %in% field)
  }
  file_names <- purrr::map2(.x = file_name_fields$sheet_name,
                            .y = file_name_fields$col_name,
                            .f = pull_field,
                            tibble_list = metadata_spreadsheet) %>%
    unlist()
  return(file_names)
}

#' Pull field
#'
#' \code{pull_field} pulls a particular field from the spreadsheet based on the
#' sheet name and the field name
#'
#' @param sheet_name a string
#' @param field_name programmatic field name
#' @param tibble_list the list of tibbles outputted from \code{load_spreadsheet}
#' @return a vector of strings the length of the field
#' @export
pull_field <- function(sheet_name, field_name, tibble_list) {
  the_field <- tibble_list[[sheet_name]][field_name] %>%
    dplyr::pull(field_name)
  return(the_field)
}

#' Pull field levels
#'
#' \code{pull_field_levels} pulls a list of unique levels for a particular field
#'
#' @param sheet_name a string
#' @param field_name programmatic field name
#' @param tibble_list the list of tibbles outputted from \code{load_spreadsheet}
#' @return a vector of unique values for a field
#' @export
pull_field_levels <- function(sheet_name, field_name, tibble_list) {
  field_levels <- tibble_list[[sheet_name]][field_name] %>%
    dplyr::pull(field_name) %>%
    factor() %>% levels()
  return(field_levels)
}

#' Get rows
#'
#' \code{get_rows} gets a set of rows from the spreadsheet by the sheet name and
#' field name.
#'
#' @param tibble_list the loaded metadata spreadsheet
#' @param sheet_name the name of sheet
#' @param field_name a list of the programmatic names of the desired columns
#' @return a tibble with the `field_names` given
#' @export
get_rows <- function(tibble_list, sheet_name, field_name){
  field_names <- unlist(field_name)
  requested_rows <- tibble_list[[sheet_name]] %>%
    dplyr::select(dplyr::one_of(field_names)) %>%
    dplyr::mutate(sheet_name = sheet_name)
  print(get_field_name(field_name))
  print()
  # colnames(requested_rows) <- c(get_field_name(field_name), "sheet_name", "pre_field_name")
  # print(colnames(requested_rows))
  return(requested_rows)
}

#' Expand rows by field level
#'
#' \code{expand_rows} takes a tibble with a consistent number of nested field
#' levels and expands (unnests) the rows.
#'
#' @param split_tibble a tibble with consistent field levels
#' @return a tibble with expanded rows
#' @export
expand_rows <- function(split_tibble) {
  split_tibble$levels <-
    purrr::map2(.x = split_tibble$sheet_name,
                .y = split_tibble$col_name,
                .f = pull_field_levels, tibble_list = metadata_spreadsheet)
  expanded_tibble <- tidyr::unnest(split_tibble)
    return(expanded_tibble)
}

#' Split list of fields
#'
#' \code{split_field_list} takes as input a single string that is made up of
#' multiple fields that are delimited by `||`` and returns a string vector
#'
#' @param field_list a || delimited string
#' @return a vector of strings
#' @export
split_field_list <- function(field_list) {
  split_vector <- c(stringr::str_split(field_list, pattern = "\\|\\|",
                                       simplify = T))
  split_vector <- split_vector[split_vector != ""]
  return(split_vector)
}

#' Get ontologies
#'
#' \code{get_ontologies} goes through the spreadsheet to mine out all the
#' ontology terms so that a wrangler can quickly look at the list of ontologies
#' for a sanity check.
#'
#' @param tibble_list the list of tibbles outputted from `load_spreadsheet()`
#' @return a dataframe with sheet_name, field_name, text, ontology,
#' ontology_label
#' @export
get_ontologies <- function(tibble_list) {



  # split_tibble <- get_col_names(tibble_list) %>%
  #   dplyr::filter(stringr::str_detect(col_name, "ontology|text")) %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(col_type = get_field_name(col_name),
  #                 not_field = get_not_field_name(col_name)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(not_field) %>%
  #   dplyr::group_split()
  #
  # expanded_tibble_list <- purrr::map(split_tibble, expand_rows)
  # bound_tibble <- do.call(dplyr::bind_rows, expanded_tibble_list)
  #
  # ontology_table <- bound_tibble %>%
  #   dplyr::filter(col_type == "ontology") %>%
  #   dplyr::select(-col_type) %>%
  #   dplyr::rename(ontology = levels) %>%
  #   dplyr::left_join(bound_tibble %>%
  #                      dplyr::filter(col_type == "ontology_label") %>%
  #                      dplyr::select(-col_type, -col_name) %>%
  #                      dplyr::rename(ontology_label = levels)) %>%
  #   dplyr::left_join(bound_tibble %>%
  #                      dplyr::filter(col_type == "text") %>%
  #                      dplyr::select(-col_type, -col_name) %>%
  #                      dplyr::rename(ontology_text = levels))

#
#
#   ontology_columns <- get_col_names(tibble_list) %>%
#     dplyr::filter(stringr::str_detect(col_name, "ontology|text")) %>%
#     dplyr::rowwise() %>%
#     dplyr::mutate(unique_entries = paste0(levels(factor(tibble_list[[sheet_name]][[col_name]][which(!is.na(tibble_list[[sheet_name]][[col_name]]))])),
#                                            collapse = "||")) %>%
#     dplyr::mutate(col_type = get_field_name(col_name),
#                   not_field = get_not_field_name(col_name)) %>%
#     dplyr::select(-col_name) %>%
#     tidyr::spread(col_type, unique_entries)
  return(ontology_table)
}


# TODO: make getter functions for each accession type and then check that each
# accession actually exists in the relevant database.
# check url is valid
# check doi is valid

# get ontology terms
# harvests ontology terms from an existing spreadsheet









