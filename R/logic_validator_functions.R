#' Build linking df
#'
#' \code{build_linking_df} builds a dataframe from the result of running
#' \code{get_linking_fields} over a multisheet spreadsheet. The wrangler can
#' then review whether all requisite linking fields are present and use as input
#' to other functions to help validation.
#' @param tibble_list the list of tibbles outputted by \code{load_spreadsheet}
#' @return a tibble with fields type_module_1, type_module_2 and the fields in common
#' between them
#' @export
build_linking_df <- function(tibble_list) {
  linking_df <- NULL
  sheet_names <- names(tibble_list)
  # TODO make this more efficient instead of a nested for loop
  for (s_1 in sheet_names) {
    for (s_2 in sheet_names) {
      this_df <- get_linking_fields(tibble_list[[s_1]],
                                       tibble_list[[s_2]])
      linking_df <- dplyr::bind_rows(linking_df, this_df)
    }
  }
  linking_df <- linking_df %>%
    dplyr::filter(!is.na(linking_field),
                  !type_module_1 == type_module_2) %>%
    dplyr::group_by(linking_field) %>%
    dplyr::filter(dplyr::row_number() == 1)
  return(linking_df)
}

#' Build type module lookup
#'
#' \code{build_type_module_lookup} uses the get_col_names utility to build
#' a df to link the sheet names for each type module field in the
#' \code{linking_df}
#'
#' @param tibble_list a tibble list outputted from \code{load_spreadsheet}
#' @return a tibble to lookup sheet names from \code{type_module} names
#' @export
build_type_module_lookup <- function(tibble_list) {
  lookup_df <- get_col_names(tibble_list) %>%
    dplyr::group_by(sheet_name) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::mutate(type_module = paste0(stringr::str_split(col_name,
                                                          pattern = "\\.",
                                                          simplify = T)[c(1,2)],
                                       collapse = "."))
  return(lookup_df)
}

# All cell suspensions in the files tabs

# Need to put together a list of all the linking columns in each tab to then check the levels are the same between each

# all accessions in cell suspension are in project tab

# genus_species same as ncbi_taxon



# do all donors have a sequence or image file file

# same library sequenced more than once? - library_preparation_id

# all files listed in spreadsheet in the identified S3
# no extra files in s3


# easy optional fields to check
# is read length filled
# is there a publication
# if published protocol are all protocol fields filled?
# protocol specific fields.
