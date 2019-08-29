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
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup()

  type_module_lookup <- build_type_module_lookup(tibble_list)
  linking_df <- linking_df %>%
    dplyr::left_join(type_module_lookup %>%
                       dplyr::select(-col_name), by = c("type_module_1" = "type_module")) %>%
    dplyr::rename(sheet_name_1 = sheet_name)  %>%
    dplyr::left_join(type_module_lookup %>%
                       dplyr::select(-col_name), by = c("type_module_2" = "type_module")) %>%
    dplyr::rename(sheet_name_2 = sheet_name)  %>%
    dplyr::select(sheet_name_1, sheet_name_2, linking_field)
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
                                       collapse = ".")) %>%
    dplyr::ungroup()
  return(lookup_df)
}

#' Validate link levels
#'
#' \code{validate_link_levels} checks the levels of a linking field are
#' consistent between two tabs of the spreadsheet. It is currently not very
#' elegantly written.
#'
#' @param linking_df output from running \code{build_linking_df}
#' @param tibble_list the loaded spreadsheet from \code{load_spreadsheet}
#' @return linking_df with two additional columns
#' @export
validate_link_levels <- function(linking_df, tibble_list) {
  all_1_in_2 <- rep(FALSE, length(linking_df$sheet_name_1))
  all_2_in_1 <- rep(FALSE, length(linking_df$sheet_name_1))
  for (i in 1:length(linking_df$sheet_name_1)) {
    tab_1_fields <- dplyr::pull(tibble_list[[linking_df$sheet_name_1[i]]][linking_df$linking_field[i]])
    tab_2_fields <- dplyr::pull(tibble_list[[linking_df$sheet_name_2[i]]][linking_df$linking_field[i]])
    tab_1_split <- unlist(lapply(tab_1_fields, split_field_list))
    tab_2_split <- unlist(lapply(tab_2_fields, split_field_list))
    tab_1_levels <- levels(factor(tab_1_split))
    tab_2_levels <- levels(factor(tab_2_split))
    all_1_in_2[i] <- all(tab_1_levels %in% tab_2_levels)
    all_2_in_1[i] <- all(tab_1_levels %in% tab_2_levels)
  }

  linking_df$all_1_in_2 <- all_1_in_2
  linking_df$all_2_in_1 <- all_2_in_1
  return(linking_df)
}

#' Check uploaded files s3
#'
#' \code{check_uploaded_files} cross references the files listed in the
#' spreadsheet against the files currently in the s3 and warns the user if there are
#' inconsistencies.
#'
#' @param ssheet_file_names result of
#' @param s3_file_names result of `list_s3_files$file_name`
#' @export
check_uploaded_files <- function(ssheet_file_names, s3_file_names) {
  if (all(ssheet_file_names %in% s3_file_names)){
    message("All Spreadsheet files present in s3 bucket")
  } else {
    missing_files <- ssheet_file_names[which(!ssheet_file_names %in% s3_file_names)]
    warning(paste0("There are ", length(missing_files), " files not in the spreadsheet that are not in the s3 bucket:\n",
                   paste0(missing_files,
                   collapse = "\n"), "\n\n"))
  }
  if (all(s3_file_names %in% ssheet_file_names)) {
    message("There are no extra files in the s3 bucket")
  } else {
    extra_files <- s3_file_names[which(!s3_file_names %in% ssheet_file_names)]
    warning(paste0("There are ", length(extra_files), " files in the s3 bucket that are not in your spreadsheet:\n",
                   paste0(extra_files,
                   collapse = "\n"), "\n"))
  }
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
