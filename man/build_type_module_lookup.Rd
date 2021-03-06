% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logic_validator_functions.R
\name{build_type_module_lookup}
\alias{build_type_module_lookup}
\title{Build type module lookup}
\usage{
build_type_module_lookup(tibble_list)
}
\arguments{
\item{tibble_list}{a tibble list outputted from \code{load_spreadsheet}}
}
\value{
a tibble to lookup sheet names from \code{type_module} names
}
\description{
\code{build_type_module_lookup} uses the get_col_names utility to build
a df to link the sheet names for each type module field in the
\code{linking_df}
}
