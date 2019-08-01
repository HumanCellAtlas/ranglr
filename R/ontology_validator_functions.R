# If ontology text filled by contributor, ontology must be filled

# ontology must be of the correct type for that field

# ontology must be in the HCA ontology

#' Read current HCA ontology
#'
#' \code{get_hca_ontology} reads the current dcp ontologies from github and
#' returns a dataframe. By default reads from github but can optionally supply
#' a file path if up to date obo's already downloaded locally.
#'
#' @param ontology_term a vector of string ontology terms e.g. EFO:0009737
#' @param ontology the name of the ontology to query
#' @param base_url the base url by default set to \code{https://ontology.staging.data.humancellatlas.org/api/ontologies}
#' @return a dataframe with api results
# TODO: this probably isn't required so put on the backburner
query_hca_ontology_term <- function(ontology_term, ontology,
                                    base_url="https://ontology.staging.data.humancellatlas.org/api/ontologies") {
  api_query = paste0(base_url,"/", ontology, "/terms?obo_id=", ontology_term)
  print(api_query)
  request <- httr::GET(url = api_query)

  if(request$status_code != 200){
    warning(paste0(ontology_term, " not found in ", ontology, "."))
    return(NA)
  }

  response <- httr::content(request, as = "text", encoding = "UTF-8")
  json_list <- jsonlite::fromJSON(response, flatten = TRUE)

  results <- json_list$'_embedded'%>%
    unlist() %>%
    t() %>%
    tibble::as_tibble() %>%
    dplyr::select(terms.obo_id, terms.label, terms.is_obsolete, terms.description)

  return(results)
}
