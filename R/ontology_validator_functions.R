# If ontology text filled by contributor, ontology must be filled

# ontology must be of the correct type for that field

# ontology must be in the HCA ontology

#' Query HCA ontology by term
#'
#' \code{query_hca_ontology_term} queries the current hca ontology using the
#' hcao ols API.
#'
#' @param ontology_term a vector of string ontology terms e.g. EFO:0009737
#' @param ontology the name of the ontology to query
#' @param base_url the base url by default set to \code{https://ontology.staging.data.humancellatlas.org/api/ontologies}
#' @return a dataframe with api results
#' @export
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

#' Query OLS for free text
#'
#' \code{query_ontology_text} allows the user to query the OLS with free text
#' using the `search` endpoint
#'
#' @param free_text to use for the query
#' @param base_url by default uses the hcao staging API
#' @param ontologies optional vector of ontologies to query
#' @param parent_term optional term that results must be a child of
#' @return a tibble with query results
#' @export
#' TODO: Add functionality to restrict by ontology and parent term
query_ontology_text <- function(free_text,
                                base_url="https://ontology.staging.data.humancellatlas.org/api/search?q=",
                                ontologies = NULL,
                                parent_term = NULL) {
  free_text <- stringr::str_replace_all(free_text, " ", "+")
  api_query = paste0(base_url,"/", free_text)
  print(api_query)
  request <- httr::GET(url = api_query)

  if(request$status_code != 200){
    stop(paste0("API call unsuccessful with status code ", request$status_code, "."))
  }

  response <- httr::content(request, as = "text", encoding = "UTF-8")
  json_list <- jsonlite::fromJSON(response, flatten = TRUE)
  return(json_list[["response"]][["docs"]])
}
