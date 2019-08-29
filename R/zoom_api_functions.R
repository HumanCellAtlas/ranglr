#' Query text in zoom
#'
#' \code{query_zoom} searches the zoom API for a free text ontology field and
#' returns possible likely ontologies.
#'
#' @param free_text the text to search
#' @return tibble of results
#' @export
query_zoom <- function(free_text, ontology=NULL){
  zoom_api_url <- "www.ebi.ac.uk/spot/zooma/v2/api/services/annotate?propertyValue="
  free_text <- stringr::str_replace_all(free_text, " ", "+")
  if (!is.null(ontology)){
    ontology_filter = paste0("&filter=required:[none],ontologies:[", ontology, "]")
    query_url <- paste0(zoom_api_url, free_text, ontology_filter)
  } else {
    query_url <- paste0(zoom_api_url, free_text)
  }
  message(paste0("querying: ", query_url))
  request <- httr::GET(query_url, httr::timeout(60))
  if (request$status_code != 200) {
    stop(paste0("API call failed with status ", request$status_code))
  }
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  zoom_df <- jsonlite::fromJSON(response,
                                flatten = T) #[["_embedded"]][[1]]
  return(zoom_df)

}
