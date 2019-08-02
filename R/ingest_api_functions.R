#' Query ingest api for uuids in an existing submission
#'
#' \code{query_exising_submission} provides an easy to use wrapper to query the
#' ingest api using a submission envelope uuid
#'
#' @param submission_id a string
#' @param submission_environment either `staging` or `prod`
#' @param entity_type one of processes, projects, protocols, files, biomaterials
#' @param size the number of results to return, 1000 by default
#' @export
query_existing_submission <- function(submission_id,
                                      submission_environment = c("staging",
                                                                 "prod"),
                                      entity_type = c("biomaterials", "files",
                                                      "processes", "projects",
                                                      "protocols"),
                                      size = 1000) {
  if (submission_environment == "prod") {
    submission_environment = ""
  } else {
    submission_environment <- paste0(".", "staging")
  }
  ingest_api_url <- paste0("https://api.ingest", submission_environment,
                           ".data.humancellatlas.org/submissionEnvelopes/")
  query_url <- paste0(ingest_api_url, submission_id, "/", entity_type,
                      "?size=", size)
  message(paste0("querying: ", query_url))
  request <- httr::GET(query_url, httr::timeout(60))
  if (request$status_code != 200) {
    stop(paste0("API call failed with status ", request$status_code))
  }
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  entity_uuid_df <- jsonlite::fromJSON(response,
                                       flatten = T)[["_embedded"]][[1]]
  return(entity_uuid_df)
}
