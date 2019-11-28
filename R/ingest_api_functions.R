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


#' Query tracker api for project information to add to an AUDR ticket
#'
#' \code{get_audr_info} provides an easy to use wrapper to query the
#' tracker api using a project uuid to get required info for the AUDR ticket
#'
#' @param submission_environment either `staging` or `prod`
#' @param project_uuid the uuid of the project you want info about
#' @export
get_audr_info <- function(submission_environment="prod",
                          project_uuid){
  if (submission_environment == "prod") {
    submission_environment = ""
  } else {
    submission_environment <- paste0(".", "staging")
  }
  tracker_api_url <- paste0("https://tracker-api", submission_environment,
                           ".data.humancellatlas.org/v0/project/")
  query_url <- paste0(tracker_api_url, project_uuid)
  message(paste0("querying: ", query_url))
  request <- httr::GET(query_url, httr::timeout(60))
  if (request$status_code != 200) {
    stop(paste0("API call failed with status ", request$status_code))
  }
  response <- httr::content(request, as = "text", encoding = "UTF-8")
  response <- jsonlite::fromJSON(response, flatten = T)
  cat("project full name: ", response$`project-info`$project_title, "\n")
  cat("project short name: ", response[["ingest-info"]][["project_short_name"]], "\n")
  cat("project uuid: ", project_uuid, "\n")
  cat("submission date: ", response[["ingest-info"]][["submission_date"]], "\n")
  cat("submission uuid: ", response[["ingest-info"]][["submission_uuid"]], "\n")
  cat("update date: ", response[["ingest-info"]][["update_date"]], "\n")
  cat("involved wranglers:", response[["ingest-info"]][["data_curator"]], "\n")
  cat("Analysis state: ", response[["analysis-info"]][["analysis_state"]], "\n")
  cat("Project state: ", response[["project-info"]][["project_state"]], "\n")
  paste0("DCP github issue: https://github.com/HumanCellAtlas/dcp/issues/",
      response[["project-info"]][["github_issue"]], "\n")
  return(response)
}
