#' Connect to EC2
#'
#' \code{ssh_ec2} uses ssh to connect to the EC2 to be able to access s3 buckets.
#'
#' The user must already have permissions to access the EC2 already set up.
#'
#' @param username string
#' @param ec2_url ec2 url, `tool.staging.data.humancellatlas.org` by default
#' @return connection ssh connection object
ssh_ec2 <- function(username, ec2_url="tool.staging.data.humancellatlas.org") {
  user_url <- paste0(username, "@", ec2_url)
  connection <- ssh::ssh_connect(user_url)
  return(connection)
}

#' List files in S3
#'
#' \code{list_s3_files} lists the s3 files in a given s3 bucket.
#'
#' @param s3_url the url of the s3 bucket, e.g. s3://org-humancellatlas-upload-staging/aaaaaaaa-bbbb-cccc-dddd...
#' @param user the user's username e.g. `mshadbolt``
#' @return a tibble of files in bucket
#' @export
list_s3_files <- function(s3_url, user) {
  this_connection <- ssh_ec2(username = user)
  this_command <- paste0("aws s3 ls ", s3_url)
  out <- ssh::ssh_exec_internal(this_connection,
                                command = this_command)
  ssh::ssh_disconnect(this_connection)

  filename_df <- stringr::str_split(rawToChar(out$stdout), "\\n",
                                    simplify = T) %>%
    as.data.frame() %>%
    t() %>%
    tibble::as_tibble() %>%
    tidyr::separate(col = V1, sep = "[[:space:]]+",
                    into = c("date", "time", "size", "file_name")) %>%
    dplyr::filter(!is.na(file_name))
  return(filename_df)
}
