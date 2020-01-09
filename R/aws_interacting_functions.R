#' Connect to EC2
#'
#' \code{ssh_ec2} uses ssh to connect to the EC2 to be able to access s3 buckets.
#'
#' The user must already have permissions to access the EC2 already set up.
#'
#' @param username string
#' @param ec2_url ec2 url, `tool.staging.data.humancellatlas.org` by default
#' @return connection ssh connection object
#' @export
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
#' @param user the user's username usually `FirstInitialLastName`
#' @return a tibble of files in bucket
#' @export
# TODO: write code to ensure it doesn't error or at least hide the error message
list_s3_files <- function(s3_url, user) {
  tryCatch({
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
  },
  error=function(cond){
    message("s3 probably empty, original error message:")
    message(cond)
    ssh::ssh_disconnect(this_connection)
    return(NA)
  }
  )


}

#' Create s3 upload area
#'
#' \code{create_s3} creates an s3 upload area for a particular project. By
#' assumes you have the staging api key saved in your .Renviron file and called
#' "STAGING_UPLOAD_API_KEY"
#'
#' @param user your username
#' @param project_shorthand the `-` delimited name of the dataset e.g. my-cool-project
#' @param upload_api_key the upload api string
#' @param upload_env_name the name of the environment, i.e. staging, dev,
#' integration
#' @param submission_n by default 0 but can be changed if it is a bucket for
#'  a subsequent submission for the same project
#' @return the url of the s3 bucket of the form \code{s3://org-humancellatlas-upload-staging/aaaaaaaa-bbbb-cccc-dddd-e012345f6789/}
#' @examples
#' create_s3("FirstInitialLastName", "my-cool-project")
#' @export
create_s3 <- function(user, project_shorthand,
                      upload_api_key=Sys.getenv("STAGING_UPLOAD_API_KEY"),
                      upload_env_name="staging",
                      submission_n=0){

  full_dataset_name <- paste0(user, "-", project_shorthand, "-", upload_env_name,
                              "-", submission_n)
  print(full_dataset_name)
  this_command <- paste0("python /data/tools/create_upload_area.py --api-key ",
                          upload_api_key, " --dataset-name ", full_dataset_name,
                          " --environment ", upload_env_name)
  this_connection <- ssh_ec2(username = user)
  out <- ssh::ssh_exec_internal(this_connection,
                                command = this_command)
  ssh::ssh_disconnect(this_connection)
  s3_bucket_url <- stringr::str_split(rawToChar(out$stdout), "\n", simplify = T)[,5]
  return(s3_bucket_url)
}

#' Copy s3 files between HCA s3 buckets
#'
#' \code{copy_s3} copies files between two HCA s3 buckets. It will not work for
#' buckets outside the HCA
#'
#' @param{source_s3} the url of the s3 where the files to be copied are
#' @param{dest_s3} the url of the s3 where you want the files to go
#' @param{user} your username
#' @export
copy_s3 <- function(source_s3, dest_s3, user){
  this_connection <- ssh_ec2(username = user)
  select_command <- paste0("hca upload select ", dest_s3)
  upload_command <- paste0("hca upload files -s true ", source_s3)
  out_select <-ssh::ssh_exec_internal(this_connection,
                         command = select_command)
  out_upload <- ssh::ssh_exec_internal(this_connection,
                         command = upload_command)
  ssh::ssh_disconnect(this_connection)
  return(list(out_select, out_upload))
}
