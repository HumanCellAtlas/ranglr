
#' load a metadata spreadsheet
#'
#' @param path a string.
#' @return a list of tibbles for each sheet in the spreadsheet.
#' @examples
#' load_spreadsheet("~/name_of_spreadsheet.xlsx")
load_spreadsheet <- function(path) {
  list_of_tibbles <- readxl::read(path)
}
