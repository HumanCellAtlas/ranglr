# If ontology text filled by contributor, ontology must be filled

# ontology must be of the correct type for that field

# ontology must be in the HCA ontology

#' Read current HCA ontology
#'
#' \code{get_hca_ontology} reads the current dcp ontologies from github and
#' returns a dataframe. By default reads from github but can optionally supply
#' a file path if up to date obo's already downloaded locally.
#'
#' @export
get_hca_ontology <- function(gh_repo = "https://github.com/HumanCellAtlas/ontology") {
  print("hello world")

}
