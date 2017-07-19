#' Format a classification data frame into dada2 reference database FASTA header format
#'
#' This is a function to take a classification data frame (in the format
#' returned by the 'taxize::classification' function) and return the taxonomy
#' information formatted as a string of the format required by dada2 for its
#' reference database. For example,
#' Metazoa;Arthropoda;Insecta;Diptera;Chironomidae;Chironomus;Chironomus riparius
#' @param classif classification data frame for an individual taxon, created by taxize::classification
#' @keywords ncbi uid taxonomy classification taxize dada2
#' @export
#' @examples
#' format_taxonomic_levels(classif=Athericidae_classification[[1]])


format_taxonomic_levels <- function(classif){
  tax_levels <- classif[["name"]]
  tax_levels[which(is.na(tax_levels))] <- ""
  tax_levels_formatted <- paste(tax_levels, collapse=";")
  return(tax_levels_formatted)
}
