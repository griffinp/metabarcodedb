#' Adds an identifier line at the bottom of a classification dataframe created by taxize::classification or obtained from BOLD
#'
#' @param classif classification data frame for an individual taxon, created by taxize::classification
#' @param identifier_value string containing value of the identifier. Good idea to specify as a key:value pair (e.g. 'uid:1109490022')
#' @param classification_source string specifying the source of the classification information: either "ncbi" or "bold" is allowed.
#' @keywords ncbi uid taxonomy classification taxize dada2
#' @export
#' @importFrom ape read.FASTA
#' @importFrom ape write.dna
#' @examples
#' add_identifier_line_to_classification(classif, identifier_value='uid:1109490022')


add_identifier_line_to_classification <- function(classif, identifier_value, classification_source="ncbi"){
  if(classification_source%in%c("ncbi", "bold")==FALSE){
    stop("classification_source must have the value 'ncbi' or 'bold'")
  }
  if(classification_source=="ncbi"){
    new_classif <- rbind(classif, c(name=identifier_value, rank="identifier", id=""))
  } 
  if(classification_source=="bold"){
    new_classif <- rbind(classif, c(name=identifier_value, rank="identifier"))
  }
  return(new_classif)
}
