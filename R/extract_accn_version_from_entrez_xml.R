#' extract_accn_version_from_entrez_xml
#'
#' This function extracts the accession.version IDs
#' from an NCBI xml record returned by the rentrez::entrez_fetch command.
#' It returns a vector of accession.version IDs
#' @param entrez_xml An xml object returned by the rentrez::entrez_fetch command
#' @keywords ncbi xml entrez
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlToDataFrame
#' @examples
#' extract_accn_version_from_entrez_xml(entrez_xml="Chlorophyta_xml")


extract_accn_version_from_entrez_xml <- function(entrez_xml){
  accn_info <- XML::getNodeSet(entrez_xml, "//*[name()='Textseq-id']", fun=XML::xmlToDataFrame)
  # Deal with the rare case where protein ID also gets extracted
  accn_chr <- lapply(accn_info, function(x) as.character(x[,1]))
  accn_check <- lapply(accn_chr, function(x) if(length(x)>2){x[1:2]<-x[2:3]} else{x<-x})
  accn_version <- lapply(accn_check, function(x) paste(x[1], ".", x[2], sep=""))
  return(unlist(accn_version))
}
