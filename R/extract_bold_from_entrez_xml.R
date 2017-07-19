#' extract_bold_from_entrez_xml
#'
#' This function extracts the BOLD (Barcode of Life Database) IDs, if present,
#' from an NCBI xml record returned by the rentrez::entrez_fetch command.
#' It returns a vector of BOLD IDs (or NA if no BOLD ID was present for a given entry)
#' @param entrez_xml An xml object returned by the rentrez::entrez_fetch command
#' @keywords ncbi xml entrez
#' @export
#' @importFrom XML getNodeSet
#' @importFrom XML xmlToDataFrame
#' @import rentrez
#' @examples
#' extract_bold_from_entrez_xml(entrez_xml="Chlorophyta_xml")

extract_bold_from_entrez_xml <- function(entrez_xml){
  out <- XML::getNodeSet(entrez_xml, "//*[name()='Org-ref_db']", fun=XML::xmlToDataFrame)
  bold_id <- lapply(out, function(x) as.character(x[which(x$Dbtag_db=="BOLD"), "Dbtag_tag"]))
  # If there is no BOLD id, make it NA
  bold_id <- lapply(bold_id, function(x) if(length(x)<1){x=NA} else{x=x})
  return(unlist(bold_id))
}

