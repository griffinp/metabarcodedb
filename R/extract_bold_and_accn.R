#' Extract NCBI Accession.Version IDS and BOLD IDs for a Vector of NCBI UIDs
#'
#' This function extracts the BOLD (Barcode of Life Database) IDs, if present, and the accession.version IDs
#' from NCBI for a input character vector of NCBI UIDs.
#' It returns a dataframe with columns uid, bold, and accn.
#' @param id_vector A character vector of NCBI UIDs
#' @keywords ncbi bold id
#' @export
#' @importFrom rentrez entrez_fetch
#' @examples
#' extract_bold_and_accn(id_vector=c('1072897717', '329757399', '259881131'))


extract_bold_and_accn <- function(id_vector){
  entrez_xml <- rentrez::entrez_fetch(db="nuccore", id=id_vector, rettype="native", parsed=TRUE, complexity=2)
  bold_id <- extract_bold_from_entrez_xml(entrez_xml=entrez_xml)
  accn <- extract_accn_version_from_entrez_xml(entrez_xml=entrez_xml)
  output_df <- data.frame(uid=id_vector,
                          bold=bold_id,
                          accn=accn, stringsAsFactors = FALSE)
  return(output_df)
}
