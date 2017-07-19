#' Get NCBI FASTA from Unique Identifiers
#'
#' This function retrieves FASTA sequences for a given vector of NCBI unique identifiers (UIDs)
#' This function is quite slow because it doesn't use the web history functionality.
#' It returns the fasta file in Entrez API fasta format.
#' @param id_vector A character vector of NCBI UIDs.
#' @param block_size An integer giving the size of blocks into which the full vector of IDs with missing classification info is split. This is important for ensuring full retrieval in the case of taxa with a large number of records (more than approx. 10000). Default = 5000
#' @keywords ncbi uid fasta metabarcoding
#' @export
#' @examples
#' get_fasta_from_id_vector(id_vector=c('1072897717', '329757399', '259881131'), taxon="Hypogasturidae", block_size=5000)

get_fasta_from_id_vector <- function(id_vector, block_size=5000){
  # This function searches the NCBI nuccore database for records for the uids provided
  # For use in cases where a web_history object isn't available
  # record_block_size is an integer giving the number of IDs to retrieve at once (default 5000)
  # which is important for ensuring full retrieval in the case of taxa with a large number of records (more than approx. 10000)
  # The function returns a character string.
  #message(paste("Fetching", length(id_vector), "sequences for", taxon))
  fasta <- entrez_fetch(db="nuccore", id=id_vector,
                        rettype="fasta", retmax=block_size)
  return(fasta)
}
