#' get_fasta_with_web_history
#'
#' This function retrieves FASTA sequences for a given ncbi_block (created with ***)
#' This function is quite fast because it uses the web history present in the ncbi_block.
#' It returns the fasta file in Entrez API fasta format.
#' @param ncbi_search_block An entry in a list created with the get_ncbi_search_blocks function. Contains a vector of NCBI UIDs ('block_ids'), the web history cookie ('cookie') and query key ('qk'), and the total number of records found with the underlying query.
#' @keywords ncbi uid fasta metabarcoding
#' @importFrom rentrez entrez_fetch
#' @export
#' @examples
#' get_fasta_with_web_history(ncbi_search_block=ncbi_search_blocks[[1]], taxon="Chlorophyta", block_size=5000)

get_fasta_with_web_history <- function(ncbi_search_block){
  id_vector <- ncbi_search_block$block_ids
  cookie <- ncbi_search_block$cookie
  query_key <- ncbi_search_block$qk
  block_size <- length(id_vector)
  start <- ncbi_search_block$start
  fasta <- tryCatch(rentrez::entrez_fetch(db="nuccore", web_history=cookie, query_key=query_key,
                        rettype="fasta", retstart=(start-1), retmax=block_size), warning=function(w) cat("entrez_fetch failed: Do you have a working internet connection?"))
  return(fasta)
}
