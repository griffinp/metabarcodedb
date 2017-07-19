#' get_ncbi_search_blocks
#'
#' This function searches the NCBI nuccore database for records for the taxon provided
#' and the gene name/s provided.
#' The function returns a list of lists
#' Each list contains the cookie and query key for the webhistory,
#' the total no. records found for that query,
#' and a vector of IDs.
#' Each ID vector contains the NCBI sequence IDs returned for that block of records.
#' @param gene_names A single gene name (as character) or a vector of gene names (as character)
#' @param record_block_size An integer giving the number of records to retrieve at once. Default = 5000. (Problems occur if more than approx. 10000 records are retrieved at once.)
#' @param taxon Taxon name to search (as character)
#' @keywords ncbi gene sequence
#' @export
#' @import rentrez
#' @examples
#' get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon="Chlorophyta")

get_ncbi_search_blocks <- function(gene_names, taxon, record_block_size=5000){

  ncbi_result_blocks <- list()
  message(paste("Searching NCBI for", paste(gene_names, collapse=" "), "records for", taxon))
  gene_text <- convert_gene_names_to_ncbi_query_text(gene_names=gene_names)
  ncbi_query <- paste(gene_text, " AND ", taxon, "[ORGN]", sep="")
  # First, just extract the total number of records. 
  ncbi_search <- rentrez::entrez_search(db="nuccore", term=ncbi_query, use_history=TRUE, retmax=record_block_size)
  ncbi_result_blocks[[1]] <- list(cookie=ncbi_search$web_history["WebEnv"],
                                  qk=ncbi_search$web_history["QueryKey"],
                                  total_number=ncbi_search$count,
                                  block_ids=ncbi_search$ids,
                                  start=1,
                                  end=record_block_size)
  total_number <- ncbi_result_blocks[[1]]$total_number

  message(paste(total_number, "NCBI records found for", taxon))

  if(total_number>0 & total_number<record_block_size){message(paste("Got NCBI records", 1, "to", total_number, "of", total_number))} # Print update

  if(total_number>record_block_size){
    message(paste("Got NCBI sequence IDs", 1, "to", record_block_size, "of", total_number))
    starts <- seq(record_block_size+1, total_number, by=record_block_size)
    ends <- c((starts-1+record_block_size), total_number)
    ends <- ends[which(ends<=total_number)]
    for(iteration in 1:length(starts)){
      start <- starts[iteration]
      end <- ends[iteration]
      message(paste("Getting NCBI IDs", start, "to", end, "of", total_number))
      # 'retstart' value is zero-indexed!!
      ncbi_search_current <- rentrez::entrez_search(db="nuccore", term=ncbi_query, use_history=TRUE, retstart=(start-1), retmax=(end-start+1))
      ncbi_result_blocks[[iteration+1]] <- list(cookie=ncbi_search_current$web_history["WebEnv"],
                                                qk=ncbi_search_current$web_history["QueryKey"],
                                                total_number=ncbi_search_current$count,
                                                block_ids=ncbi_search_current$ids,
                                                start=start,
                                                end=end)
    }
  } # If more records than block size: multiple queries, add to list
  return(ncbi_result_blocks)
}
