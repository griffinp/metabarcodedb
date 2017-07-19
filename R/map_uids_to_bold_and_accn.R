#' Map NCBI Unique Identifiers (UIDs) to Accession.Version and BOLD IDs
#'
#' This function extracts the BOLD (Barcode of Life Database) IDs, if present, and the accession.version IDs
#' from NCBI for a potentially very large input character vector of NCBI UIDs.
#' It returns a dataframe with columns uid, bold, and accn.
#' If any errors are encountered (e.g. in connecting to the Entrez API), the relevant chunk will be skipped
#' and the extraction will continue.
#' @param id_vector A character vector of NCBI UIDs
#' @param chunk_size An integer giving the size of chunks into which id_vector will be split for efficient extraction. Defaults to 60.
#' @param taxon Taxon name as a character string.
#' @keywords ncbi bold id
#' @export
#' @importFrom dplyr bind_rows
#' @examples
#' map_uids_to_bold_and_accn(id_vector=c('1072897717', '329757399', '259881131'), taxon="Hypogasturidae", chunk_size=60)

map_uids_to_bold_and_accn <- function(id_vector, taxon, chunk_size=60){
  # I played around with extracting sequences from the xml
  # but ran into a few strange cases where xml harvesting didn't work well
  # so am avoiding it at this point
  number_of_records <- length(id_vector)
  id_list <- split(id_vector, ceiling(seq_along(id_vector)/chunk_size))
  number_chunks <- length(id_list)
  bold_and_accn_list <- list()
  for(i in 1:number_chunks){
    chunk <- id_list[[i]]
    chunk_start <- (i*chunk_size)-chunk_size+1
    if(length(chunk)==chunk_size){
      chunk_end <- (i-1)*chunk_size+length(chunk)
    }else{chunk_end <- i*chunk_size}
    message(paste("Extracting BOLD ID and accession.version ID for records", chunk_start, "to", chunk_end, "for", taxon))
    #seqids <- id_list[[chunk]]
    bold_and_accn_list[[i]] <- tryCatch(extract_bold_and_accn(chunk), error=function(e) NULL)
  }
  non_null <- Filter(Negate(is.null), bold_and_accn_list)
  return(dplyr::bind_rows(non_null))
}
