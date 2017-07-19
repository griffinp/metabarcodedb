#' Check for and Perform ID Mapping
#'
#' This function checks for an existing mapping file for the relevant taxon
#' in the given input directory with the given suffix
#' If the mapping file already exists, it checks whether any NCBI UIDs are missing
#' and adds the mapping info (NCBI UID, BOLD (Barcode of Life Database) ID and NCBI accession.version ID)
#' for those UIDs.
#' If the mapping file doesn't yet exist, it creates the file and performs the ID mapping.
#' Mapping is done 'chunk-wise' to fit with the NCBI API limitations.
#' If any errors are encountered during the mapping, (e.g. in connecting to the Entrez API), the relevant chunk will be skipped
#' and the extraction will continue.
#' @param taxon Taxon name as a character string.
#' @param mapping_file_name Path to the mapping file
#' @param id_vector A character vector of NCBI UIDs
#' @param chunk_size An integer giving the size of chunks into which id_vector will be split for efficient extraction. Defaults to 60.
#' @keywords ncbi bold id
#' @export
#' @importFrom dplyr bind_rows
#' @examples
#' check_and_do_mapping(taxon="Hypogasturidae", mapping_file_name, id_list <- list(c('1072897717', '329757399', '259881131')), chunk_size=60)

check_and_do_mapping <- function(taxon, mapping_file_name, id_list, chunk_size=60){
  if(file.exists(mapping_file_name)){ # if mapping file exists
    existing_mapping_ids <- get_existing_ids_from_mapping_file(mapping_file_name = mapping_file_name)
    
    ids_to_fetch_as_prelist <- lapply(id_list, function(x){x[which(x%in%existing_mapping_ids==FALSE)]})
    ids_to_fetch_as_vector <- unlist(ids_to_fetch_as_prelist, recursive = TRUE)
    if(length(ids_to_fetch_as_vector) > 0){
      message(paste("Mapping info required for", length(ids_to_fetch_as_vector), "IDs"))
      ids_to_fetch_as_newlist <- split(ids_to_fetch_as_vector, ceiling(seq_along(ids_to_fetch_as_vector)/chunk_size))
      mapping_as_list <- lapply(ids_to_fetch_as_newlist,
                                map_uids_to_bold_and_accn,
                                taxon = taxon, chunk_size=chunk_size)
      mapping <- bind_rows(mapping_as_list)
      write.table(mapping, file=mapping_file_name,
                  sep="\t", col.names=FALSE, row.names = FALSE, quote=FALSE, append=TRUE)
    } else{message(paste("Mapping for", taxon, "is up to date."))}
  } else{# if mapping file doesn't yet exist:
    mapping_as_list <- lapply(id_list,
                              map_uids_to_bold_and_accn,
                              taxon = taxon, chunk_size=chunk_size)
    mapping <- bind_rows(mapping_as_list)
    write.table(mapping, file=mapping_file_name,
                sep="\t", col.names=TRUE, row.names = FALSE, quote=FALSE)
  }
}
