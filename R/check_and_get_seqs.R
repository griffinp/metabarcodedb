#' Check for and obtain FASTA sequences from NCBI
#'
#' This function checks for existing FASTA sequences that match the NCBI UIDs in a ncbi_record_blocks object (e.g. created by ***)
#' and adds to the FASTA file if some UIDs are missing. If the FASTA file doesn't yet exist,
#' it creates it and adds sequences for all NCBI UIDs in the ncbi_record_blocks object.
#' @param taxon Taxon name (character string)
#' @param raw_fasta_file_name Path to the raw fasta file (character string)
#' @param mapping_file_name Path to the mapping file (character string)
#' @param ncbi_record_blocks A list of lists of NCBI record info created by get_ncbi_search_blocks. This function will inherit the block_size from the ncbi_search_blocks (causes problems if it's specified differently).
#' @keywords ncbi uid taxonomy classification
#' @importFrom pbapply pblapply
#' @export
#' @examples
#' check_and_get_seqs("Chlorophyta", raw_fasta_file_name="~/Documents/metabarcoding/Chlorophyta.fasta", mapping_file_name="~/Documents/metabarcoding/Chlorophyta_mapping.txt",
#' ncbi_record_blocks=ncbi_record_blocks)

check_and_get_seqs <- function(taxon, raw_fasta_file_name, mapping_file_name, ncbi_record_blocks, remove_duplicates=TRUE){
    block_size <- length(ncbi_record_blocks[[1]][["block_ids"]])
    if(file.exists(raw_fasta_file_name)){ # if raw fasta file exists
      if(remove_duplicates){ #check for and remove duplicates
        raw_fasta_file <- ape::read.FASTA(raw_fasta_file_name)
        duplicate_names <- duplicated(names(raw_fasta_file))
        if(length(which(duplicate_names==TRUE))>=1){
          deduplicated <- raw_fasta_file[duplicate_names==FALSE]
          ape::write.dna(deduplicated, file=raw_fasta_file_name, format="fasta", nbcol = -1)
          message(paste("Removed", length(which(duplicate_names==TRUE)), "duplicate sequence records"))
        }
      }
      if(file.exists(mapping_file_name)==FALSE){
        message(paste('Mapping file', mapping_file_name, 'not found; please create this file with check_and_do_mapping first'))
      } else{
        existing_acc_ver_ids <- get_accession_version_ids_from_fasta_file(raw_fasta_file_name = raw_fasta_file_name)
        mapping <- read.table(mapping_file_name,
                              sep="\t", header=TRUE, stringsAsFactors=FALSE)
        existing_uids <- mapping[which(mapping$accn%in%existing_acc_ver_ids), 'uid']
        all_uids <- extract_ids_from_ncbi_search_blocks(ncbi_search_blocks=ncbi_record_blocks)
        
        message(paste(length(existing_acc_ver_ids), "seq already obtained;\n", 
                      length(unlist(all_uids)), "UIDs found at NCBI;\n", 
                      nrow(mapping), "entries in mapping file"))
        
        all_uids_as_vector <- unlist(all_uids)
        ids_to_fetch_as_vector <- all_uids_as_vector[-which(all_uids_as_vector%in%existing_uids)]
        if(length(ids_to_fetch_as_vector) > 0){
          ids_to_fetch_as_newlist <- split(ids_to_fetch_as_vector, ceiling(seq_along(ids_to_fetch_as_vector)/block_size))
          pboptions(type="txt", char=".", txt.width=length(ids_to_fetch_as_newlist))
          fasta_as_list <- pblapply(ids_to_fetch_as_newlist, 
                                    tryCatch(get_fasta_from_id_vector, error=function(e) NULL),
                                  block_size=block_size)
          fasta <- paste(unlist(fasta_as_list), collapse="\n")
          write(fasta, file=raw_fasta_file_name, append=TRUE)
        } else{message(paste("Sequences for", taxon, "are up to date."))}
      }
  } else{# if sequence file doesn't yet exist:
    message(paste("Getting", ncbi_record_blocks[[1]]["total_number"], "sequences for", taxon))

    pboptions(type="txt", char=".", txt.width=length(ncbi_record_blocks))
    fasta_as_list <- pblapply(ncbi_record_blocks,
                            tryCatch(get_fasta_with_web_history, error=function(e) NULL))
    fasta <- paste(unlist(fasta_as_list), collapse = "\n")
    write(fasta, file=raw_fasta_file_name)
  }
}
