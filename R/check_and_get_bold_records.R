#' check_and_get_bold_records
#'
#' This function examines an NCBI mapping file to see which BOLD records were already obtained from NCBI.
#' It then obtains the remaining public sequences from BOLD for the given taxon and marker combination.
#' It outputs a fasta file containing marker sequence with the taxonomy information (names only) for 
#' kingdom, phylum, class, order, family, genus, and species presented in the format required for
#' a dada2 reference database.
#' NB THIS FUNCTION CURRENTLY DOESN'T WORK - SEEMS TO BE DUE TO A BUG IN bold::bold_specimens
#' Have raised at https://github.com/ropensci/bold/issues/46 
#' 
#' @param taxon a taxon name as character string
#' @param marker One or more marker names in BOLD format (character string with pipe delimiter separating names)
#' @param mapping_file_name Path to mapping file created with check_and_do_mapping command
#' @param dada2_file_name Path to output file
#' @keywords bold metabarcoding ncbi fasta dada2
#' @importFrom bold bold_specimens
#' @importFrom bold bold_seq
#' @importFrom ape as.DNAbin
#' @importFrom ape write.dna
#' @export
#' @examples
#' check_and_get_bold_records(taxon="Athericidae", marker="COI-5P|COI-3P", mapping_file_name=make_mapping_name(taxon="Athericidae", directory="~/Documents/output/"), dada2_file_name=paste("~/Documents/output/", "Athericidae_extra_bold_seq_dada2.fasta", sep=""))


check_and_get_bold_records <- function(taxon, marker, mapping_file_name, dada2_file_name){
  if(!file.exists(dada2_file_name)){
    bold_specimen_records <-  tryCatch(bold::bold_specimens(taxon=taxon, format="tsv"), error=function(e) NULL)
    if(!is.null(get0("bold_specimen_records"))){
      bold_records_of_interest <- filter_bold_specimens(bold_specimen_records=bold_specimen_records, 
                                                        taxon = taxon, mapping_file_name=mapping_file_name)
      if(!is.null(bold_records_of_interest)){
        if(nrow(bold_records_of_interest)>0){
          message(paste("Checking", nrow(bold_records_of_interest), "extra records for", taxon, "from BOLD", sep=" "))
          bold_taxonomy <- extract_taxonomy_from_bold_specimens(bold_records_of_interest)
          taxonomy_formatted <- lapply(bold_taxonomy, format_taxonomic_levels)
          
          # Get Request-URI Too Long (HTTP 414) error if more than approx. 600 ids submitted at a time:
          # split into blocks of 500 names at a time to avoid this problem
          block_size <- 500
          ids_to_fetch_as_blocks <- split(names(taxonomy_formatted), ceiling(seq_along(names(taxonomy_formatted))/block_size))
          
          bold_sequences <- list()
          bold_sequences[[1]] <- bold::bold_seq(ids=ids_to_fetch_as_blocks[[1]], marker = marker)
          if(length(ids_to_fetch_as_blocks)>1){
            for(k in 2:length(ids_to_fetch_as_blocks)){
              message(paste("Obtaining block", k, "of", length(ids_to_fetch_as_blocks)))
              # Dealing with the occasional case where an ID does not seem to have retrievable records
              # (not sure what the cause is; could it be that these are not public sequences?)
              bold_sequences[[k]] <- tryCatch(bold::bold_seq(ids=ids_to_fetch_as_blocks[[k]], marker=marker), error=function(e) "Error retrieving records")
              if(bold_sequences[[k]][[1]] == "Error retrieving records"){
                for(j in 1:length(ids_to_fetch_as_blocks[[k]])){
                  bold_sequences[[k]][[j]] <- tryCatch(bold::bold_seq(ids=ids_to_fetch_as_blocks[[k]][j], marker=marker), error=function(e) NULL)
                }
              }
            }
          }
          bold_sequences <- unlist(bold_sequences, recursive=FALSE)
          bold_sequences <- bold_sequences[!is.null(bold_sequences)]
          message(paste("Obtained", length(bold_sequences), "records for", marker))
          extracted_sequences <- unlist(lapply(bold_sequences, "[[", "sequence"))
          extracted_ids <- unlist(lapply(bold_sequences, "[[", "id"))
          
          y <- as.matrix(strsplit(extracted_sequences, ""), tolower)
          rownames(y) <- names(taxonomy_formatted[extracted_ids])
          reformatted <- ape::as.DNAbin(y)
          names(reformatted) <- taxonomy_formatted[extracted_ids]
          
          ape::write.dna(reformatted, file=dada2_file_name, format = "fasta", nbcol=1, blocksep=0, colw=1e06)
        }
      }
    }
  }
}