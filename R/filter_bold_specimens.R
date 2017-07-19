#' filter_bold_specimens
#'
#' This function works on a BOLD specimen record (tsv format)
#' obtained using the bold::bold_specimens function.
#' It then filters to retain only those specimens not yet present in the mapping file
#' (since these records were already obtained from NCBI). The BOLD IDs as listed in NCBI
#' seem to map incompletely to the processids however, so an extra filtering step
#' is also performed, to remove any records that have the text 'NCBI' in the institution_storing field.
#' It returns the ******
#' @param bold_specimen_records a data.frame of BOLD specimen records obtained with bold::bold_specimens
#' @param mapping_file_name Path to a mapping file previously created with check_and_do_mapping
#' @keywords bold metabarcoding
#' @export
#' @examples
#' filter_bold_specimens(bold_specimens(taxon="Athericidae", format="tsv"), mapping_file_name="./Athericidae_mapping.txt")

filter_bold_specimens <- function(bold_specimen_records, taxon, mapping_file_name){
  if(file.exists(mapping_file_name)){
    existing_bold_ids <- get_existing_bold_from_mapping_file(mapping_file_name=mapping_file_name)
    if(!is.null(existing_bold_ids)){
      extra_bold_1 <- bold_specimen_records[-which(bold_specimen_records$processid%in%existing_bold_ids),]
      ncbi_mentioned <- grep("NCBI", extra_bold_1$institution_storing)
      if(length(ncbi_mentioned)>0){
        extra_bold_2 <- extra_bold_1[-ncbi_mentioned,]
        return(extra_bold_2)
      } else{return(extra_bold_1)}
    }
  } else{return(bold_specimen_records)}
}

