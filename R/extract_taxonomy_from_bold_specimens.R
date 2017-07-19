#' extract_taxonomy_from_bold_specimens
#'
#' This function works on a BOLD specimen record (tsv format)
#' obtained using the bold::bold_specimens function (and potentially filtered using filter_bold_specimens).
#' It extracts the taxonomy information (names only) for kingdom, phylum, class, order, family, genus, and species.
#' NB the BOLD specimen record only includes phylum, class, order, family, genus, species
#' so if a more detailed taxonomy is needed it is probably better to obtain it from ncbi or 
#' another database using taxize::classification on the taxon name. 
#' It returns a list of data.frames, one per specimen.
#' Each list contains a data.frame with columns 'name' (taxonomic level name) and 'rank' (taxonomic level rank).
#' 
#' @param bold_specimen_record a data.frame of BOLD specimen records obtained with bold::bold_specimens
#' @param include.identifier logical variable specifying whether the bold ID should be included as a taxonomic rank. Default: TRUE
#' @keywords bold metabarcoding
#' @importFrom bold bold_specimens
#' @export
#' @examples
#' extract_taxonomy_from_bold_specimens(bold_specimens(taxon="Athericidae", format="tsv"))

extract_taxonomy_from_bold_specimens <- function(bold_specimen_record, include_identifier=TRUE){
  tax_columns <- bold_specimen_record[,c("phylum_name", "class_name", "order_name",
                                         "family_name", "genus_name", "species_name")]
  tax_columns <- data.frame(tax_columns, stringsAsFactors=FALSE)
  colnames(tax_columns) <- ""
  ranks <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  identifier <- bold_specimen_record[,"processid"]
  tax_list <- list()
  for(i in 1:nrow(bold_specimen_record)){
    tax_names <- unlist(tax_columns[i,])
    tax_list[[i]] <- data.frame(name=c("Metazoa", tax_names), rank=ranks, stringsAsFactors=FALSE)
    tax_list[[i]][which(tax_list[[i]]$rank==" "), "rank"] <- ""
  }
  names(tax_list) <- identifier
  if(include_identifier==TRUE){
    for(i in 1:length(tax_list)){
      tax_list[[i]] <- add_identifier_line_to_classification(tax_list[[i]],
                                           identifier_value=paste("bold", identifier[i], sep=":"), 
                                           classification_source="bold")
    }
  }
  return(tax_list)
}