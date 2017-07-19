#' Reduce taxonomic levels in a classification data frame
#'
#' This is a function to take a classification data frame (in the format
#' returned by the 'taxize::classification' function) and return new data frame
#' containing only the classification for the taxonomic levels specified
#' in the character vector 'taxonomic_levels'
#' @param classif classification data frame for an individual taxon, created by taxize::classification
#' @param taxonomic_levels character vector of the names of the taxonomic levels to retain
#' @keywords ncbi uid taxonomy classification taxize
#' @export
#' @examples
#' reduce_taxonomic_levels(classif=Athericidae_classification[[1]], taxonomic_levels=c("kingdom", "phylum", "class", "order", "family", "genus", "species"))


reduce_taxonomic_levels <- function(classif, taxonomic_levels){
  if(all(is.na(classif))){
    #message(paste("No classification found for ID", names(classif)))
    reduced_classif <- NA
  } else{
    levels_available <- which(taxonomic_levels%in%classif[["rank"]])
    missing_levels <- taxonomic_levels[-levels_available]
    for(missing_level in missing_levels){
      missing_level_df <- data.frame(name="", rank=missing_level, id="")
      classif <- rbind(classif, missing_level_df)
    }
    reduced_classif <- classif[match(taxonomic_levels, classif[["rank"]]),]
  }
  return(reduced_classif)
}
