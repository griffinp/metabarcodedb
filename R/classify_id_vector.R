#' Perform Taxonomic Classification on a Vector of NCBI Unique Identifiers
#'
#' This function performs taxonomic classification using the taxize::classification function,
#' on a vector of NCBI UIDs.
#' This function is quite slow because it is unable to use the web history (this produces only unique taxon IDs, which will not map 1-to-1 to sequence UIDs)
#' It returns the extracted classification values, one per id, in a list.
#' Each classification value is a dataframe (see taxize::classification for more detail.)
#' @param id_vector A character vector of NCBI UIDs
#' @param chunk_size An integer giving the size of chunks into which the id vector is split. This is needed to return the results properly
#' @keywords ncbi uid taxonomy classification
#' @import rentrez
#' @importFrom taxize classification
#' @export
#' @examples
#' classify_id_vector(id_vector=c('1072897717', '329757399', '259881131'), taxon="Hypogasturidae", chunk_size=60)

classify_id_vector <- function(id_vector, chunk_size=60, taxon){
  #Extract taxonomy info for sequence ids
  #broken into chunks if vector is > chunk_size in length
  #returns classification as list of dataframes
  # Need to split ids into chunks (default size=60) to return properly; using the web history
  # object defaults to creating a list of unique identifiers, which then no longer
  # match clearly to seq records. Unfortunately this is a bit slow...
  test_number <- length(id_vector)
  taxon <- taxon
  classification_list <- list()
  if(test_number==1){
    taxids <- tryCatch(rentrez::entrez_link(dbfrom="nuccore", db="taxonomy",
                                   id=id_vector), error=function(e) NULL)
    unlisted_taxids <- taxids$links[1]
    class <- tryCatch(taxize::classification(unlisted_taxids, db="ncbi"), error=function(e) NULL)
    if(is.null(class)==FALSE){
      classification_list[1] <- class
    } else{
      message(paste("Error obtaining classifications for ID 1"))
      classification_list[1] <- NULL
    }
  }
  else {
    if(test_number>chunk_size){
      if(test_number%%chunk_size < 1){
        no_chunks <- test_number%/%chunk_size
      } else{no_chunks <- test_number%/%chunk_size + 1}
    } else{no_chunks <- 1}
    for(chunk in 1:no_chunks){
      chunk_start <- chunk*chunk_size-(chunk_size-1)
      if(no_chunks > 1 & chunk<no_chunks){
        chunk_end <- chunk*chunk_size
      }else{chunk_end <- test_number}
      message(paste("Classifying seq", chunk_start, "to", chunk_end, "for", taxon))
      chunk_taxids <- tryCatch(rentrez::entrez_link(dbfrom="nuccore", db="taxonomy", by_id=TRUE,
                                           id=id_vector[chunk_start:chunk_end]), error=function(e) NULL)
      unlisted_taxids <- unlist(lapply(lapply(chunk_taxids, "[[", 1), "[[", 1))
      chunk_class <- tryCatch(taxize::classification(unlisted_taxids, db="ncbi"), error=function(e) NULL)
      if(is.null(chunk_class)==FALSE){
        classification_list[chunk_start:chunk_end] <- chunk_class
      } else{
        message(paste("Error obtaining classifications for seq", chunk_start, "to", chunk_end))
        classification_list[chunk_start:chunk_end] <- NULL
      }
    }
  }
  names(classification_list) <- id_vector
  return(classification_list)
}
