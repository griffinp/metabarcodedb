#' check_and_do_classification
#'
#' This function performs taxonomic classification using the taxize::classification function,
#' on a list of vectors of NCBI UIDs such as that returned by extract_ids_from_ncbi_search_blocks.
#' This function is quite slow because it is unable to use the web history (this produces only unique taxon IDs, which will not map 1-to-1 to sequence UIDs)
#' It returns the extracted classification values, one per id, in a list.
#' @param id_list A list of character vectors of NCBI UIDs, such as that produced by extract_ids_from_ncbi_search_blocks.
#' @param chunk_size An integer giving the size of chunks into which each id vector should be split. This is needed to return the results properly
#' @param classification_file_name Path to the classification file (character string)
#' @param block_size An integer giving the size of blocks into which the full vector of IDs with missing classification info is split. Default = 5000
#' @keywords ncbi uid taxonomy classification
#' @importFrom taxize classification
#' @export
#' @examples
#' check_and_do_classification(taxon="Hypogasturidae", classification_file_name=make_classification_file_name(taxon="Hypogasturidae", directory="~/Documents/classification_files"), id_list=list('1072897717', '329757399', '259881131'),  chunk_size=60)

check_and_do_classification <- function(taxon, classification_file_name, id_list, chunk_size=60, block_size=5000){

  if(file.exists(classification_file_name)){ # if classification file exists
    existing_classification_ids <- extract_ids_from_classification_file(classification_file_name)
    if(is.null(existing_classification_ids)==FALSE){
      #message(paste("Some classifications exist already for", taxon))
      # check for 'NA' classifications and remove them so they can be tried again
      # no test for this bit yet
      existing_classifications <- readRDS(classification_file_name)
      na_classif <- lapply(existing_classifications, FUN=function(x){all(is.na(x))})
      non_na_classif <- existing_classifications[which(na_classif==FALSE)]
      saveRDS(non_na_classif, file=classification_file_name)
      
      existing_ids_as_vector <- unlist(existing_classification_ids)
      ids_to_fetch_as_vector <- unlist(id_list)[-which(unlist(id_list)%in%existing_ids_as_vector)]
      #ids_to_fetch_as_vector <- unlist(ids_to_fetch_as_prelist)
      if(length(ids_to_fetch_as_vector) > 0){
        ids_to_fetch_as_newlist <- split(ids_to_fetch_as_vector, ceiling(seq_along(ids_to_fetch_as_vector)/block_size))
        no_blocks <- length(ids_to_fetch_as_newlist)
        iterator <- 1
        for(id_block in ids_to_fetch_as_newlist){
          # doing this as a for loop instead of a lapply
          # because this allows writing out to file in each loop iteration
          # instead of waiting until the end
          message(paste("Classifying block", iterator, "of", no_blocks, "new classification blocks"))
          classification_as_list <- classify_id_vector(id_vector=unlist(id_block), taxon=taxon, chunk_size=chunk_size)
          existing_classifications <- readRDS(classification_file_name)
          # exclude any un-named classifications that might be present
          existing_classifications <- existing_classifications[which(names(existing_classifications)!="")]
          # add the new classifications
          updated_classifications <- existing_classifications
          updated_classifications[(length(existing_classifications)+1):(length(existing_classifications)+length(classification_as_list))] <- classification_as_list
          # ensure that the names of the new classifications get transferred
          names(updated_classifications)[(length(existing_classifications)+1):(length(existing_classifications)+length(classification_as_list))] <- names(classification_as_list)
          saveRDS(updated_classifications, file=classification_file_name)
          iterator <- iterator+1
        }
      } else{message(paste("Classification for", taxon, "is up to date."))}
    } else{
      message("Classification file exists, but has no named entries; remaking file")
      file.remove(classification_file_name)
      ids_to_fetch_as_vector <- unlist(id_list, recursive=TRUE)
      ids_to_fetch_as_newlist <- split(ids_to_fetch_as_vector, ceiling(seq_along(ids_to_fetch_as_vector)/block_size))
      no_blocks <- length(ids_to_fetch_as_newlist)
      iterator <- 1
      for(id_block in ids_to_fetch_as_newlist){
        message(paste("Classifying block", iterator, "of", no_blocks, "new classification blocks"))
        classification_as_list <- classify_id_vector(unlist(id_block), taxon=taxon, chunk_size=chunk_size)
        #classification <- lapply(classification_as_list, "[[", 1)
        if(iterator > 1){
          existing_classifications <- readRDS(classification_file_name)
          # exclude any un-named classifications that might be present
          existing_classifications <- existing_classifications[which(names(existing_classifications)!="")]
          # add the new classifications
          updated_classifications <- existing_classifications
          updated_classifications[(length(existing_classifications)+1):(length(existing_classifications)+length(classification_as_list))] <- classification_as_list
          # ensure that the names of the new classifications get transferred
          names(updated_classifications)[(length(existing_classifications)+1):(length(existing_classifications)+length(classification_as_list))] <- names(classification_as_list)
          saveRDS(updated_classifications, file=classification_file_name)
          iterator <- iterator+1
        } else{saveRDS(classification_as_list, file=classification_file_name)
          iterator <- iterator+1
        }
      }
    }

  } else{# if classification file doesn't yet exist:
    ids_to_fetch_as_vector <- unlist(id_list, recursive = TRUE)
    id_list <- split(ids_to_fetch_as_vector, ceiling(seq_along(ids_to_fetch_as_vector)/block_size))
    no_blocks <- length(id_list)
    iterator <- 1
    for(id_block in id_list){
      message(paste("Classifying block", iterator, "of", no_blocks, "new classification blocks"))
      classification_as_list <- classify_id_vector(unlist(id_block), taxon=taxon, chunk_size=chunk_size)
      if(iterator > 1){
        existing_classifications <- readRDS(classification_file_name)
        # exclude any un-named classifications that might be present
        existing_classifications <- existing_classifications[which(names(existing_classifications)!="")]
        # add the new classifications
        updated_classifications <- existing_classifications
        updated_classifications[(length(existing_classifications)+1):(length(existing_classifications)+length(classification_as_list))] <- classification_as_list
        # ensure that the names of the new classifications get transferred
        names(updated_classifications)[(length(existing_classifications)+1):(length(existing_classifications)+length(classification_as_list))] <- names(classification_as_list)
        saveRDS(updated_classifications, file=classification_file_name)
        iterator <- iterator+1
        } else{saveRDS(classification_as_list, file=classification_file_name)
        iterator <- iterator+1
        }
    }
  }
}
