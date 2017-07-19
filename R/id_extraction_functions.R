#' extract_ids_from_ncbi_search_blocks
#'
#' This function extracts NCBI UIDs from a ncbi_search_blocks object
#' created with the get_ncbi_search_blocks function.
#' It returns the extracted IDs only, as a list of lists *** IS THIS RIGHT? LIST OF VECTORS? ***
#' @param ncbi_search_blocks An object created with the get_ncbi_search_blocks function
#' @keywords ncbi uid
#' @export
#' @examples
#' extract_ids_from_ncbi_search_blocks(ncbi_search_blocks=Chlorophyta_blocks)

extract_ids_from_ncbi_search_blocks <- function(ncbi_search_blocks){
  extracted_ids_as_block_list <- lapply(ncbi_search_blocks, "[[", 4)
  return(extracted_ids_as_block_list)
}

#' extract_ids_from_classification_file
#'
#' This function extracts NCBI UIDs from a classification file
#' created with the check_and_do_classification function
#' It returns the extracted IDs only, as a character vector
#' @param classification_file_name A classification file (RDS format) created by the function check_and_do_classification
#' @keywords ncbi uid taxonomy
#' @export
#' @examples
#' extract_ids_from_classification_file(classification_file_name="Chlorophyta_classification.rds")

extract_ids_from_classification_file <- function(classification_file_name){
  classification_list <- readRDS(classification_file_name)
  classification_list <- classification_list[!sapply(classification_list, is.null)]
  ids <- names(lapply(classification_list, names))
  return(ids)
}



#' extract_acc_ver_ids_from_raw_fasta_object
#'
#' This function extracts NCBI accession.version ID from a NCBI-retrieved FASTA object
#' in the form of a list of class 'DNAbin' (i.e. as produced by ape::read.FASTA)
#' It returns the extracted IDs only, as a list of character vectors
#' @param fasta_object A FASTA object: a list of class 'DNAbin' as produced by ape::read.FASTA
#' @keywords ncbi uid taxonomy
#' @export
#' @import stringr
#' @examples
#' extract_acc_ver_ids_from_raw_fasta_object(fasta_object="Chlorophyta.fasta")

extract_acc_ver_ids_from_raw_fasta_object <- function(fasta_object){
  # extract accession.version ids from headers of fasta sequences
  headers <- names(fasta_object)
  # split headers, extract id field, convert to vector
  acc_ver_ids <- unlist(lapply(str_split(headers, " "), "[[", 1))
  # deal with the rare case where the acc_ver id doesn't appear properly in the fasta name...
  strange_format_ids <- grep(acc_ver_ids, pattern = "^:")
  if(length(strange_format_ids)>0){
    for(i in strange_format_ids){
      acc_ver_ids[i] <- paste(unlist(str_split(acc_ver_ids[i], pattern = ":"))[2], ".1", sep="")
    }
  }
  return(acc_ver_ids)
}

#' get_existing_ids_from_mapping_file
#'
#' This function extracts NCBI UIDs from a mapping file created by the *** function
#' It returns the extracted IDs only, as a character vector
#' @param mapping_file_name Path to the mapping file
#' @keywords ncbi uid taxonomy
#' @export
#' @examples
#' get_existing_ids_from_mapping_file(mapping_file_name="Chlorophyta_mapping.txt")

get_existing_ids_from_mapping_file <- function(mapping_file_name){
  mapping_file <- read.table(file=mapping_file_name, header=TRUE,
                             sep="\t", stringsAsFactors=FALSE)
  existing_seq_ids <- mapping_file$uid
  return(existing_seq_ids)
}

#' get_existing_bold_from_mapping_file
#'
#' This function extracts BOLD IDs from a mapping file created by the *** function
#' It returns the extracted IDs only, as a character vector
#' @param mapping_file_name Path to the mapping file
#' @keywords BOLD id taxonomy
#' @export
#' @examples
#' get_existing_bold_from_mapping_file(mapping_file_name="Chlorophyta_mapping.txt")

get_existing_bold_from_mapping_file <- function(mapping_file_name){
  mapping_file <- read.table(file=mapping_file_name, header=TRUE,
                             sep="\t", stringsAsFactors=FALSE)
  existing_bold_ids <- mapping_file$bold[-which(is.na(mapping_file$bold))]
  if(length(existing_bold_ids)>0){
    split_ids <- unlist(lapply(strsplit(existing_bold_ids, split="\\."), "[[", 1))
    return(split_ids)
  } else{return(c())}
}

#' get_accession_version_ids_from_fasta_file
#'
#' This function extracts NCBI accession.version ID from a NCBI-retrieved FASTA file
#' It returns the extracted IDs only, as a character vector
#' @param raw_fasta_file_name Path to the fasta file
#' @keywords ncbi fasta sequence metabarcoding
#' @importFrom ape read.FASTA
#' @export
#' @examples
#' get_accession_version_ids_from_fasta_file(raw_fasta_file_name="Chlorophyta.fasta")

get_accession_version_ids_from_fasta_file <- function(raw_fasta_file_name){
  fasta_file <- ape::read.FASTA(raw_fasta_file_name)
  acc_ver_ids <- extract_acc_ver_ids_from_raw_fasta_object(fasta_file)
  return(acc_ver_ids)
}
