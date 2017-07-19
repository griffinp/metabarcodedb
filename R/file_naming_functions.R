#' make_mapping_name
#'
#' This function creates the file path for the mapping file (data.frame)
#' @param taxon taxon name (character string)
#' @param directory path to the directory containing the mapping file (character string)
#' @param suffix desired suffix for the file. Defaults to "_mapping.txt"
#' @keywords filename
#' @export
#' @examples
#' make_mapping_name(taxon="Chlorophyta", directory="~/Documents/metabarcoding/ref_datafiles/", suffix="_mapping.txt")

make_mapping_name <- function(taxon, directory, suffix="_mapping.txt"){
  # Makes the file path for the ncbi-to-bold mapping file
  mapping_name <- paste(directory, "/", taxon, suffix, sep="")
}

#' make_classification_name
#'
#' This function creates the file path for the classification file (an RDS object)
#' @param taxon taxon name (character string)
#' @param directory path to the directory containing the classification file (character string)
#' @param suffix desired suffix for the file. Defaults to "_classification.rds"
#' @keywords filename
#' @export
#' @examples
#' make_mapping_name(taxon="Chlorophyta", directory="~/Documents/metabarcoding/ref_datafiles/", suffix="_classification.rds")

make_classification_name <- function(taxon, directory, suffix="_classification.rds"){
  # Makes the file path for the classification file
  class_name <- paste(directory, "/", taxon, suffix, sep="")
}

#' make_raw_fasta_name
#'
#' This function creates the file path for the raw fasta file (fasta format)
#' @param taxon taxon name (character string)
#' @param directory path to the directory containing the fasta file (character string)
#' @param suffix desired suffix for the file. Defaults to ".fasta"
#' @keywords filename
#' @export
#' @examples
#' make_mapping_name(taxon="Chlorophyta", directory="~/Documents/metabarcoding/ref_datafiles/", suffix=".fasta")

make_raw_fasta_name <- function(taxon, directory, suffix=".fasta"){
  # Makes the file path for the fasta file
  fasta_name <- paste(directory, "/", taxon, suffix, sep="")
}

#' make_dada2_fasta_name
#'
#' This function creates the file path for the dada2-formatted fasta file (fasta format)
#' @param taxon taxon name (character string)
#' @param directory path to the directory containing the fasta file (character string)
#' @param suffix desired suffix for the file. Defaults to "_dada2.fasta"
#' @keywords filename
#' @export
#' @examples
#' make_mapping_name(taxon="Chlorophyta", directory="~/Documents/metabarcoding/ref_datafiles/", suffix="_dada2.fasta")


make_dada2_fasta_name <- function(taxon, directory, suffix="_dada2.fasta"){
  # Makes the file path for the fasta file
  fasta_name <- paste(directory, "/", taxon, suffix, sep="")
}
