#' Makes a reference metabarcoding database in the format required by dada2
#' This function finds NCBI FASTA and taxonomy files as well as a mapping file
#' for a given taxon, and outputs a new FASTA file with headers formatted as required by dada2: e.g.
#' # >Level1;Level2;Level3;Level4;Level5;Level6;
#' ACCTAGAAAGTCGTAGATCGAAGTTGAAGCATCGCCCGATGATCGTCTGAAGCTGTAGCATGAGTCGATTTTCACATTCAGGGATACCATAGGATAC
#'
#' @param classif classification data frame for an individual taxon, created by taxize::classification
#' @keywords ncbi uid taxonomy classification taxize dada2
#' @export
#' @importFrom ape read.FASTA
#' @importFrom ape write.dna
#' @examples
#' format_taxonomic_levels(classif=Athericidae_classification[[1]])


make_dada2_ref_database <- function(taxon, raw_fasta_file_name,
                                    mapping_file_name, classification_file_name, dada2_fasta_file_name,
                                    taxonomic_levels=c("kingdom", "phylum", "class", "order", "family", "genus", "species"),
                                    include.identifier="none"){
  if(include.identifier%in%c("none", "uid", "accn", "bold")==FALSE){
    stop("include.identifier must have a value of 'none', 'uid', 'accn', or 'bold'")
  } else{
    message(paste("Converting FASTA to dada2 format for", taxon))
    # remove any '>' characters occurring not at the start of a line (nb this shouldn't occur
    # but does very occasionally)
    internal_arrow_matches <- as.integer(system(command=paste("grep '.>'", raw_fasta_file_name, "| wc -l"), intern=TRUE))
    if(internal_arrow_matches>=1){
      system(command=paste("sed -i .backup 's/.>/__/g'", raw_fasta_file_name), intern=FALSE)
      #system(command="rm *.backup")
      message(paste("Found and replaced internal '>' in", raw_fasta_file_name))
    }

    raw_fasta_file <- ape::read.FASTA(raw_fasta_file_name)
    classification_list <- readRDS(classification_file_name)
    mapping_file <- read.table(mapping_file_name, sep="\t", stringsAsFactors=FALSE, header=TRUE)
    
    # get accession_version vector from fasta
    fasta_acc_ver_ids <- as.data.frame(extract_acc_ver_ids_from_raw_fasta_object(raw_fasta_file))
    names(fasta_acc_ver_ids) <- "fasta_accn"
    
    # map to uid (using mapping file)
    acc_ver_to_uid_map <- merge(fasta_acc_ver_ids, mapping_file, by.x="fasta_accn", by.y="accn",
                                all=FALSE, sort=FALSE)
    ordered_map <- acc_ver_to_uid_map[match(fasta_acc_ver_ids$fasta_accn, acc_ver_to_uid_map$fasta_accn),]
    names(ordered_map)[which(names(ordered_map)=="fasta_accn")] <- "accn"
    fasta_uids <- ordered_map$uid
    
    # sort classification list so as to match to uid and extract dada2 FASTA headers
    sorted_classification <- classification_list[as.character(fasta_uids)]
    # for(i in 1:length(sorted_classification)){
    #   reduced <- reduce_taxonomic_levels(sorted_classification[[i]], taxonomic_levels=taxonomic_levels)
    # }

    subsetted_classification <- lapply(sorted_classification, FUN=reduce_taxonomic_levels, taxonomic_levels=taxonomic_levels)
    if(include.identifier!="none"){
      identifier_values <- paste(include.identifier, ordered_map[,include.identifier], sep=":")
      for(i in 1:length(subsetted_classification)){
        subsetted_classification[[i]] <- add_identifier_line_to_classification(subsetted_classification[[i]], identifier_value = identifier_values[i])
      }
    }
    
    # for(i in 1:length(subsetted_classification)){
    #   formatted <- format_taxonomic_levels(subsetted_classification[[i]])
    # }
    
    formatted_classification <- lapply(subsetted_classification, FUN=format_taxonomic_levels)
    # replace FASTA headers with new headers and write file
    dada2_fasta_file <- raw_fasta_file
    names(dada2_fasta_file) <- formatted_classification
    
    ape::write.dna(dada2_fasta_file, file = dada2_fasta_file_name, format = "fasta", nbcol=1, blocksep=0, colw=1e06)
    
    # replace any blank lines...
    
    system(command=paste("awk 'NF > 0' <", dada2_fasta_file_name, ">", paste(dada2_fasta_file_name, "_2", sep="")))
    system(command=paste("mv", paste(dada2_fasta_file_name, "_2", sep=""), dada2_fasta_file_name))
    
    # replace any weird characters

    system(command=paste("sed -E 's/(^>.*$)/#\\1#/'", dada2_fasta_file_name,
                         "| tr -d '\r' | tr -d '\n' | sed -e 's/$/#/' | tr '#' '\n' | sed -e '/^$/d' >", paste(dada2_fasta_file_name, "_2", sep="")))
    system(command=paste("mv", paste(dada2_fasta_file_name, "_2", sep=""), dada2_fasta_file_name))
    system(command=paste("sed '/^>/ s/$/;/'", dada2_fasta_file_name, "| sed 's/; ;/;;/g' | sed 's/ /_/g' >", paste(dada2_fasta_file_name, "_2", sep="")))
    system(command=paste("mv", paste(dada2_fasta_file_name, "_2", sep=""), dada2_fasta_file_name))
    system(command=paste("awk '{ if ($0 !~ />/) {print toupper($0)} else {print $0} }'", dada2_fasta_file_name, ">", paste(dada2_fasta_file_name, "_2", sep="")))
    system(command=paste("mv", paste(dada2_fasta_file_name, "_2", sep=""), dada2_fasta_file_name))
    
  }
}
