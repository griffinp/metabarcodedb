#' convert_gene_names_to_ncbi_query_text
#' 
#' This function converts a single gene name or a vector of alternative gene names
#' to properly-formatted text to include in a NCBI search query
#' @param gene_names A single gene name (as character) or a vector of gene names (as character)
#' @keywords ncbi gene sequence
#' @export
#' @examples 
#' convert_gene_names_to_ncbi_query_text(gene_names=c("COI", "CO1", "COX1"))

convert_gene_names_to_ncbi_query_text <- function(gene_names){
  gene_name_text <- paste(gene_names[1], "[Gene]", sep="")
  if(length(gene_names)>1){
    for(other_name in gene_names[2:length(gene_names)]){
      gene_name_text <- paste(gene_name_text, " OR ", 
                              other_name, "[Gene]", sep="")
    }
    gene_name_text <- paste("(", gene_name_text, ")", sep="")
  } 
  return(gene_name_text)
}
