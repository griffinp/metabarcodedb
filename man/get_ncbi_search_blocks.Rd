% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ncbi_search_blocks.R
\name{get_ncbi_search_blocks}
\alias{get_ncbi_search_blocks}
\title{get_ncbi_search_blocks}
\usage{
get_ncbi_search_blocks(gene_names, taxon, record_block_size = 5000)
}
\arguments{
\item{gene_names}{A single gene name (as character) or a vector of gene names (as character)}

\item{taxon}{Taxon name to search (as character)}

\item{record_block_size}{An integer giving the number of records to retrieve at once. Default = 5000. (Problems occur if more than approx. 10000 records are retrieved at once.)}
}
\description{
This function searches the NCBI nuccore database for records for the taxon provided
and the gene name/s provided.
The function returns a list of lists
Each list contains the cookie and query key for the webhistory,
the total no. records found for that query,
and a vector of IDs.
Each ID vector contains the NCBI sequence IDs returned for that block of records.
}
\examples{
get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon="Chlorophyta")
}
\keyword{gene}
\keyword{ncbi}
\keyword{sequence}
