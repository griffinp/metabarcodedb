% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_taxonomy_from_bold_specimens.R
\name{extract_taxonomy_from_bold_specimens}
\alias{extract_taxonomy_from_bold_specimens}
\title{extract_taxonomy_from_bold_specimens}
\usage{
extract_taxonomy_from_bold_specimens(bold_specimen_record,
  include_identifier = TRUE)
}
\arguments{
\item{bold_specimen_record}{a data.frame of BOLD specimen records obtained with bold::bold_specimens}

\item{include.identifier}{logical variable specifying whether the bold ID should be included as a taxonomic rank. Default: TRUE}
}
\description{
This function works on a BOLD specimen record (tsv format)
obtained using the bold::bold_specimens function (and potentially filtered using filter_bold_specimens).
It extracts the taxonomy information (names only) for kingdom, phylum, class, order, family, genus, and species.
NB the BOLD specimen record only includes phylum, class, order, family, genus, species
so if a more detailed taxonomy is needed it is probably better to obtain it from ncbi or 
another database using taxize::classification on the taxon name. 
It returns a list of data.frames, one per specimen.
Each list contains a data.frame with columns 'name' (taxonomic level name) and 'rank' (taxonomic level rank).
}
\examples{
extract_taxonomy_from_bold_specimens(bold_specimens(taxon="Athericidae", format="tsv"))
}
\keyword{bold}
\keyword{metabarcoding}
