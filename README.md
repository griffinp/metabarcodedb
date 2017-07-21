# metabarcodedb
R package for making a metabarcoding reference database 

This R package has been developed in order to create a custom reference database for a metabarcoding project.
For an input vector of taxon names and an input vector of gene names, the package provides tools to 
download all sequences from the NCBI nuccore and BOLD databases. 
It also obtains the taxonomic classification of each sequence and provides a tool to format the fasta file
to include the taxonomic classification in the header, in the format required by the dada2 package for further
downstream processing. 

Current status (as of 19th July 2017) - still very much in development, use at your own risk!
