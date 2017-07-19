context("Checking, obtaining and formatting extra records from BOLD")

test_that("check_and_get_bold_records returns FASTA file of right format", {
  taxon="Athericidae"
  marker="COI-5P|COI-3P"
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  dada2_file_name <- paste("./", taxon, "extra_records_dada2.fasta", sep="")
  
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                       id_list=extract_ids_from_ncbi_search_blocks(ncbi_record_block))
  check_and_get_bold_records(taxon=taxon, marker=marker, mapping_file_name=mapping_file_name,
                             dada2_file_name=dada2_file_name)
  
  expect_true(file.exists(dada2_file_name))
  bold_file <- ape::read.FASTA(file=dada2_file_name)
  expect_type(bold_file, "list")
  
  expect_match(names(bold_file), regexp="[A-Za-z0-9;]")
  file.remove(mapping_file_name)
  file.remove(dada2_file_name)
})

