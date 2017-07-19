context("Making dada2 reference database")

test_that("make_dada2_ref_database outputs FASTA file in right format", {
  taxon="Athericidae"
  raw_fasta_file_name <- make_raw_fasta_name(taxon=taxon, directory="./")
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  classification_file_name <- make_classification_name(taxon=taxon, directory="./")
  dada2_fasta_file_name <- make_dada2_fasta_name(taxon=taxon, directory="./")

  ncbi_record_blocks <- get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon="Athericidae")
  id_list <- extract_ids_from_ncbi_search_blocks(ncbi_record_blocks)
  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name, id_list=id_list)
  check_and_do_classification(taxon=taxon, classification_file_name = classification_file_name,
                              id_list=id_list)
  check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                     mapping_file_name=mapping_file_name, ncbi_record_blocks=ncbi_record_blocks)
  expect_error(make_dada2_ref_database(taxon="Athericidae", raw_fasta_file_name=raw_fasta_file_name,
                          mapping_file_name=mapping_file_name, classification_file_name=classification_file_name,
                          dada2_fasta_file_name=dada2_fasta_file_name, include.identifier="my_id"), 
               regexp = "include.identifier must have a value of 'none', 'uid', 'accn', or 'bold'")
  
  make_dada2_ref_database(taxon="Athericidae", raw_fasta_file_name=raw_fasta_file_name,
                          mapping_file_name=mapping_file_name, classification_file_name=classification_file_name,
                          dada2_fasta_file_name=dada2_fasta_file_name)

  expect_true(file.exists(dada2_fasta_file_name))

  dada2_results <- ape::read.FASTA(file=dada2_fasta_file_name)

  expect_type(names(dada2_results), "character")
  expect_match(names(dada2_results), regexp="[A-Za-z]")
  expect_match(names(dada2_results), regexp="([^;]*;){6}[^;]*$")

  file.remove(raw_fasta_file_name)
  file.remove(mapping_file_name)
  file.remove(classification_file_name)
  file.remove(dada2_fasta_file_name)
})


test_that("make_dada2_ref_database adds correct identifier values when include.identifier is set", {
  taxon="Athericidae"
  raw_fasta_file_name <- make_raw_fasta_name(taxon=taxon, directory="./")
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  classification_file_name <- make_classification_name(taxon=taxon, directory="./")
  dada2_fasta_file_name <- make_dada2_fasta_name(taxon=taxon, directory="./")
  
  ncbi_record_blocks <- get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon=taxon)
  id_list <- extract_ids_from_ncbi_search_blocks(ncbi_record_blocks)
  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name, id_list=id_list)
  check_and_do_classification(taxon=taxon, classification_file_name = classification_file_name,
                              id_list=id_list)
  check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                     mapping_file_name=mapping_file_name, ncbi_record_blocks=ncbi_record_blocks)
 
  make_dada2_ref_database(taxon=taxon, raw_fasta_file_name=raw_fasta_file_name,
                          mapping_file_name=mapping_file_name, classification_file_name=classification_file_name,
                          dada2_fasta_file_name=dada2_fasta_file_name, include.identifier="uid")
  expect_true(file.exists(dada2_fasta_file_name))
  dada2_results <- ape::read.FASTA(file=dada2_fasta_file_name)
  expect_match(names(dada2_results), regexp="([^;]*;){7}[^;]*$")
  expect_match(names(dada2_results), regexp="uid\\:[0-9]")
  file.remove(dada2_fasta_file_name)
  
  make_dada2_ref_database(taxon="Athericidae", raw_fasta_file_name=raw_fasta_file_name,
                          mapping_file_name=mapping_file_name, classification_file_name=classification_file_name,
                          dada2_fasta_file_name=dada2_fasta_file_name, include.identifier="bold")
  expect_true(file.exists(dada2_fasta_file_name))
  dada2_results <- ape::read.FASTA(file=dada2_fasta_file_name)
  expect_match(names(dada2_results), regexp="([^;]*;){7}[^;]*$")
  expect_match(names(dada2_results), regexp="bold\\:")
  file.remove(dada2_fasta_file_name)
  
  make_dada2_ref_database(taxon="Athericidae", raw_fasta_file_name=raw_fasta_file_name,
                          mapping_file_name=mapping_file_name, classification_file_name=classification_file_name,
                          dada2_fasta_file_name=dada2_fasta_file_name, include.identifier="accn")
  expect_true(file.exists(dada2_fasta_file_name))
  dada2_results <- ape::read.FASTA(file=dada2_fasta_file_name)
  expect_match(names(dada2_results), regexp="([^;]*;){7}[^;]*$")
  expect_match(names(dada2_results), regexp="accn\\:[A-Z0-9]")
  file.remove(dada2_fasta_file_name)
  
  file.remove(raw_fasta_file_name)
  file.remove(mapping_file_name)
  file.remove(classification_file_name)

})
