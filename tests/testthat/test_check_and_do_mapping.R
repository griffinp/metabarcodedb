context("Checking for existing ID mapping and adding missing info")

test_that("check_and_do_mapping outputs a file containing a data.frame of the right length and format", {
  taxon="Athericidae"
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                       id_list=extract_ids_from_ncbi_search_blocks(ncbi_record_block))
  expect_true(file.exists(mapping_file_name))
  mapping_file <- read.table(file=mapping_file_name, header=TRUE,
                             sep="\t", stringsAsFactors=FALSE)
  expect_type(mapping_file, "list")
  expect_equal(nrow(mapping_file), ncbi_record_block[[1]]$total_number)
  expect_equal(length(mapping_file), 3)
  file.remove(mapping_file_name)
})

test_that("check_and_do_mapping behaves properly when mapping file exists and is up-to-date", {
  taxon="Athericidae"
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                       id_list=extract_ids_from_ncbi_search_blocks(ncbi_record_block))
  expect_message(check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                                      id_list=extract_ids_from_ncbi_search_blocks(ncbi_record_block)),
                 regexp = 'up to date')
  file.remove(mapping_file_name)
})

test_that("check_and_do_mapping behaves properly when mapping file exists but is out of date", {
  taxon="Athericidae"
  raw_fasta_file_name <- make_raw_fasta_name(taxon=taxon, directory="./")
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  complete_uids <- extract_ids_from_ncbi_search_blocks(ncbi_record_block)
  incomplete_uids <- complete_uids[[1]][1:10]
  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                       id_list=list(incomplete_uids))
  mapping_file <- read.table(file=mapping_file_name, header=TRUE,
                             sep="\t", stringsAsFactors=FALSE)
  expect_lt(nrow(mapping_file), ncbi_record_block[[1]]$total_number)

  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                       id_list=complete_uids)
  mapping_file <- read.table(file=mapping_file_name, header=TRUE,
                             sep="\t", stringsAsFactors=FALSE)
  expect_equal(nrow(mapping_file), ncbi_record_block[[1]]$total_number)

  #file.remove(mapping_file_name)
})
