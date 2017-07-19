context("Checking for existing sequences and adding missing info")

test_that("check_and_get_seqs outputs a .fasta file of the right length and format", {
  taxon="Athericidae"
  raw_fasta_file_name <- make_raw_fasta_name(taxon=taxon, directory="./")
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                     mapping_file_name = mapping_file_name, ncbi_record_block = ncbi_record_block)
  expect_true(file.exists(raw_fasta_file_name))
  fasta_object <- ape::read.FASTA(raw_fasta_file_name)
  expect_s3_class(fasta_object, "DNAbin")
  expect_equal(length(fasta_object), ncbi_record_block[[1]]$total_number)
  expect_match(names(fasta_object), regexp="[A-Z0-9]\\.[0-9]")
  file.remove(raw_fasta_file_name)
})

test_that("check_and_get_seqs behaves properly when .fasta file exists but mapping file doesn't", {
  taxon="Athericidae"
  raw_fasta_file_name <- make_raw_fasta_name(taxon=taxon, directory="./")
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  if(file.exists(mapping_file_name)){file.remove(mapping_file_name)}
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                     mapping_file_name = mapping_file_name, ncbi_record_block = ncbi_record_block)
  expect_message(check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                     mapping_file_name = mapping_file_name, ncbi_record_block = ncbi_record_block), regexp = 'please create this file with check_and_do_mapping first')
  file.remove(raw_fasta_file_name)
})

test_that("check_and_get_seqs behaves properly when up-to-date .fasta and mapping files exist", {
  taxon="Athericidae"
  raw_fasta_file_name <- make_raw_fasta_name(taxon=taxon, directory="./")
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                       id_list=extract_ids_from_ncbi_search_blocks(ncbi_record_block))
  check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                     mapping_file_name = mapping_file_name, ncbi_record_block = ncbi_record_block)
  expect_message(check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                                    mapping_file_name = mapping_file_name, ncbi_record_block = ncbi_record_block), regexp = 'up to date')
  file.remove(mapping_file_name)
  file.remove(raw_fasta_file_name)
})

test_that("check_and_get_seqs behaves properly when out-of-date .fasta and up-to-date mapping files exist", {
  taxon="Athericidae"
  raw_fasta_file_name <- make_raw_fasta_name(taxon=taxon, directory="./")
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=10)
  complete_uids <- extract_ids_from_ncbi_search_blocks(ncbi_record_block)
  incomplete_uids <- complete_uids[[1]][1:5]
  fasta_object <- get_fasta_from_id_vector(id_vector=incomplete_uids)
  split_fasta_object <- unlist(strsplit(fasta_object, split=">"))[-1]
  write(fasta_object, file=raw_fasta_file_name)
  
  expect_lt(length(split_fasta_object), length(complete_uids[[1]]))

  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                       id_list=extract_ids_from_ncbi_search_blocks(ncbi_record_block), chunk_size = 5)
  check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                                    mapping_file_name = mapping_file_name, ncbi_record_block = ncbi_record_block)
  fasta_object <- ape::read.FASTA(raw_fasta_file_name)
  
  expect_equal(length(fasta_object), length(unlist(complete_uids)))
  expect_s3_class(fasta_object, "DNAbin")
  expect_match(names(fasta_object), regexp="[A-Z0-9]\\.[0-9]")

  file.remove(mapping_file_name)
  file.remove(raw_fasta_file_name)
})

test_that("check_and_get_seqs correctly removes duplicate entries from FASTA file", {
  taxon="Athericidae"
  raw_fasta_file_name <- make_raw_fasta_name(taxon=taxon, directory="./")
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                     mapping_file_name = mapping_file_name, ncbi_record_block = ncbi_record_block)
  
  fasta_without_duplicates <- ape::read.FASTA(raw_fasta_file_name)
  
  duplicates <- tryCatch(get_fasta_from_id_vector(id_vector=ncbi_record_block[[1]]["block_ids"][[1]][1:5],  block_size=5), error=function(e) NULL)
  
  write(duplicates, file=raw_fasta_file_name, append=TRUE)
  #fasta_object <- cbind(fasta_object, fasta_object[1:5])
  # names(fasta_object[(length(fasta_without_duplicates)+1):(length(fasta_without_duplicates)+5)]) <- names(fasta_without_duplicates[1:5])
  # fasta_object[(length(fasta_without_duplicates)+1):(length(fasta_without_duplicates)+5)] <- fasta_without_duplicates[1:5]
  # 
  fasta_with_duplicates <- ape::read.FASTA(raw_fasta_file_name)
  
  expect_equal(length(fasta_with_duplicates), length(fasta_without_duplicates)+5)

  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                       id_list=extract_ids_from_ncbi_search_blocks(ncbi_record_block))
  check_and_get_seqs(taxon=taxon, raw_fasta_file_name = raw_fasta_file_name,
                     mapping_file_name = mapping_file_name, ncbi_record_block = ncbi_record_block)
  fasta_after_duplicate_removal <- ape::read.FASTA(raw_fasta_file_name)
  expect_equal(length(fasta_after_duplicate_removal), length(fasta_without_duplicates))
  expect_false(any(duplicated(names(fasta_after_duplicate_removal))))
  file.remove(raw_fasta_file_name)
})
