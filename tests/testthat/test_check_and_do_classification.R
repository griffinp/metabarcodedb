context("Checking for existing classifications and adding missing info")

test_that("check_and_do_classification outputs an RDS file of the right format", {
  taxon="Athericidae"
  classification_file_name <- make_classification_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  id_list <- extract_ids_from_ncbi_search_blocks(ncbi_record_block)
  check_and_do_classification(taxon=taxon, id_list=id_list,
                              classification_file_name=classification_file_name)
  expect_true(file.exists(classification_file_name))
  classification_object <- readRDS(classification_file_name)
  ids <- names(classification_object)
  expect_equal(sort(ids), expected=sort(id_list[[1]]))
  file.remove(classification_file_name)
})

test_that("check_and_do_classification behaves properly when classification file exists and is up-to-date", {
  taxon="Athericidae"
  classification_file_name <- make_classification_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  id_list <- extract_ids_from_ncbi_search_blocks(ncbi_record_block)
  check_and_do_classification(taxon=taxon, id_list=id_list,
                              classification_file_name=classification_file_name)
  expect_message(check_and_do_classification(taxon=taxon, classification_file_name=classification_file_name,
                                      id_list=id_list),
                 regexp = 'up to date')
  file.remove(classification_file_name)
})

test_that("check_and_do_classification behaves properly when classification file exists but is out of date", {
  taxon="Athericidae"
  classification_file_name <- make_classification_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=5000)
  complete_uids <- extract_ids_from_ncbi_search_blocks(ncbi_record_block)
  incomplete_uids <- complete_uids[[1]][1:10]
  check_and_do_classification(taxon=taxon, classification_file_name=classification_file_name,
                       id_list=list(incomplete_uids))
  classification_file <- readRDS(file=classification_file_name)
  expect_lt(length(classification_file), ncbi_record_block[[1]]$total_number)

  check_and_do_classification(taxon=taxon, classification_file_name=classification_file_name,
                              id_list=complete_uids)
  classification_file <- readRDS(file=classification_file_name)
  expect_equal(length(classification_file), ncbi_record_block[[1]]$total_number)

  file.remove(classification_file_name)
})

test_that("check_and_do_classification handles case where id_list is split into blocks different from block_size", {

  # Not sure this test is particularly good

  taxon="Athericidae"
  classification_file_name <- make_classification_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=10)
  complete_uids <- extract_ids_from_ncbi_search_blocks(ncbi_record_block)
  incomplete_uids <- extract_ids_from_ncbi_search_blocks(ncbi_record_block)[[1]]
  check_and_do_classification(taxon=taxon, classification_file_name=classification_file_name,
                              id_list=list(incomplete_uids), block_size=5)
  classification_file <- readRDS(file=classification_file_name)
  expect_equal(length(classification_file), length(incomplete_uids))
  expect_match(names(classification_file), regexp='[0-9]')
  check_and_do_classification(taxon=taxon, classification_file_name=classification_file_name,
                              id_list=complete_uids, block_size=5)
  classification_file <- readRDS(file=classification_file_name)
  expect_equal(length(classification_file), ncbi_record_block[[1]]$total_number)
  expect_match(names(classification_file), regexp='[0-9]')
  file.remove(classification_file_name)
})
