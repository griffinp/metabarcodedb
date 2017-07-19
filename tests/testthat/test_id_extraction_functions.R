context("Extracting and mapping IDs")

# extract_ids_from_ncbi_search_blocks

test_that("extract_ids_from_ncbi_search_blocks returns IDs as characters as a list containing 1 vector if 1 block", {
  blocks <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"), taxon="Athericidae", record_block_size=2000)
  ids <- extract_ids_from_ncbi_search_blocks(blocks)
  expect_type(ids, "list")
  expect_type(ids[[1]], "character")
  expect_match(ids[[1]], regexp='[0-9]')
})

test_that("extract_ids_from_ncbi_search_blocks returns IDs as characters as a list of vectors, if >1 block", {
  block_size=10
  blocks <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"), taxon="Athericidae", record_block_size=block_size)
  ids <- extract_ids_from_ncbi_search_blocks(blocks)
  expect_gt(length(ids), 1)
  expect_type(ids, "list")
  expect_type(ids[[1]], "character")
  expect_match(ids[[1]], regexp='[0-9]')

})

# extract_bold_from_entrez_xml

test_that("extract_bold_from_entrez_xml returns a vector of BOLD IDs of the right format and length", {
  id_vector=c('1072897717', '329757399', '259881131')
  fetched_xml <- rentrez::entrez_fetch(db="nuccore", id=id_vector, rettype="native", parsed=TRUE, complexity=2)
  extracted_bold <- extract_bold_from_entrez_xml(fetched_xml)
  expect_type(extracted_bold, "character")
  expect_true(is.na(extracted_bold[2]))
  expect_equal(extracted_bold[1], "ANTSP152-13.COI-5P")
  expect_length(extracted_bold, length(id_vector))
})

# extract_accn_version_from_entrez_xml

test_that("extract_accn_version_from_entrez_xml returns a vector of Accession.Version IDs of the right format and length", {
  id_vector=c('1072897717', '329757399', '259881131')
  fetched_xml <- rentrez::entrez_fetch(db="nuccore", id=id_vector, rettype="native", parsed=TRUE, complexity=2)
  extracted_acc_ver <- extract_accn_version_from_entrez_xml(fetched_xml)
  expect_type(extracted_acc_ver, "character")
  expect_match(extracted_acc_ver, regexp="[A-Z0-9]\\.[0-9]")
})

# write another test for extract_accn_version_from_entrez_xml for the
# rare case where a protein ID is also in the record?? (the function should deal with this but would be good to test)

# extract_bold_and_accn

test_that("extract_bold_and_accn returns a dataframe of UID, BOLD and Accession.Version IDs", {
  id_vector=c('1072897717', '329757399', '259881131')
  extracted_df <- extract_bold_and_accn(id_vector=id_vector)
  expect_type(extracted_df, "list")
  expect_named(extracted_df, expected = c("uid", "bold", "accn"))
  expect_match(extracted_df[,1], regexp='[0-9]')
  expect_match(extracted_df[,3], regexp='[A-Z0-9]\\.[0-9]')
  expect_equal(nrow(extracted_df), length(id_vector))
})

test_that("extract_ids_from_classification_file returns UIDs as a character vector", {
  taxon="Athericidae"
  classification_file_name <- make_classification_name(taxon=taxon, directory="./")
  check_and_do_classification(taxon=taxon, id_list=list(c("1109490020", "1109490018", "29171116")),
                              classification_file_name=classification_file_name)
  extracted_ids <- extract_ids_from_classification_file(classification_file_name)
  expect_type(extracted_ids, "character")
  expect_match(extracted_ids, regexp='[0-9]')
  file.remove(classification_file_name)
})

# map_uids_to_bold_and_accn

test_that("map_uids_to_bold_and_accn returns a dataframe containing UID, BOLD and Accession.Version IDs", {
  id_vector=c('1072897717', '329757399', '259881131')
  mapped_uids <- map_uids_to_bold_and_accn(id_vector=id_vector, taxon="Hypogasturidae", chunk_size=60)
  expect_type(mapped_uids, "list")
  expect_named(mapped_uids, expected=c("uid", "bold", "accn"))
  expect_match(mapped_uids$uid, regexp='[0-9]')
  expect_match(mapped_uids$accn, regexp='[A-Z0-9]\\.[0-9]')
  expect_equal(nrow(mapped_uids), length(id_vector))

})

test_that("map_uids_to_bold_and_accn properly handles cases where no. IDs > chunk_size", {
  id_vector <- c("1109490022", "1109490020", "1109490018", "29171116", "1004606911", "699123441", "699123438",
                 "699123435", "699123430", "699123426", "699123423", "699123420", "699123417", "699123411",
                 "686476599", "506954684", "506954654")
  mapped_uids <- map_uids_to_bold_and_accn(id_vector=id_vector, taxon="Athericidae", chunk_size=5)
  expect_type(mapped_uids, "list")
  expect_named(mapped_uids, expected=c("uid", "bold", "accn"))
  expect_match(mapped_uids$uid, regexp='[0-9]')
  expect_match(mapped_uids$accn, regexp='[A-Z0-9]\\.[0-9]')
  expect_equal(nrow(mapped_uids), length(id_vector))
})

test_that("get_existing_bold_from_mapping_file returns vector of BOLD IDs", {
  taxon="Athericidae"
  mapping_file_name <- make_mapping_name(taxon=taxon, directory="./")
  ncbi_record_block <- get_ncbi_search_blocks(gene_names=c("CO1", "COI", "COX1"),
                                              taxon=taxon, record_block_size=1000)
  check_and_do_mapping(taxon=taxon, mapping_file_name=mapping_file_name,
                       id_list=extract_ids_from_ncbi_search_blocks(ncbi_record_block))
  extracted_bold <- get_existing_bold_from_mapping_file(mapping_file_name=mapping_file_name)
  expect_match(extracted_bold, regexp='[a-zA-Z0-9]')
  expect_true(length(which(is.na(extracted_bold)))<1)
})