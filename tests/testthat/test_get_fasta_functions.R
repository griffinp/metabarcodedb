context("Getting sequences as FASTA")

# get_fasta_from_id_vector

test_that("get_fasta_from_id_vector returns a FASTA object", {
  id_vector <- c('1072897717', '329757399', '259881131')
  fas <- get_fasta_from_id_vector(id_vector=id_vector)
  expect_type(fas, "character")
  expect_match(fas, regexp="^>[A-Z0-9]")
  # could do some better tests of format here
})

# test the web history version as well

test_that("get_fasta_with_web_history returns a FASTA object", {
  blocks <- get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon="Athericidae", record_block_size=5000)
  fas <- get_fasta_with_web_history(blocks[[1]])
  expect_type(fas, "character")
  expect_match(fas, regexp="^>[A-Z0-9]")
  # could do some better tests of format here
})

# test that get_fasta_with_web_history works properly with different values of start

test_that("get_fasta_with_web_history returns different sequences for different values of start", {
  blocks <- get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon="Athericidae", record_block_size=7)
  fas1 <- get_fasta_with_web_history(blocks[[1]])
  fas2 <- get_fasta_with_web_history(blocks[[2]])
  fas1_split <- strsplit(fas1, split = "\n")[[1]][1]
  fas2_split <- strsplit(fas2, split = "\n")[[1]][1]
  expect_failure(expect_identical(fas1_split, fas2_split))
})

