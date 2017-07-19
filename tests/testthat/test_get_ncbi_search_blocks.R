context("Getting NCBI nuccore search results")

test_that("get_ncbi_search_blocks returns a list in appropriate format", {
  blocks <- get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon="Athericidae")
  expect_type(blocks, "list")
  expect_named(blocks[[1]], expected=c("cookie", "qk", "total_number", "block_ids", "start", "end"))
  expect_type(blocks[[1]]$cookie$WebEnv, "character")
  expect_match(blocks[[1]]$cookie$WebEnv, regexp='[a-zA-Z0-9_\\.]')
  expect_type(blocks[[1]]$qk$QueryKey, "character")
  expect_match(blocks[[1]]$qk$QueryKey, regexp='[0-9]')
  expect_type(blocks[[1]]$total_number, "integer")
  expect_type(blocks[[1]]$block_ids, "character")
  expect_match(blocks[[1]]$block_ids, regexp='[0-9]')
  expect_type(blocks[[1]]$start, "double")
  expect_type(blocks[[1]]$end, "double")
  expect_lt(blocks[[1]]$start, blocks[[1]]$end)
})

test_that("get_ncbi_search_blocks prints the desired messages", {
  expect_message(get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon="Athericidae"),
                 regexp="NCBI records found for")
  expect_message(get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon="Athericidae"),
                 regexp="Got NCBI records")
})

test_that("get_ncbi_search_blocks outputs list of lists, each containing up to record_block_size UIDs", {
  record_block_size <- 7
  blocks <- get_ncbi_search_blocks(gene_names=c("COI", "CO1", "COX1"), taxon="Athericidae", record_block_size=record_block_size)
  expect_gt(length(blocks), 1)
  expect_equal(length(blocks[[1]]$block_ids), record_block_size)
  all_block_ids <- unlist(lapply(blocks, "[", 4))
  expect_equal(length(all_block_ids), blocks[[1]]$total_number)
})
