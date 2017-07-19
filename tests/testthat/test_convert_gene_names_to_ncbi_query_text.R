context("Making NCBI query string")

test_that("convert_gene_names_to_ncbi_query_text outputs a character string", {
  gene_names_3 <- c("gene1", "gene2", "gene3")
  query_text_3 <- convert_gene_names_to_ncbi_query_text(gene_names=gene_names_3)
  expect_type(query_text_3, 'character')
})

test_that("convert_gene_names_to_ncbi_query_text is of right regex format for 1 gene", {
  gene_names_1 <- "gene1"
  query_text_1 <- convert_gene_names_to_ncbi_query_text(gene_names=gene_names_1)
  # check no whitespace
  expect_match(query_text_1, regexp='[:alnum:]')
  # should check the regexp below...
  expect_match(query_text_1, regexp='[:alnum:]{1}[Gene]')
})

test_that("convert_gene_names_to_ncbi_query_text is of right regex format for multiple genes", {
  gene_names_2 <- c("gene1", "gene2")
  gene_names_3 <- c("gene1", "gene2", "gene3")
  query_text_2 <- convert_gene_names_to_ncbi_query_text(gene_names=gene_names_2)
  query_text_3 <- convert_gene_names_to_ncbi_query_text(gene_names=gene_names_3)
  # test the regexp format of query_text_2
  expect_match(query_text_2, regexp='^\\([a-zA-Z0-9]+\\[Gene]\\sOR\\s[a-zA-Z0-9]+\\[Gene]\\)')
  # test the regexp format of query_text_3
  expect_match(query_text_3, regexp='^\\([a-zA-Z0-9]+\\[Gene]\\sOR\\s[a-zA-Z0-9]+\\[Gene]\\sOR\\s[a-zA-Z0-9]+\\[Gene]\\)')
  })
