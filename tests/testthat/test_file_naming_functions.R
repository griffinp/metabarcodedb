context("Creating output file names")

test_that("make_mapping_name outputs a character string", {

  mapping_name <- make_mapping_name(taxon="Taxon1",
                                    directory="~/Documents/mapping_files")
  expect_type(mapping_name, 'character')
})

test_that("make_classification_name outputs a character string", {

  classification_name <- make_classification_name(taxon="Taxon1",
                                    directory="~/Documents/classification_files")
  expect_type(classification_name, 'character')
})

test_that("make_raw_fasta_name outputs a character string", {

  raw_fasta_name <- make_raw_fasta_name(taxon="Taxon1",
                                           directory="~/Documents/fasta_files")
  expect_type(raw_fasta_name, 'character')
})

test_that("make_dada2_fasta_name outputs a character string", {

  dada2_fasta_name <- make_dada2_fasta_name(taxon="Taxon1",
                                        directory="~/Documents/fasta_files")
  expect_type(dada2_fasta_name, 'character')
})

