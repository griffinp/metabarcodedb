context("Formatting taxonomic levels for dada2")

test_that("format_taxonomic_levels returns a string of semicolon-delimited taxonomic levels", {
  full_tax_levels <- taxize::classification("Chironomus riparius", db = 'ncbi')
  levels_to_retain <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  reduced_tax_levels <- reduce_taxonomic_levels(full_tax_levels[[1]], levels_to_retain)
  formatted_tax_levels <- format_taxonomic_levels(reduced_tax_levels)

  expect_type(formatted_tax_levels, "character")
  expect_match(formatted_tax_levels, regexp="[A-Za-z]")
  expect_match(formatted_tax_levels, regexp="^(?:[^;]*;){6}[^;]*$")
})

test_that("reduce_taxonomic_levels.R returns a dataframe of the right format", {
  full_tax_levels <- taxize::classification("Chironomus riparius", db = 'ncbi')
  levels_to_retain <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  reduced_tax_levels <- reduce_taxonomic_levels(full_tax_levels[[1]], levels_to_retain)
  
  expect_type(reduced_tax_levels, "list")
  expect_equal(names(full_tax_levels[[1]]), names(reduced_tax_levels))
  expect_equal(reduced_tax_levels$rank, levels_to_retain)
  
})

test_that("add_identifier_line_to_classification.R properly adds a line containing the identifier info for ncbi format classification", {
  full_tax_levels <- taxize::classification("Chironomus riparius", db = 'ncbi')
  levels_to_retain <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
  reduced_tax_levels <- reduce_taxonomic_levels(full_tax_levels[[1]], levels_to_retain)
  identifier_added <- add_identifier_line_to_classification(reduced_tax_levels, identifier_value=paste("uid", "10101", sep=":"), 
                                                            classification_source = "ncbi")
  expect_type(identifier_added, "list")
  expect_equal(identifier_added$rank, c(levels_to_retain, "identifier"))
})

test_that("add_identifier_line_to_classification.R properly adds a line containing the identifier info for bold format classification", {
  full_records <- bold_specimens(taxon="Chironomus riparius", format="tsv")
  no_identifier <- extract_taxonomy_from_bold_specimens(full_records, include_identifier=FALSE)
  first_record <- no_identifier[1]
  identifier_added <- add_identifier_line_to_classification(first_record[[1]], identifier_value=paste("bold", names(first_record), sep=":"),
                                                            classification_source = "bold")
  expect_type(identifier_added, "list")
  expect_equal(identifier_added$rank, c("kingdom", "phylum", "class", "order", "family", "genus", "species", "identifier"))
})
