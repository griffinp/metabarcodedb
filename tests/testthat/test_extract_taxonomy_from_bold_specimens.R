context("Extracting taxonomic classification from BOLD specimen record")

test_that("extract_taxonomy_from_bold_specimens.R returns a list of dataframes with the correct structure", {
  bold_records <- bold::bold_specimens(taxon="Athericidae", format="tsv")
  extracted_taxonomy <- extract_taxonomy_from_bold_specimens(bold_specimen_record = bold_records, include_identifier = TRUE)
  
  expect_equal(length(extracted_taxonomy), nrow(bold_records))
  
})
