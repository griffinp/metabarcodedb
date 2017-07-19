context("Taxonomic classification of UID vector")

test_that("classify_id_vector returns a list of dataframes containing characters", {
  classified <- classify_id_vector(id_vector=c('1072897717', '329757399', '259881131'), taxon="Hypogasturidae", chunk_size=60)
  expect_type(classified, 'list')
  expect_type(classified[[1]], 'list')
  expect_type(classified[[1]][[1]], 'character')
})

test_that("classify_id_vector list items have the expected structure", {
  classified <- classify_id_vector(id_vector=c('1072897717', '329757399', '259881131'), taxon="Hypogasturidae", chunk_size=60)
  expect_match(names(classified), regexp='[0-9]')
  expect_equal(length(classified[[1]]), 3)
  expect_named(classified[[1]], expected = c('name', 'rank', 'id'))
  expect_match(classified[[1]][,1], regexp='[a-zA-z]')
  expect_match(classified[[1]][,2], regexp='[a-z]')
  expect_match(classified[[1]][,3], regexp='[0-9]')
})

