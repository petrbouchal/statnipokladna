test_that("List of codelists make sense", {
  expect_equivalent(unique(sp_codelists), sp_codelists)
  expect_equivalent(unique(sp_codelists$id), sp_codelists$id)
  expect_equivalent(unique(sp_codelists$name), sp_codelists$name)
  expect_equivalent(sort(sp_codelists$id), sp_codelists$id)
  expect_equivalent(dplyr::distinct(sp_codelists), sp_codelists)
})

test_that("get_codelist works", {
  expect_error(get_codelist("blah"))
})
