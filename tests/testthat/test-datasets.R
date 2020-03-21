test_that("List of datasets makes sense", {
  expect_equivalent(unique(sp_datasets), sp_datasets)
  expect_equivalent(unique(sp_datasets$id), sp_datasets$id)
  expect_equivalent(unique(sp_datasets$name), sp_datasets$name)
  expect_equivalent(sort(sp_datasets$id), sp_datasets$id)
  expect_equivalent(dplyr::distinct(sp_datasets), sp_datasets)
})

test_that("get_dataset breaks on nonsense", {
  expect_error(get_dataset("blah"))
})
