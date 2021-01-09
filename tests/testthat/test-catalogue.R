catalogue_datasets <- sp_list_datasets()

test_that("catalogue is retrieved", {
  expect_gt(nrow(catalogue_datasets), 100)
})

test_that("catalogue shows some recent entries", {
  expect_gt(max(catalogue_datasets$end), as.Date("2020-06-30"))
})

test_that("all entries are zip files", {
  expect_true(all(stringr::str_detect(catalogue_datasets$url, "\\.(zip|ZIP)$")))
})

test_that("catalogue datasets match sp_datasets", {
  expect_true(all(sp_datasets_i$name_catalogue %in% catalogue_datasets$dataset_name))
})

catalogue_codelists <- sp_list_codelists()

test_that("catalogue is retrieved", {
  expect_gt(nrow(catalogue_codelists), 30)
})

test_that("all entries are xml files", {
  expect_true(all(stringr::str_detect(catalogue_codelists$url, "\\.(xml|XML)$")))
})

test_that("catalogue codelists match sp_codelists", {
  expect_true(all(sp_codelists$name %in% catalogue_codelists$codelist_name))
})
