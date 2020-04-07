test_that("List of datasets makes sense", {
  expect_equivalent(unique(sp_datasets), sp_datasets)
  expect_equivalent(unique(sp_datasets$id), sp_datasets$id)
  expect_equivalent(unique(sp_datasets$name), sp_datasets$name)
  expect_equivalent(sort(sp_datasets$id), sp_datasets$id)
  expect_equivalent(dplyr::distinct(sp_datasets), sp_datasets)
})

test_that("get_dataset breaks on nonsense", {
  expect_error(sp_get_dataset("blah", 2012, 10))
})

check_dataset_error <- function(dataset, year, month) {
  url <- get_dataset_url(dataset, year, month)
}

test_that("select downloads exist", {
  skip_on_cran()
  expect_type(purrr::map_chr(sp_datasets$id[sp_datasets$id != "finu"],
                             get_dataset_url), type = "character")
  expect_type(get_dataset_url("finm", 2012, "12"), "character")
  expect_error(get_dataset_url("finu")) # default dates should not work
  expect_error(get_dataset_url("x"))
})
