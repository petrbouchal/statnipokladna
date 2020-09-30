test_that("List of codelists make sense", {
  expect_equivalent(unique(sp_codelists), sp_codelists)
  expect_equivalent(unique(sp_codelists$id), sp_codelists$id)
  expect_equivalent(unique(sp_codelists$name), sp_codelists$name)
  expect_equivalent(sort(sp_codelists$id), sp_codelists$id)
  expect_equivalent(dplyr::distinct(sp_codelists), sp_codelists)
})

test_that("get_codelist works", {
  expect_error(sp_get_codelist("blah"))
})

check_codelist_error <- function(codelist_id) {
  x <- stringr::str_glue("{sp_base_url}/data/xml/{codelist_id}.xml")
  iserror <- httr::http_error(x, httr::user_agent(usr))
  iserror
}

test_that("all codelists exist", {
  skip_on_cran()
  expect(!any(purrr::map_lgl(sp_codelists$id, check_codelist_error)), "some codelist do not exist")
})

