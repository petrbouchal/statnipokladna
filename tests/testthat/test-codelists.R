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

test_that("key codelists parse correctly", {

  options(statnipokladna.dest_dir = NULL)

  skip_on_cran()
  key_codelist_names <- c("polozka", "paragraf", "polvyk", "paragraf_long")
  key_codelists <- purrr::map(key_codelist_names,
                              sp_get_codelist, redownload = T)

  cl_is_df <- purrr::map_lgl(key_codelists, is.data.frame)
  cl_has_complete_dates <- purrr::map_lgl(key_codelists,
                                          function(x) {
                                            complete_end <- all(!is.na(x$end_date))
                                            complete_start<- all(!is.na(x$start_date))
                                            return(all(complete_end, complete_start))
                                          })

  cl_has_sensible_dates <- purrr::map_lgl(key_codelists,
                                                function(x) {
                                                  df <- x %>%
                                                    dplyr::filter(start_date < "1000-01-01" |
                                                             end_date < "2000-01-01" |
                                                             start_date > "2030-01-01")
                                                  return(nrow(df) == 0)
                                                })
  cl_polozka <- key_codelists[[1]]
  nrow_cl_polozka <- nrow(cl_polozka)
  kon_cols <- c("kon_pol", "kon_rep", "kon_okr", "kon_kraj")


  expect_true(all(cl_is_df))
  expect_true(all(cl_has_complete_dates))
  expect_true(all(cl_has_sensible_dates))
  expect_true(all(kon_cols %in% names(cl_polozka)))
  expect_true(nrow(cl_polozka %>%
                     tidyr::drop_na(tidyselect::all_of(kon_cols))) == nrow_cl_polozka)
})
