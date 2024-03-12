test_that("List of tables make sense", {
  expect_equivalent(unique(sp_tables), sp_tables)
  expect_equivalent(unique(sp_tables$id), sp_tables$id)
  expect_equivalent(unique(sp_tables$id), sp_tables$id)
  expect_equivalent(sort(sp_tables$id), sp_tables$id)
  expect_equivalent(dplyr::distinct(sp_tables), sp_tables)
})

test_that("get_table works", {
  expect_error(sp_get_table("blah", "2012", "12"))
})

test_that("get_table works with newer files", {
  expect_gt(nrow(sp_get_table("budget-local", "2022", "12")), 1000)
})
