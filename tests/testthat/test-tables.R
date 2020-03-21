test_that("List of tables make sense", {
  expect_equivalent(unique(sp_tables), sp_tables)
  expect_equivalent(unique(sp_tables$id), sp_tables$id)
  expect_equivalent(unique(sp_tables$table_code), sp_tables$table_code)
  expect_equivalent(sort(sp_tables$id), sp_tables$id)
  expect_equivalent(dplyr::distinct(sp_tables), sp_tables)
})

test_that("get_table works", {
  expect_error(sp_get_table("blah"))
})
