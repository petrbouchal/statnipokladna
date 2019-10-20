sp_base_url <- "http://monitor.statnipokladna.cz"

available_tables <- tidyr::tribble(~dataset_id, ~table_num, ~table_id, ~table_name, ~dataset_name, ~implemented)
available_codelists <- c(~id, ~name)

dataset_from_table <- function(table)

sp_dataset_url <- function(year = 2018, month = 12, dataset_id, table, check_if_exists = T) {
  x <- stringr::str_glue("{sp_base_url}/data/{year}_{month}_{dataset_id}.zip")
  if(check_if_exists) httr::url_success(x, httr::config(followlocation = 0L), USE.NAMES = FALSE)
  return(x)
}

sp_codelist_url <- function(codelist, check_if_exists = T) {
  x <- stringr::str_glue("{sp_base_url}/xsd/{codelist}.xml")
  if(check_if_exists) httr::url_success(x, httr::config(followlocation = 0L), USE.NAMES = FALSE)
  return(x)
}
