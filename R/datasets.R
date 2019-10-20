sp_datasets <- tibble::tribble(~id, ~name, ~implemented,
                                      "finm", "FIN 2-12 M - Pln\\u011bn\\u00ed rozpo\\u010dtu M\\u0158O", F) %>%
  dplyr::mutate(name = stringi::stri_unescape_unicode(name))

dataset_from_table <- function(table) {
  dataset <- table
}

get_dataset_url <- function(dataset_id, year = 2018, month = 12, check_if_exists = T) {
  if(!(month %in% c(3, 6, 9, 12))) stop("`Month` must be 3, 6, 9, or 12")
  if(!(dataset_id %in% sp_datsets$id)) stop("Not a valid dataset ID")
  dataset_name <- sp_datasets[sp_datasets$id == dataset_id, "name"]
  month <- formatC(month, width = 2, format = "d", flag = "0")
  message(stringr::str_glue("Building URL for dataset `{dataset_id}`: {dataset_name}"))
  x <- stringr::str_glue("{sp_base_url}/data/{year}_{month}_Data_CSUIS_{toupper(dataset_id)}.zip")
  if(check_if_exists) {
    iserror <- httr::http_error(x, httr::config(followlocation = 0L), USE.NAMES = FALSE)
    if(iserror) stop("File for this does not exist for this dataset and period combination.")
  }
  doc_url <- stringr::str_glue("{sp_base_url}/data/struktura/{dataset_id}.xlsx")
  message(stringr::str_glue("Get the dataset documentation at {doc_url}"))
  return(x)
}

get_dataset <- function(dataset_id, year = 2018, month = 12, force_redownload = F) {
  dataset_url <- get_dataset_url(dataset_id = dataset_id, year = year, month = month)
  td <- tempdir()
  dir.create(paste0(td, "/statnipokladna"), showWarnings = F)
  dir.create(paste0(td, "/statnipokladna/", dataset_id), showWarnings = F)
  td <- paste0(td, "/statnipokladna/", dataset_id)
  tf <- paste0(td, "/", dataset_id, year, month, ".zip")
  if(file.exists(tf) & !force_redownload) {
    message(stringr::str_glue("Files already in {td}, not downloading. Set `force_redownload` to TRUE if needed."))
  } else {
    message(stringr::str_glue("Storing downloaded archive in and extracting to {td}"))
    utils::download.file(dataset_url, tf, headers = c('User-Agent' = usr))
    utils::unzip(tf, exdir = td)
  }
  list.files(td, pattern = "*.csv")
}
