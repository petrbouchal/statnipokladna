
sp_datasets_i <- tibble::tribble(~id, ~name, ~implemented,
                               "finm", "FIN 2-12 M - Pln\\u011bn\\u00ed rozpo\\u010dtu M\\u0158O", F,
                               "finu", "FIN 2-04 U - Pln\\u011bn\\u00ed rozpo\\u010dtu KAP, OSS a SF (2010 - 2014)", F,
                               "finsf", "FIN 2-04 U - Pln\\u011bn\\u00ed rozpo\\u010dtu SF", F,
                               "misris", "FIN 1-12 OSS - Pln\\u011bn\\u00ed rozpo\\u010dtu KAP a OSS", F,
                               "rozv", "Rozvaha", F,
                               "ppt", "P\\u0159ehled pen\\u011b\\u017en\\u00edch tok\\u016f", F,
                               "pozvk", "P\\u0159ehled o zm\\u011bn\\u00e1ch vlastn\\u00edho kapit\\u00e1lu", F,
                               "pril", "P\\u0159\\u00edloha", F,
                               "vykzz", "V\\u00fdkaz zisk\\u016f a ztr\\u00e1t", F) %>%
  dplyr::mutate(name = stringi::stri_unescape_unicode(name)) %>%
  dplyr::arrange(id)
# stringi::stri_escape_unicode("xxx")

#' List of available datasets
#'
#' Contains IDs and names of all available datasets that can be retrieved by get_dataset.
#' See <https://monitor.statnipokladna.cz/2019/zdrojova-data/transakcni-data> for a more detailed descriptions
#' of the datasets.
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{\code{id}}{character. Dataset ID, used as `id` argument to `get_dataset`.}
#'   \item{\code{name}}{character. Dataset name, mostly corresponds to title on the statnipokladna GUI.}
#' }
sp_datasets <- sp_datasets_i %>% dplyr::select(id, name)
# usethis::use_data(sp_datasets, overwrite = T)

get_dataset_url <- function(dataset_id, year = 2018, month = 12, check_if_exists = T) {
  if(!(month %in% c(3, 6, 9, 12))) stop("`Month` must be 3, 6, 9, or 12")
  if(!(dataset_id %in% sp_datasets$id)) stop("Not a valid dataset ID")
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
