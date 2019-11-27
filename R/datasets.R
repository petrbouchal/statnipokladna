
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
#'
#' See <https://monitor.statnipokladna.cz/2019/zdrojova-data/transakcni-data> for a more detailed descriptions
#' of the datasets.
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{\code{id}}{character. Dataset ID, used as `id` argument to `get_dataset`.}
#'   \item{\code{name}}{character. Dataset name, mostly corresponds to title on the statnipokladna GUI.}
#' }
#' @family Lists of available entities
sp_datasets <- sp_datasets_i %>% dplyr::select(id, name)
# usethis::use_data(sp_datasets, overwrite = T)

get_dataset_url <- function(dataset_id, year = 2018, month = 12, check_if_exists = T) {
  if(!(dataset_id %in% sp_datasets_i$id)) stop("Not a valid dataset ID")
  dataset_name <- sp_datasets_i[sp_datasets_i$id == dataset_id, "name"]
  message(stringr::str_glue("Building URL for dataset `{dataset_id}`: {dataset_name}, {year}-{month}"))
  x <- stringr::str_glue("{sp_base_url}/data/{year}_{month}_Data_CSUIS_{toupper(dataset_id)}.zip")
  print(x)
  if(check_if_exists) {
    iserror <- httr::http_error(x, httr::config(followlocation = 0L), USE.NAMES = FALSE)
    if(iserror) stop("File does not exist for this dataset and period combination.")
  }
  doc_url <- stringr::str_glue("{sp_base_url}/data/struktura/{dataset_id}.xlsx")
  message(stringr::str_glue("Get the dataset documentation at {doc_url}"))
  return(x)
}


#' Get dataset documentation
#'
#' Downloads XLS file with dataset documentation, or opens link to this file in browser.
#'
#' @param dataset_id dataset ID. See `sp_datasets`.
#' @param destdir character. Where the file should be written. Defaults to working directory.
#' @param download Whether to download (the default) or open link in browser.
#'
#' @return character (link) if download = TRUE, nothing otherwise.
#' @family Helpers
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
get_dataset_doc <- function(dataset_id, destdir = ".", download = T) {
  doc_url <- stringr::str_glue("{sp_base_url}/data/struktura/{dataset_id}.xlsx")
  if(!download) {
    browseURL(doc_url)
    return(stringr::str_glue("Link to file opened in browser."))
    } else {
    file_path <- stringr::str_glue("{destdir}/{dataset_id}.xlsx")
    message(stringr::str_glue("Getting dataset documentation from {doc_url}"))
    download.file(doc_url, file_path)
    return(stringr::str_glue("File downloaded to {file_path}."))
  }
}


#' Retrieve dataset from statnipokladna
#'
#' Downloads and unzips files for a given dataset.
#'
#' @param dataset_id A dataset ID. See `id` column in `sp_datasets` for a list of available codelists.
#' @param year year, numeric, 2015-2018 for some datasets, 2010-2018 for others.
#' @param month month, numeric. Must be 3, 6, 9 or 12.
#' @param force_redownload Redownload even if recent file present? Defaults to FALSE.
#'
#' @return character string with complete paths to downloaded files.
#' @examples
#' # ADD_EXAMPLES_HERE
#' @family Core workflow
#' @export
get_dataset <- function(dataset_id, year = 2018, month = 12, force_redownload = F) {
  if(!(month %in% c(3, 6, 9, 12))) stop("`Month` must be 3, 6, 9, or 12")
  month <- formatC(month, width = 2, format = "d", flag = "0")
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
  paste0(td, "/", list.files(td, pattern = paste0("*_", year, "0", month, ".csv")))
}

