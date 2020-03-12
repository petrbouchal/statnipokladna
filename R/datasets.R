
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
#'   \item{\code{id}}{character. Dataset ID, used as `dataset_id` argument to `get_dataset`.}
#'   \item{\code{name}}{character. Dataset name, mostly corresponds to title on the statnipokladna GUI.}
#' }
#' @family Lists of available entities
sp_datasets <- sp_datasets_i %>% dplyr::select(id, name) %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
# usethis::use_data(sp_datasets, overwrite = T)

get_dataset_url <- function(dataset_id, year = 2018, month = 12, check_if_exists = T) {
  if(!(dataset_id %in% sp_datasets_i$id)) stop("Not a valid dataset ID")
  dataset_name <- sp_datasets_i[sp_datasets_i$id == dataset_id, "name"]
  usethis::ui_info("Building URL for dataset {usethis::ui_value(dataset_id)}: {usethis::ui_value(dataset_name)}, {usethis::ui_value(stringr::str_c(year,'-',month))}")
  x <- stringr::str_glue("{sp_base_url}/data/{year}_{month}_Data_CSUIS_{toupper(dataset_id)}.zip")
  # print(x)
  if(check_if_exists) {
    iserror <- httr::http_error(x, httr::config(followlocation = 0L), USE.NAMES = FALSE)
    if(iserror) ui_stop("File does not exist for this dataset and period combination.")
  }
  doc_url <- stringr::str_glue("{sp_base_url}/data/struktura/{dataset_id}.xlsx")
  usethis::ui_info("Get the dataset documentation at {usethis::ui_path(doc_url)}")
  return(x)
}


#' Get dataset documentation
#'
#' Downloads XLS file with dataset documentation, or opens link to this file in browser.
#'
#' @param dataset_id dataset ID. See `sp_datasets`.
#' @param dest_dir character. Where the file should be written. Defaults to working directory.
#' @param download Whether to download (the default) or open link in browser.
#'
#' @return character (link) if download = TRUE, nothing otherwise.
#' @family Utilities
#' @examples
#' \dontrun{
#' get_dataset_doc("finm")
#' }
#' @export
get_dataset_doc <- function(dataset_id, dest_dir = ".", download = T) {
  doc_url <- stringr::str_glue("{sp_base_url}/data/struktura/{dataset_id}.xlsx")
  if(!download) {
    utils::browseURL(doc_url)
    return(stringr::str_glue("Link to file opened in browser."))
    } else {
    file_path <- stringr::str_glue("{dest_dir}/{dataset_id}.xlsx")
    message(stringr::str_glue("Getting dataset documentation from {doc_url}"))
    utils::download.file(doc_url, file_path, headers = c('User-Agent' = usr))
    return(stringr::str_glue("File downloaded to {file_path}."))
  }
}


#' Retrieve dataset from statnipokladna
#'
#' Downloads and unzips files for a given dataset.
#'
#' Files are stored in a temp folder as determined by `tempdir()` and further sorted into
#' subdirectories by dataset, year and month. They persist per session to avoid redownloads.
#'
#' @param dataset_id A dataset ID. See `id` column in `sp_datasets` for a list of available codelists.
#' @param year year, numeric, 2015-2018 for some datasets, 2010-2019 for others. Defaults to 2019.
#' @param month month, numeric. Must be between 1 and 12. Defaults to 12.
#' @param dest_dir character. Directory in which downloaded files will be stored. Defaults to `tempdir()`. Will be created if it does not exist.
#' @param redownload Redownload even if file has already been downloaded? Defaults to FALSE.
#'
#' @return character string with complete paths to downloaded files.
#' @examples
#' \dontrun{
#' budget_latest <- get_dataset("finu")
#' budget_2018 <- get_dataset("finu", 2018)
#' budget_mid2018 <- get_dataset("finu", 2018, 6)
#' budget_multiyear <- get_dataset("finu", 2012:2018, 9)
#' budget_multihalfyears <- get_dataset("finu", 2012:2018, c(6, 12))
#' }
#' @family Core workflow
#' @export
get_dataset <- function(dataset_id, year = 2019, month = 12,
                        dest_dir = tempdir(), redownload = F) {
  if(interactive() == FALSE & (missing(year) | missing(month))) {
    usethis::ui_warn("Either {usethis::ui_field('year')} or {usethis::ui_field('month')} not set.
                     Using defaults of {usethis::ui_value(year)} and {usethis::ui_value(month)}.
                     Set these values explicitly for reproducibility as the defaults may change in the future
                     to provide access to the latest data by default.")
  }
  if(!(month %in% c(1:12))) stop("`Month` must be an integer from 1 to 12.")
  if(!(year %in% c(2010:lubridate::year(lubridate::today())))) stop("`Year` must be between 2010 and now.")
  month <- formatC(month, width = 2, format = "d", flag = "0")
  td <- paste(dest_dir, "statnipokladna", dataset_id, year, month, sep = "/")
  dir.create(td, showWarnings = F, recursive = T)
  tf <- paste0(td, "/", dataset_id, year, month, ".zip")
  if(file.exists(tf) & !redownload) {
    usethis::ui_info("Files already in {td}, not downloading. Set {usethis::ui_code('redownload = TRUE')} if needed.")
  } else {
    dataset_url <- get_dataset_url(dataset_id = dataset_id, year = year, month = month)
    usethis::ui_done("Storing downloaded archive in and extracting to {usethis::ui_path(td)}")
    utils::download.file(dataset_url, tf, headers = c('User-Agent' = usr))
    utils::unzip(tf, exdir = td)
  }
  file_list <- paste0(td, "/", list.files(td, pattern = "(csv|CSV)$"))
  return(file_list)
}

