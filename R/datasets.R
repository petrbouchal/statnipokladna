
sp_datasets_i <- tibble::tribble(~id, ~name, ~implemented, ~dir,
                                 "finm", "FIN 2-12 M - Pln\\u011bn\\u00ed rozpo\\u010dtu M\\u0158O", F, "FinM",
                                 "finu", "FIN 2-04 U - Pln\\u011bn\\u00ed rozpo\\u010dtu KAP, OSS a SF (2010 - 2014)", F, "FinU",
                                 "finsf", "FIN 2-04 U - Pln\\u011bn\\u00ed rozpo\\u010dtu SF", F, "FinSF",
                                 "misris", "FIN 1-12 OSS - Pln\\u011bn\\u00ed rozpo\\u010dtu KAP a OSS", F, "FinOSS",
                                 "rozv", "Rozvaha", F, "Rozvaha",
                                 "ppt", "P\\u0159ehled pen\\u011b\\u017en\\u00edch tok\\u016f", F, "PenezniToky",
                                 "pozvk", "P\\u0159ehled o zm\\u011bn\\u00e1ch vlastn\\u00edho kapit\\u00e1lu", F, "VlastniKapital",
                                 "pril", "P\\u0159\\u00edloha", F, "Priloha",
                                 "vykzz", "V\\u00fdkaz zisk\\u016f a ztr\\u00e1t", F, "ZiskZtraty") %>%
  dplyr::mutate(name = stringi::stri_unescape_unicode(.data$name)) %>%
  dplyr::arrange(.data$id)
# stringi::stri_escape_unicode("xxx")

#' List of available datasets
#'
#' Contains IDs and names of all available datasets that can be retrieved by get_dataset.
#'
#' See <https://monitor.statnipokladna.cz/datovy-katalog/transakcni-data> for a more detailed descriptions
#' of the datasets.
#'
#' @format A data frame with 9 rows and 3 variables:
#' \describe{
#'   \item{\code{id}}{character. Dataset ID, used as `dataset_id` argument to `sp_get_dataset`.}
#'   \item{\code{name}}{character. Dataset name, mostly corresponds to title on the statnipokladna GUI.}
#' }
#' @family Lists of available entities
sp_datasets <- sp_datasets_i %>% dplyr::select("id", "name") %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
# usethis::use_data(sp_datasets, overwrite = TRUE)


#' Get URL of dataset
#'
#' Useful for workflows where you want to keep track of URLs and intermediate files, rather
#' than having all steps performed by one function.
#'
#' @param dataset_id Dataset ID. See `id` column in `sp_datasets` for a list of available codelists.
#' @param year year, numeric vector of length <= 1 (can take multiple values), 2015-2019 for some datasets, 2010-2020 for others.
#' (see Details for how to work with data across time periods.)
#' @param month month, numeric vector of length <= 1 (can take multiple values). Must be between 1 and 12. Defaults to 12.
#' (see Details for how to work with data across time periods.)
#' @param check_if_exists Whether to check that the URL works (HTTP 200).
#'
#' @return a character vector of length one, containing a URL
#' @family Detailed workflow
#' @examples
#' \dontrun{
#' sp_get_dataset_url("finm", 2018, 6, FALSE)
#' sp_get_dataset_url("finm", 2029, 6, FALSE) # works but returns invalid URL
#' if(FALSE) sp_get_dataset_url("finm_wrong", 2018, 6, TRUE) # fails, invalid dataset ID
#' if(FALSE) sp_get_dataset_url("finm", 2022, 6, TRUE) # fails, invalid time period
#' }
#' @export
sp_get_dataset_url <- function(dataset_id, year, month = 12, check_if_exists = TRUE) {
  if(!(dataset_id %in% sp_datasets_i$id)) cli::cli_abort("Not a valid dataset ID")
  dataset_name <- sp_datasets_i[sp_datasets_i$id == dataset_id, "name"]
  dataset_dir <- sp_datasets_i[sp_datasets_i$id == dataset_id, "dir"]
  month <- formatC(month, width = 2, format = "d", flag = "0")
  # cli::cli_inform("Building URL for dataset {.value {dataset_id}}: {.value {dataset_name}}, {.value {year}-{month}}")
  x <- stringr::str_glue("{sp_base_url}/data/extrakty/csv/{dataset_dir}/{year}_{month}_Data_CSUIS_{toupper(dataset_id)}.zip")
  # print(x)
  if(check_if_exists) {
    check_online(x)
  }
  # doc_url <- stringr::str_glue("{sp_base_url}/data/struktura/{dataset_id}.xlsx")
  # ui_info("Get the dataset documentation at {ui_path(doc_url)}")
  return(x)
}


#' Get dataset documentation
#'
#' Downloads XLS file with dataset documentation, or opens link to this file in browser.
#'
#' @param dataset_id dataset ID. See `sp_datasets`.
#' @param dest_dir character. Directory in which downloaded files will be stored.
#' If left unset, will use the `statnipokladna.dest_dir` option if the option is set, and `tempdir()` otherwise. Will be created if it does not exist.
#' @param download Whether to download (the default) or open link in browser.
#'
#' @return (invisible) path to file if `download = TRUE`, URL otherwise
#' @family Utilities
#' @examples
#' \dontrun{
#' sp_get_dataset_doc("finm")
#' }
#' @export
sp_get_dataset_doc <- function(dataset_id, dest_dir = NULL, download = TRUE) {
  if(!(dataset_id %in% sp_datasets_i$id)) cli::cli_abort(c(x = "Not a valid dataset ID",
                                                           i = "see {.var sp_datasets}"))
  doc_url <- stringr::str_glue("{sp_base_url}/data/struktura/{dataset_id}.xlsx")
  check_online(doc_url)
  if(is.null(dest_dir)) dest_dir <- getOption("statnipokladna.dest_dir",
                                              default = tempdir())

  dir.create(dest_dir, showWarnings = F, recursive = T)

  if(!download) {
    utils::browseURL(doc_url)
    cli::cli_alert_info("Link to file opened in browser. ({.url {doc_url}})")
    invisible(doc_url)
  } else {
    file_path <- file.path(dest_dir, stringr::str_glue("{dataset_id}.xlsx"))
    cli::cli_alert_info("Getting dataset documentation from {.url {doc_url}}")
    utils::download.file(doc_url, file_path, headers = c('User-Agent' = usr))
    cli::cli_alert_info("File downloaded to {.path {file_path}}.")
    invisible(file_path)
  }
}


#' Retrieve dataset from statnipokladna
#'
#' Downloads ZIP archives for a given dataset. If `year` or `month` have length > 1, gets all combinations.
#'
#' Files are stored in a temp folder as determined by `tempdir()` or the `dest_dir` param or the `statnipokladna.dest_dir` option.
#' and further sorted into subdirectories by dataset, year and month. If saved to `tempdir()` (the default), downloaded files per session to avoid redownloads.
#'
#' How data for different time periods is exported differs by dataset.
#' This has significant implications for how you get to usable full-year numbers or time series in different tables.
#' See `vignette("statnipokladna")` for details on this.
#'
#' @param dataset_id A dataset ID. See `id` column in `sp_datasets` for a list of available datasets
#' @param year year, numeric vector of length <= 1 (can take multiple values), 2015-2019 for some datasets, 2010-2020 for others. Defaults to 2018.
#' (see Details for how to work with data across time periods.)
#' @param month month, numeric vector of length <= 1 (can take multiple values). Must be between 1 and 12. Defaults to 12.
#' (see Details for how to work with data across time periods.)
#' @param dest_dir character. Directory in which downloaded files will be stored.
#' If left unset, will use the `statnipokladna.dest_dir` option if the option is set, and `tempdir()` otherwise. Will be created if it does not exist.
#' @param redownload Redownload even if file has already been downloaded? Defaults to FALSE.
#'
#' @return character string with complete paths to downloaded ZIP archives.
#' @examples
#' \dontrun{
#' budget_2018 <- sp_get_dataset("finm", 2018)
#' budget_mid2018 <- sp_get_dataset("finm", 2018, 6)
#' }
#' @family Core workflow
#' @export
sp_get_dataset <- function(dataset_id, year, month = 12,
                           dest_dir = NULL, redownload = FALSE) {
  if(interactive() == FALSE & missing(month)) {
    if(missing(month)) {
      cli::cli_alert_warning("{.var month} not set. Using default of {.value {month}}.")
    }
  }

  if(is.null(dest_dir)) dest_dir <- getOption("statnipokladna.dest_dir",
                                              default = tempdir())

  if(!all(month %in% c(1:12))) stop("{.var month} must be an integer from 1 to 12.")
  thisyr <- lubridate::year(lubridate::today())
  if(!all(year %in% c(2010:lubridate::year(lubridate::today()))) | missing(year)) cli::cli_abort("{.var year} must be between 2010 and {thisyr}")
  month <- formatC(month, width = 2, format = "d", flag = "0")

  years_months <- expand.grid(y = year, m = month, stringsAsFactors = F)

  get_one_dataset <- function(dataset_id, year, month, dest_dir, redownload) {
    td <- file.path(dest_dir, dataset_id, year, month)
    dir.create(td, showWarnings = FALSE, recursive = TRUE)
    tf <- file.path(td, paste0(dataset_id, year, month, ".zip"))
    if(file.exists(tf) & !redownload) {
      cli::cli_alert_info(c(i = "Files already in {.path {td}}, not downloading"))
      cli::cli_li(c(i = "Set {.code redownload = TRUE} if needed."))
    } else {
      dataset_url <- sp_get_dataset_url(dataset_id = dataset_id, year = year, month = month)
      cli::cli_alert_success("Storing downloaded archive in {.path {td}}")
      if(dest_dir == tempdir()) cli::cli_li("Set {.var dest_dir} for more control over downloaded files.")
      utils::download.file(dataset_url, tf, headers = c('User-Agent' = usr))
    }
    return(tf)
  }

  file_list <- purrr::map2_chr(years_months$y, years_months$m,
                               ~get_one_dataset(dataset_id, .x, .y, dest_dir = dest_dir,
                                                redownload = redownload))
  invisible(file_list)
}

# Deprecated --------------------------------------------------------------


#' Deprecated: Retrieve and read dataset from statnipokladna
#'
#' Deprecated, use `sp_get_dataset()` instead.\cr\cr
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams sp_get_dataset
#'
#' @return character (link) if download = TRUE, nothing otherwise.
#' @keywords internal
#' @export
get_dataset <- function(dataset_id, year, month = 12,
                        dest_dir = NULL, redownload = FALSE) {
  lifecycle::deprecate_warn("0.5.2", "statnipokladna::get_dataset()", "sp_get_dataset()")
  sp_get_dataset(dataset_id = dataset_id, year = year, month = month,
                 dest_dir = dest_dir, redownload = redownload)
}


#' Deprecated: Get dataset documentation
#'
#' Deprecated, use `sp_get_dataset_doc()` instead.\cr\cr
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams sp_get_dataset_doc
#'
#' @keywords internal
#' @return a [tibble][tibble::tibble-package]
#' @export
get_dataset_doc <- function(dataset_id, dest_dir = ".", download = TRUE) {
  lifecycle::deprecate_warn("0.5.2", "statnipokladna::get_dataset_doc()", "sp_get_dataset_doc()")
  sp_get_dataset_doc(dataset_id = dataset_id, dest_dir = dest_dir, download = download)
}
