
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
sp_datasets <- sp_datasets_i %>% dplyr::select(.data$id, .data$name) %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
# usethis::use_data(sp_datasets, overwrite = TRUE)

get_dataset_url <- function(dataset_id, year = 2018, month = 12, check_if_exists = TRUE) {
  if(!(dataset_id %in% sp_datasets_i$id)) usethis::ui_stop("Not a valid dataset ID")
  dataset_name <- sp_datasets_i[sp_datasets_i$id == dataset_id, "name"]
  dataset_dir <- sp_datasets_i[sp_datasets_i$id == dataset_id, "dir"]
  usethis::ui_info("Building URL for dataset {usethis::ui_value(dataset_id)}: {usethis::ui_value(dataset_name)}, {usethis::ui_value(stringr::str_c(year,'-',month))}")
  x <- stringr::str_glue("{sp_base_url}/data/extrakty/csv/{dataset_dir}/{year}_{month}_Data_CSUIS_{toupper(dataset_id)}.zip")
  # print(x)
  if(!curl::has_internet()) usethis::ui_stop(c("No internet connection. Cannot continue. Retry when connected.",
                                               "If you need offline access to the data across R sessions, set the {ui_field('dest_dir')} parameter."))
  if(check_if_exists) {
    iserror <- httr::http_error(x, httr::user_agent(usr))
    if(iserror) usethis::ui_stop("File does not exist for this dataset and period combination.")
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
#' @param dest_dir character. Directory in which downloaded files will be stored.
#' If left unset, will use the `statnipokladna.dest_dir` option if the option is set, and `tempdir()` otherwise. Will be created if it does not exist.
#' @param download Whether to download (the default) or open link in browser.
#'
#' @return (invisible) path to file if `download = TRUE`, URL otherwise
#' @family Utilities
#' @examples
#' \donttest{
#' sp_get_dataset_doc("finm")
#' }
#' @export
sp_get_dataset_doc <- function(dataset_id, dest_dir = NULL, download = TRUE) {
  if(!curl::has_internet()) usethis::ui_stop(c("No internet connection. Cannot continue. Retry when connected."))
  if(!(dataset_id %in% sp_datasets_i$id)) usethis::ui_stop("Not a valid dataset ID")
  doc_url <- stringr::str_glue("{sp_base_url}/data/struktura/{dataset_id}.xlsx")

  if(is.null(dest_dir)) dest_dir <- getOption("statnipokladna.dest_dir",
                                              default = tempdir())

  if(!download) {
    utils::browseURL(doc_url)
    usethis::ui_info("Link to file opened in browser. ({usethis::ui_path(doc_url)})")
    invisible(doc_url)
  } else {
    file_path <- file.path(dest_dir, stringr::str_glue("{dataset_id}.xlsx"))
    usethis::ui_info("Getting dataset documentation from {doc_url}")
    utils::download.file(doc_url, file_path, headers = c('User-Agent' = usr))
    usethis::ui_info("File downloaded to {usethis::ui_path(file_path)}.")
    invisible(file_path)
  }
}

#' Deprecated: Get dataset documentation
#'
#' Deprecated, use `sp_get_dataset_doc()` instead.\cr\cr
#' \lifecycle{deprecated}
#'
#' @inheritParams sp_get_dataset_doc
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
get_dataset_doc <- function(dataset_id, dest_dir = ".", download = TRUE) {
  lifecycle::deprecate_warn("0.5.2", "statnipokladna::get_dataset_doc()", "sp_get_dataset_doc()")
  sp_get_dataset_doc(dataset_id = dataset_id, dest_dir = dest_dir, download = download)
}


#' Retrieve dataset from statnipokladna
#'
#' Downloads and unzips files for a given dataset.
#'
#' Files are stored in a temp folder as determined by `tempdir()` and further sorted into
#' subdirectories by dataset, year and month. They persist per session to avoid redownloads.
#'
#' How data for different time periods is exported differs by dataset.
#' This has significant implications for how you get to usable full-year numbers or time series in different tables.
#' See `vignette("statnipokladna")` for details on this.
#'
#' @param dataset_id A dataset ID. See `id` column in `sp_datasets` for a list of available codelists.
#' @param year year, numeric, 2015-2018 for some datasets, 2010-2019 for others. Defaults to 2018.
#' (see Details for how to work with data across time periods.)
#' @param month month, numeric. Must be between 1 and 12. Defaults to 12.
#' (see Details for how to work with data across time periods.)
#' @param dest_dir character. Directory in which downloaded files will be stored.
#' If left unset, will use the `statnipokladna.dest_dir` option if the option is set, and `tempdir()` otherwise. Will be created if it does not exist.
#' @param redownload Redownload even if file has already been downloaded? Defaults to FALSE.
#'
#' @return character string with complete paths to downloaded files.
#' @examples
#' \donttest{
#' budget_latest <- sp_get_dataset("finm")
#' budget_2018 <- sp_get_dataset("finm", 2018)
#' budget_mid2018 <- sp_get_dataset("finm", 2018, 6)
#' }
#' @family Core workflow
#' @export
sp_get_dataset <- function(dataset_id, year = 2018, month = 12,
                           dest_dir = NULL, redownload = FALSE) {
  if(interactive() == FALSE & (missing(year) | missing(month))) {
    if(missing(year)) {
      usethis::ui_warn("{usethis::ui_field('year')} not set.
                     Using default of {usethis::ui_value(year)}.")

    } else if(missing(month)) {
      usethis::ui_warn("{usethis::ui_field('month')} not set.
                     Using default of {usethis::ui_value(month)}.")

    }

    usethis::ui_todo("Set period parameters explicitly for reproducibility as the defaults may change in the future
                     to provide access to the latest data by default.")
  }

  if(is.null(dest_dir)) dest_dir <- getOption("statnipokladna.dest_dir",
                                              default = tempdir())

  if(!(month %in% c(1:12))) stop("`Month` must be an integer from 1 to 12.")
  if(!(year %in% c(2010:lubridate::year(lubridate::today())))) stop("`Year` must be between 2010 and now.")
  month <- formatC(month, width = 2, format = "d", flag = "0")
  td <- paste(dest_dir, dataset_id, year, month, sep = "/")
  dir.create(td, showWarnings = FALSE, recursive = TRUE)
  tf <- paste0(td, "/", dataset_id, year, month, ".zip")
  if(file.exists(tf) & !redownload) {
    usethis::ui_info("Files already in {td}, not downloading. Set {usethis::ui_code('redownload = TRUE')} if needed.")
  } else {
    dataset_url <- get_dataset_url(dataset_id = dataset_id, year = year, month = month)
    usethis::ui_done("Storing downloaded archive in and extracting to {usethis::ui_path(td)}")
    if(dest_dir == tempdir()) usethis::ui_info("Set {usethis::ui_field('dest_dir')} for more control over downloaded files.")
    utils::download.file(dataset_url, tf, headers = c('User-Agent' = usr))
    utils::unzip(tf, exdir = td)
  }
  file_list <- paste0(td, "/", list.files(td, pattern = "(csv|CSV)$"))
  invisible(file_list)
}

#' Deprecated: Retrieve and read dataset from statnipokladna
#'
#' Deprecated, use `sp_get_dataset()` instead.\cr\cr
#' \lifecycle{deprecated}
#'
#' @inheritParams sp_get_dataset
#'
#' @return character (link) if download = TRUE, nothing otherwise.
#' @family Utilities
#' @export
get_dataset <- function(dataset_id, year = 2019, month = 12,
                        dest_dir = NULL, redownload = FALSE) {
  lifecycle::deprecate_warn("0.5.2", "statnipokladna::get_dataset()", "sp_get_dataset()")
  sp_get_dataset(dataset_id = dataset_id, year = year, month = month,
                 dest_dir = dest_dir, redownload = redownload)
}
