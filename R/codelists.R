
#' List of available codelists
#'
#' Contains IDs and names of all (most) available codelists that can be retrieved by sp_get_codelist.
#'
#' The `id` is to be used as the `codelist_id` parameter in `sp_get_codelist`.
#' See <https://monitor.statnipokladna.cz/datovy-katalog/ciselniky> for a more detailed
#' descriptions and a GUI for exploring the lists.
#'
#' @format A data frame with 27 rows and 2 variables:
#' \describe{
#'   \item{\code{id}}{character. ID, used as `codelist_id` argument in `sp_get_codelist`.}
#'   \item{\code{name}}{character. Short name, mostly corresponds to title used on statnipokladna.cz.}
#' }
#' @family Lists of available entities
sp_codelists <- tibble::tribble(~id, ~name,
                                "aktorg", "Aktivn\\u00ed organizace",
                                "cofog", "Klasifikace funkc\\u00ed vl\\u00e1dn\\u00edch instituc\\u00ed COFOG",
                                "druhrizeni", "Druh \\u0159\\u00edzen\\u00ed",
                                "druhuj", "Druh \\u00fa\\u010detn\\u00ed jednotky",
                                "finmisto", "Finan\\u010dn\\u00ed m\\u00edsto (kapitoly, OSS)",
                                "forma", "Forma \\u00fa\\u010detn\\u00ed jednotky",
                                "kapitola", "Kapitola rozpo\\u010detu",
                                "katobyv", "Kategorie po\\u010dtu obyvatel",
                                "nuts", "Klasifikace \\u00fazemn\\u00edch statistick\\u00fdch jednotek NUTS",
                                "paragraf", "Paragraf",
                                "paragraf_long", "Paragraf (\\u0161estim\\u00edstn\\u00fd k\\u00f3d)",
                                "poddruhuj", "Poddruh \\u00fa\\u010detn\\u00ed jednotky",
                                "polvyk", "Polo\\u017eka v\\u00fdkazu",
                                "polozka", "Rozpo\\u010dtov\\u00e1 polo\\u017eka",
                                "polvkk", "Z\\u00e1vazn\\u00e9 ukazatele st\\u00e1tn\\u00edho rozpo\\u010dtu (Do 2014)",
                                "psuk", "Z\\u00e1vazn\\u00e9 ukazatele st\\u00e1tn\\u00edho rozpo\\u010dtu (Od 2015)",
                                "pvs", "P\\u0159\\u00edjmov\\u00e1 a v\\u00fddajov\\u00e1 struktura: strukturn\\u00ed t\\u0159\\u00edd\\u011bn\\u00ed",
                                "uctosnova", "Sm\\u011brn\\u00e1 \\u00fa\\u010dtov\\u00e1 osnova (polo\\u017eky \\u00fa\\u010detn\\u00edch v\\u00fdkaz\\u016f)",
                                "typfinmista", "Typ finan\\u010dn\\u00edho m\\u00edst",
                                "typorg", "Typ OSS",
                                "rozprog", "Rozpo\\u010dtov\\u00fd program",
                                "ucel", "\u00da\\u010del",
                                "ucjed", "\\u00da\\u010detn\\u00ed jednotka",
                                "ucelznak", "\\u00da\\u010delov\\u00fd znak",
                                "vykaz", "\\u010c\\u00edseln\\u00edk v\\u00fdkaz\\u016f a tabulek",
                                "zazjedn", "Z\\u00e1znamov\\u00e1 jednotka (konsolida\\u010dn\\u00ed t\\u0159\\u00edd\\u011bn\\u00ed)",
                                "zdroj", "Zdroj",
                                "nastroj", "N\\u00e1stroj",
                                "nastrojanal", "N\\u00e1stroj - analytika",
                                "zdrojfin", "Zdroj financov\\u00e1n\\u00ed organizac\\u00ed",
                                "zpodm", "Zp\\u016fsob odm\\u011b\\u0148ov\\u00e1n\\u00ed",
                                "zuj", "Z\\u00e1kladn\\u00ed \\u00fazemn\\u00ed jednotka") %>%
  dplyr::mutate(name = stringi::stri_unescape_unicode(.data$name)) %>%
  dplyr::arrange(.data$id)
# stringi::stri_escape_unicode("xxx")
# usethis::use_data(sp_codelists, overwrite = TRUE)


# https://stackoverflow.com/questions/62459736/how-do-i-use-tidyselect-where-in-a-custom-package
# https://github.com/r-lib/tidyselect/issues/201
# used below
utils::globalVariables("where")

#' Load codelist into a tibble from XML file
#'
#' This is normally called inside `sp_get_codelist()` but can be used separately if
#' finer-grained control of intermediate outputs is needed, e.g. in a `{targets}` workflow.
#'
#' @param path Path to a file as returned by `sp_get_codelist_file()`
#' @param n Number of rows to return. Default (NULL) means all. Useful for quickly inspecting a codelist.
#'
#' @return a [tibble][tibble::tibble-package]
#' @family Detailed workflow
#'
#' @examples
#' \dontrun{
#' cf <- sp_get_codelist_file("druhuj")
#' sp_load_codelist(cf)
#' }
#' @export
sp_load_codelist <- function(path, n = NULL) {
  stopifnot(file.exists(path))
  if(grepl("ucjed", path)) cli::cli_alert_info("Large codelist: this will take a while...")
  xml_all <- xml2::read_xml(path)
  xml_children_all <- xml_all %>% xml2::xml_children()
  xml_children <- if(is.null(n)) xml_children_all else xml_children_all[1:n]
  nms <- xml2::xml_child(xml_all) %>% xml2::xml_children() %>% xml2::xml_name()

  process_codelist <- function(x) {x %>% xml2::xml_children() %>%
      xml2::xml_text() %>%
      # as.character() %>%
      t() %>%
      # purrr::set_names(nms) %>%
      as.data.frame() %>%
      tibble::as_tibble(.name_repair = "minimal")}

  xvals_raw <- purrr::map_df(xml_children, process_codelist)

  # print(xvals_raw)

  xvals <- xvals_raw %>%
    purrr::set_names(nms) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_date")),
                     ~lubridate::as_date(lubridate::parse_date_time(., orders = c("Ymd", "dmY")))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("kon_")), as.logical) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("^vtab$")),
                     ~stringr::str_pad(., 6, "left", "0")) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("^vykaz$")),
                     ~stringr::str_pad(., 3, "left", "0")) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("^polvyk_order$")), as.numeric) %>%
    dplyr::mutate(dplyr::across(where(is.character), dplyr::na_if, ""))

  return(xvals)
}


#' Download a codelist XML file
#'
#' This is normally called inside `sp_get_codelist()` but can be used separately if
#' finer-grained control of intermediate outputs is needed, e.g. in a `{targets}` workflow.
#'
#' @param codelist_id A codelist ID. See `id` column in `sp_codelists` for a list of available codelists.
#' @param url DESCRIPTION. Either this or `codelist_id` must be set. If both are set, `url` wins.
#' @param dest_dir character. Directory in which downloaded files will be stored.
#' If left unset, will use the `statnipokladna.dest_dir` option if the option is set, and `tempdir()` otherwise. Will be created if it does not exist.
#' @param redownload Redownload even if file has already been downloaded? Defaults to FALSE.

#'
#' @return path to XML file; character vector of length one.
#' @family Detailed workflow
#' @examples
#' \dontrun{
#' sp_get_codelist_file("druhuj")
#' codelist_url <- sp_get_codelist_url("druhuj")
#' sp_get_codelist_file(url = codelist_url)
#' }
#' @export
sp_get_codelist_file <- function(codelist_id = NULL, url = NULL, dest_dir = NULL, redownload = FALSE) {
  if(is.null(dest_dir)) dest_dir <- getOption("statnipokladna.dest_dir",
                                              default = tempdir())

  td <- dest_dir
  dir.create(td, showWarnings = FALSE, recursive = TRUE)

  filename <- ifelse(is.null(url),
                     paste0(codelist_id, ".xml"),
                     stringr::str_extract(url, "[a-zA-Z]*\\.xml$"))

  tf <- file.path(td, filename)
  if(file.exists(tf) & !redownload) {
    cli::cli_alert_info(c(i = "Codelist file already in {.path {td}}, not downloading",
                          i = "Set {.code redownload = TRUE} if needed."))
  } else {
    if(is.null(url)) url <- sp_get_codelist_url(codelist_id)
    cli::cli_alert_info("Storing codelist in {.path {td}}")
    if(dest_dir == tempdir()) cli::cli_alert_info("Set {.var dest_dir} for more control over downloaded files.")
    utils::download.file(url, tf, headers = c('User-Agent' = usr))
  }
  return(tf)
}


#' Get codelist
#'
#' Downloads and processes codelist identified by `codelist_id`. See `sp_codelists` for a list of
#' of available codelists with their IDs and names.
#'
#' You can usually tell which codelist you need from the name of the column whose code you
#' are looking to expand, e.g. the codes in column paragraf can be expanded by codelist paragraf.
#'
#' The processing ensures that the resulting codelist can be correctly joined to
#' the data, automatically using `sp_add_codelist()` or manually.
#' The entire codelist is downloaded and not filtered for any particular date.
#'
#' Codelist XML files are stored in a temporary directory as determined by `tempdir()`
#' and persist per session to avoid redownloads.
#'
#'
#' @param codelist_id A codelist ID. See `id` column in `sp_codelists` for a list of available codelists.
#' @param n Number of rows to return. Default (NULL) means all. Useful for quickly inspecting a codelist.
#' @param dest_dir character. Directory in which downloaded files will be stored.
#' If left unset, will use the `statnipokladna.dest_dir` option if the option is set, and `tempdir()` otherwise. Will be created if it does not exist.
#' @param redownload Redownload even if file has already been downloaded? Defaults to FALSE.
#'
#' @return a [tibble][tibble::tibble-package]
#' @examples
#' \dontrun{
#' sp_get_codelist("paragraf")
#' }
#' @export
#' @family Core workflow

sp_get_codelist <- function(codelist_id, n = NULL, dest_dir = NULL, redownload = FALSE) {
  tf <- sp_get_codelist_file(codelist_id, dest_dir = dest_dir, redownload = redownload)
  cl_parsed <- sp_load_codelist(tf, n)
  return(cl_parsed)

}

#' Get/open URL of codelist viewer
#'
#' Returns a URL for the online codelist browser in monitor.statnipokladna.cz and opens it in browser if open = TRUE.
#'
#' @param codelist_id A codelist ID. See `id` column in `sp_codelists` for a list of available codelists.
#' @param open Whether to open URL in browser. Defaults to TRUE.
#'
#' @return a URL, character vector of length one.
#' @family Utilities
#' @keywords internal
sp_get_codelist_viewer <- function(codelist_id, open = TRUE) {
  if(!is.character(codelist_id) | length(codelist_id) > 1) cli::cli_abort("Codelist ID must be a character vector of length one.")
  if(!(codelist_id %in% sp_codelists$id)) cli::cli_abort("Not a valid codelist ID")
  codelist_name <- sp_codelists[sp_codelists$id == codelist_id, "name"]
  cli::cli_alert_info("Building URL for codelist {.value {codelist_id}} - {.value {codelist_name}}")
  x <- stringr::str_glue("{sp_base_url}/datovy-katalog/ciselniky/prohlizec/{codelist_id}")
  if(open) utils::browseURL(x)
  return(x)
}


#' Add codelist data to downloaded data
#'
#' Joins a provided codelist, or downloads and processes one if necessary, and adds it to the data.
#'
#' The `data` argument should be a data frame produced by `sp_get_table()` If this is true, the `period_column` argument is not needed.
#' The `codelist` argument, if a data frame, should be a data frame produced by
#' `sp_get_codelist()`. Specifically, it assumes it contains the following columns:
#'
#' - start_date, a date
#' - end_date, a date
#' - column with the code, character usually named the same as the codelist
#'
#' #' You can usually tell which codelist you need from the name of the column whose code you
#' are looking to expand, e.g. the codes in column paragraf can be expanded by codelist paragraf.
#'
#' The function filters the codelist to obtain a set of entries relevant to the time period of `data`.
#' If `data` contains tables for multiple periods, this is handled appropriately.
#' Codelist-originating columns in the resulting data frame are renamed so they do not interfere with
#' joining additional codelists, perhaps in a single pipe call.
#'
#' Note that some codelists are "secondary" and can only be joined onto other codelists.
#' If a codelist does not join using `sp_add_codelis()`, store the output of `sp_get_codelist()` and join
#' it manually using `dplyr`.
#'
#' @param data a data frame returned by `sp_get_table()`.
#' @param codelist The codelist to add. Either a character vector of length one (see `sp_tables` for possible values), or a data frame returned by `sp_get_codelist()`.
#' @param by character. Columns by which to join the codelist. Same form as for `dplyr::left_join()``.`.
#' @param dest_dir character. Directory in which downloaded files will be stored.
#' If left unset, will use the `statnipokladna.dest_dir` option if the option is set, and `tempdir()` otherwise. Will be created if it does not exist.
#' @param period_column Unquoted column name of column identifying the data period in `data`. Leave to default if you have not changed the `data` object returned by `sp_get_table()`.
#' @param redownload Redownload even if file has already been downloaded? Defaults to FALSE.
#' @return A [tibble][tibble::tibble-package] of same length as `data`, with added columns from `codelist`. See Details.
#' @family Core workflow
#' @export
#' @examples
#' \dontrun{
#' sp_get_table("budget-central", 2017) %>%
#'   sp_add_codelist("polozka") %>%
#'   sp_add_codelist("paragraf")
#'
#' pol <- sp_get_codelist("paragraf")
#' par <- sp_get_codelist("polozka")
#'
#' sp_get_table("budget-central", 2017) %>%
#'   sp_add_codelist(pol) %>%
#'   sp_add_codelist(par)
#' }
sp_add_codelist <- function(data, codelist = NULL, period_column = .data$vykaz_date,
                            by = NULL,
                            redownload = FALSE,
                            dest_dir = NULL) {
  if(is.null(codelist)) stop("Please supply a codelist")
  # print(rlang::as_label({{period_column}}))
  stopifnot("data.frame" %in% class(data),
            "data.frame" %in% class(codelist) | is.character(codelist))
  if(is.character(codelist)) stopifnot(length(codelist) == 1)



  if(is.character(codelist)) {
    cl_data <- sp_get_codelist(codelist, redownload = redownload,
                               dest_dir = dest_dir)
    codelist_name <- codelist
  } else {
    cl_data <- codelist
    codelist_name <- deparse(substitute(codelist))
  }
  common_columns <- names(data)[names(data) %in% names(cl_data)]
  overlap <- length(common_columns)
  if (overlap > 1 & is.null(by)) {
    cli::cli_alert_info(c("Joining on {.value {overlap}} columns: {.value {stringr::str_c(common_columns, collapse = ', ')}}.",
                          "This may indicate a problem with the data.",
                          "Set {.var by} if needed."))
  } else if(overlap == 0) {cli::cli_abort(c(x = "No columns to join by.",
                                            "{cli::symbol$pointer} Are you sure you are merging the right codelist onto the right data?",
                                            i = "Set {.value by} if needed."))}

  slepit <- function(.x, .y) {
    # print(.x)
    # print(.y)
    nrows_start <- nrow(.x)
    this_period <- dplyr::pull(.y, {{period_column}})
    # print(this_period)
    codelist_filtered <- cl_data %>%
      dplyr::filter(.data$end_date >= this_period & .data$start_date <= this_period) %>%
      dplyr::rename_at(dplyr::vars(dplyr::ends_with("_date")), ~paste0(codelist_name, "_", .)) %>%
      dplyr::rename_at(dplyr::vars(dplyr::matches("^nazev$")), ~paste0(codelist_name, "_", .))

    # print(codelist_filtered)
    slp <- suppressMessages(dplyr::left_join(.x, codelist_filtered, by = by))
    if(nrow(slp) != nrows_start) {
      cli::cli_abort(c(x = "Something went wrong with matching the codelist to the data for period {.value {this_period}}.",
                       i = "Please inspect the dates on the codelist to make sure there are no duplicate items valid for one given date. You may want to filter/edit the codelist manually and pass it to the {.fn add_codelist} function as an object."))
    }
    return(slp)
  }

  if("start_date" %in% names(cl_data) & "end_date" %in% names(cl_data)) {
    slepeno <- data %>%
      dplyr::ungroup() %>%
      dplyr::group_by({{period_column}}) %>%
      dplyr::group_map(slepit, .keep = TRUE) %>%
      dplyr::bind_rows()
  } else {
    slepeno <- suppressMessages(data %>%
                                  dplyr::left_join(cl_data, by = by))
  }
  return(slepeno)
}



#' Get URL of a given codelist
#'
#' This is normally called inside `sp_get_codelist()` but can be used separately if
#' finer-grained control of intermediate outputs is needed, e.g. in a `{targets}` workflow.
#'
#' @param codelist_id DESCRIPTION.
#' @param check_if_exists Whether to check that the URL works (HTTP 200).
#'
#' @family Detailed workflow
#'
#' @return character vector of length one containing URL
#' @examples
#' \dontrun{
#' sp_get_codelist_url("ucjed", FALSE)
#' if(FALSE) sp_get_codelist_url("ucjed_wrong", TRUE) # fails, invalid codelist
#' }
#' @export
sp_get_codelist_url <- function(codelist_id, check_if_exists = TRUE) {
  if(!is.character(codelist_id) | length(codelist_id) > 1) cli::cli_abort("Codelist ID must be a character vector of length one.")
  if(!(codelist_id %in% sp_codelists$id)) cli::cli_abort("Not a valid codelist ID")
  codelist_name <- sp_codelists[sp_codelists$id == codelist_id, "name"]
  # cli::cli_alert_info("Building URL for codelist {.value {codelist_id}} - {.value {codelist_name}}")
  x <- paste(sp_base_url, "data/xml", paste0(codelist_id, ".xml"), sep = "/")
  if(check_if_exists) {
    check_online(x)
  }
  return(x)
}



# Deprecated --------------------------------------------------------------

#' Deprecated: Add codelist data to downloaded data
#'
#' Deprecated, use `sp_add_codelist()` instead.\cr\cr
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams sp_add_codelist
#'
#' @return A [tibble][tibble::tibble-package] of same length as `data`, with added columns from `codelist`. See Details.
#' @family Core workflow
#' @export
add_codelist <- function(data, codelist = NULL, period_column = .data$vykaz_date,
                         redownload = FALSE,
                         dest_dir = NULL) {
  lifecycle::deprecate_warn("0.5.2", "statnipokladna::add_codelist()", "sp_add_codelist()")
  sp_add_codelist(data = data, codelist = codelist, period_column = period_column,
                  redownload = redownload, dest_dir = dest_dir)
}


#' Deprecated: Get codelist
#'
#' Deprecated: use `sp_get_codelist()`\cr\cr
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams sp_get_codelist
#'
#' @return A [tibble][tibble::tibble-package]
#' @export
#' @family Core workflow

get_codelist <- function(codelist_id, n = NULL, dest_dir = NULL, redownload = FALSE) {
  lifecycle::deprecate_warn("0.5.2", "statnipokladna::get_codelist()", "sp_get_codelist()")
  sp_get_codelist(codelist_id = codelist_id, n = n, dest_dir = dest_dir, redownload = redownload)
}
