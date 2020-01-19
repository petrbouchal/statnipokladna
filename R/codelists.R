
#' List of available codelists
#'
#' Contains IDs and names of all (most) available codelists that can be retrieved by get_codelist.
#'
#' The `id` is to be used as the `codelist_id` parameter in `get_codelist`.
#' See <https://monitor.statnipokladna.cz/2019/zdrojova-data/ciselniky> for a more detailed
#' descriptions and a GUI for exploring the lists.
#'
#' @format A data frame with 27 rows and 2 variables:
#' \describe{
#'   \item{\code{id}}{character. ID, used as `codelist_id` argument in `get_codelist`.}
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
                                "uctosnova", "Sm\\u011brn\\u00e1 \\u00fa\\u010dtov\\u00e1 osnova (polo\\u017eky \\u00fa\\u010detn\\u00edch v\\u00fdkaz\\u016f)",
                                "typfinmista", "Typ finan\\u010dn\\u00edho m\\u00edst",
                                "typorg", "Typ OSS",
                                "ucel", "\u00da\\u010del",
                                "ucjed", "\\u00da\\u010detn\\u00ed jednotka",
                                "ucelznak", "\\u00da\\u010delov\\u00fd znak",
                                "vykaz", "\\u010c\\u00edseln\\u00edk v\\u00fdkaz\\u016f a tabulek",
                                "zazjedn", "Z\\u00e1znamov\\u00e1 jednotka (konsolida\\u010dn\\u00ed t\\u0159\\u00edd\\u011bn\\u00ed)",
                                "zdroj", "Zdroj",
                                "zdrojfin", "Zdroj financov\\u00e1n\\u00ed organizac\\u00ed",
                                "zpodm", "Zp\\u016fsob odm\\u011b\\u0148ov\\u00e1n\\u00ed",
                                "zuj", "Z\\u00e1kladn\\u00ed \\u00fazemn\\u00ed jednotka") %>%
  dplyr::mutate(name = stringi::stri_unescape_unicode(name)) %>%
  dplyr::arrange(id)
# stringi::stri_escape_unicode("xxx")
# usethis::use_data(sp_codelists, overwrite = T)

#' Get codelist
#'
#' Downloads and processes codelist identified by `codelist_id`. See `sp_codelists` for a list of
#' of available codelists with their IDs and names.
#'
#' You can usually tell which codelist you need from the name of the column whose code you
#' are looking to expand, e.g. the codes in column paragraf can be expanded by codelist paragraf.
#'
#' The processing ensures that the resulting codelist can be correcly joined to
#' the data, autamatically using `add_codelist()` or manually.
#' The entire codelist is downlaoded and not filtered for any particular date.
#'
#' Codelists XML files are stored in a temporary directory as determined by `tempdir()`
#' and persist per session to avoid redownloads.
#'
#' @param codelist_id A codelist ID. See `id` column in `sp_codelists` for a list of available codelists.
#' @param n Number of rows to return. Default (NULL) means all. Useful for quickly inspecting a codelist.
#' @param force_redownload Redownload even if file has already been downloaded? Defaults to FALSE.
#'
#' @return A tibble
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
#' @family Core workflow

get_codelist <- function(codelist_id, n = NULL, force_redownload = F) {
  td <- paste0(tempdir(), "/statnipokladna/")
  tf <- paste0(td, codelist_id, ".xml")
  if(file.exists(tf) & !force_redownload) {
    message(stringr::str_glue("Codelist file already in {td}, not downloading. Set `force_redownload` to TRUE if needed."))
  } else {
    url <- get_codelist_url(codelist_id)
    message(stringr::str_glue("Storing codelist in {td}"))
    utils::download.file(url, tf, headers = c('User-Agent' = usr))
  }
  xml_all <- xml2::read_xml(tf)
  message("Processing codelist data")
  if(codelist_id %in% c("ucjed")) message("Large codelist: this will take a while...")
  xml_children_all <- xml_all %>% xml2::xml_children()
  xml_children <- if(is.null(n)) xml_children_all else xml_children_all[1:n]
  nms <- xml2::xml_child(xml_all) %>% xml2::xml_children() %>% xml2::xml_name()
  xvals <- purrr::map_df(xml_children, function(x) {x %>% xml2::xml_children() %>%
      xml2::xml_text() %>%
      # as.character() %>%
      t() %>% tibble::as_tibble()}) %>%
    purrr::set_names(nms) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_date")), lubridate::dmy) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("kon_")), as.logical) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("^vtab$")),
                     ~stringr::str_pad(., 6, "left", "0")) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("^vykaz$")),
                     ~stringr::str_pad(., 3, "left", "0")) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches("^polvyk_order$")), as.numeric)

  return(xvals)
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
#' @examples
#' # ADD_EXAMPLES_HERE
get_codelist_viewer <- function(codelist_id, open = TRUE) {
  if(!(codelist_id %in% sp_codelists$id)) stop("Not a valid codelist ID")
  codelist_name <- sp_codelists[sp_codelists$id == codelist_id, "name"]
  message(stringr::str_glue("Building URL for codelist {codelist_id} - {codelist_name}"))
  x <- stringr::str_glue("{sp_base_url}/2019/zdrojova-data/prohlizec-ciselniku/{codelist_id}")
  if(open) utils::browseURL(x)
  return(x)
}

switch_minus <- function(string) {
  swtch <- function(strg) {
    r0 <- strg %>%
      stringr::str_remove("-$")
    return(stringr::str_c("-", r0))
  }
  rslt <- dplyr::if_else(grepl("-$", string),
                         swtch(string), string)
  return(rslt)
}


#' Add codelist data to downloaded data
#'
#' Joins a provided codelist, or downloads and processes one if necessary, and adds it to the data.
#'
#' The `data` argument should be a data frame produced by `get_table()` If this is true, the `period_column` argument is not needed.
#' The `codelist` argument, if a data frame, should be a data frame produced by
#' `get_codelist()`. Specifically, it assumes it contains the following columns:
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
#' @param data a data frame returned by `get_table()`.
#' @param codelist The codelist to add. Either a character vector of length one (see `sp_tables` for possible values), or a data frame returned by `get_codelist()`.
#' @param period_column Unquoted column name of column identifying the data period in `data`. Leave to default if you have not changed the `data` object returned by `get_table()`.
#' @param force_redownload Redownload even if file has already been downloaded? Defaults to FALSE.
#' @return A data frame of same length as `data`, with added columns from `codelist`. See Details.
#' @family Core workflow
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
add_codelist <- function(data, codelist = NULL, period_column = period_vykaz,
                         force_redownload = F) {
  if(is.null(codelist)) stop("Please supply a codelist")
  # print(rlang::as_label({{period_column}}))
  stopifnot("data.frame" %in% class(data),
            "data.frame" %in% class(codelist) | is.character(codelist))
  if(is.character(codelist)) stopifnot(length(codelist) == 1)
  if(is.character(codelist)) {
    cl_data <- get_codelist(codelist, force_redownload = force_redownload)
    codelist_name <- codelist
  } else {
    cl_data <- codelist
    codelist_name <- deparse(substitute(codelist))
  }
  slepit <- function(.x, .y) {
    # print(.x)
    # print(.y)
    nrows_start <- nrow(.x)
    this_period <- dplyr::pull(.y, {{period_column}})
    # print(this_period)
    codelist_filtered <- cl_data %>%
      dplyr::filter(end_date >= this_period & start_date <= this_period) %>%
      dplyr::rename_at(dplyr::vars(dplyr::ends_with("_date")), ~paste0(codelist_name, "_", .)) %>%
      dplyr::rename_at(dplyr::vars(dplyr::ends_with("nazev")), ~paste0(codelist_name, "_", .))

    # print(codelist_filtered)
    slp <- dplyr::left_join(.x, codelist_filtered)
    if(nrow(slp) != nrows_start) {
      errmsg <- stringr::str_glue(
        "Something went wrong with matching the codelist to the data for period {this_period}.
        Please inspect the dates on the codelist to make sure there are no duplicate
        items valid for one given date.\nYou may want to filter/edit the codelist manually
        and pass it to the add_codelist function as an object."
      )
      stop(errmsg)
      }
    return(slp)
  }

  slepeno <- data %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{period_column}}) %>%
    dplyr::group_map(slepit, keep = T) %>%
    dplyr::bind_rows()
  return(slepeno)
}

get_codelist_url <- function(codelist_id, check_if_exists = T) {
  if(!(codelist_id %in% sp_codelists$id)) stop("Not a valid codelist ID")
  codelist_name <- sp_codelists[sp_codelists$id == codelist_id, "name"]
  message(stringr::str_glue("Building URL for codelist {codelist_id} - {codelist_name}"))
  x <- stringr::str_glue("{sp_base_url}/data/{codelist_id}.xml")
  if(check_if_exists) {
    iserror <- httr::http_error(x, httr::config(followlocation = 0L), USE.NAMES = FALSE)
    if(iserror) stop("Codelist XML for a codelist with this ID does not exist")
  }
  return(x)
}
