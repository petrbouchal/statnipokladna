
#' List of available codelists
#'
#' Contains IDs and names of all available codelists that can be retrieved by get_codelist.
#' See <https://monitor.statnipokladna.cz/2019/zdrojova-data/ciselniky> for a more detailed
#' descriptions and a GUI for exploring the lists.
#'
#' The `id` is to be used as the `codelist_id` parameter in `get_codelist`.
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
                                "polvkk", "Z\\u00e1vazn\\u00e9 ukazatele st\\u00e1tn\\u00edho rozpo\\u010dtu (Do 2014)",
                                "psuk", "Z\\u00e1vazn\\u00e9 ukazatele st\\u00e1tn\\u00edho rozpo\\u010dtu (Od 2015)",
                                "smeruc", "Sm\\u011brn\\u00e1 \\u00fa\\u010dtov\\u00e1 osnova (polo\\u017eky \\u00fa\\u010detn\\u00edch v\\u00fdkaz\\u016f)",
                                "typfinmista", "Typ finan\\u010dn\\u00edho m\\u00edst",
                                "typorg", "Typ OSS",
                                "ucel", "\u00da\\u010del",
                                "ucjed", "\\u00da\\u010detn\\u00ed jednotka",
                                "ucznak", "\\u00da\\u010delov\\u00fd znak",
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
#' @param codelist_id A codelist ID. See `id` column in `sp_codelists` for a list of available codelists.
#'
#' @return A tibble
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
#' @family Core workflow

get_codelist <- function(codelist_id) {
  url <- get_codelist_url(codelist_id)
  message("Downloading codelist data")
  vy <- httr::with_config(config = httr::config(useragent = usr),
                          xml2::read_xml(url)) %>% xml2::xml_children()
  message("Processing codelist data")
  if(codelist_id %in% c("ucjed")) message("Large codelist: this will take a while...")
  xnames <- purrr::map(vy, function(x) x %>% xml2::xml_children() %>% xml2::xml_name())
  if(length(unique(xnames)) == 1)
  {nms <- xnames[[1]]}
  else
    stop("No single set of names")

  xvals <- purrr::map_df(vy, function(x) {x %>% xml2::xml_children() %>%
      xml2::xml_text() %>%
      as.character() %>%
      t() %>% tibble::as_tibble()}) %>%
    purrr::set_names(nms) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_date")), lubridate::dmy)
  return(xvals)
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

join_codelist <- function(data, codelist_id = NULL, codelist_df = NULL, period_column) {
  if(is.null(codelist_id) & is.null(codelist_df)) {
    stop("At least one of codelist_id and codelist_df must be specified.")
  }
  if(is.null(codelist_df) & is.character(codelist_id)) {
    ciselnik <- get_codelist(codelist_id)
  } else if (is.data.frame(codelist_df) & !is.null(codelist_id)) {
    stop("Both `codelist_df` and `codelist_id` supplied. Please supply only one")
  } else if (is.data.frame(codelist_df) & is.null(codelist_id)) {
    if(!(c("end_date", "start_date") %in% names(codelist_df)) & length(codelist_df) > 2) {
      stop("Wrong column specification of `codelist_df`. It must contain columns named
           `start_date`, `end_date` and at least one more column")
    }
    ciselnik <- codelist_df
  }

  stopifnot(period_column %in% names(data),
            "data.frame" %in% class(data),
            "data.frame" %in% class(ciselnik))
  perd = unique(data[{{ period_column }}])
  stopifnot(length(unique(data [{{ period_column }}])) == 1)
  ciselnik_filtered <- ciselnik %>%
    dplyr::filter({{ end_date }} >= perd)

  slepeno <- dplyr::left_join(data, ciselnik_filtered)

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
