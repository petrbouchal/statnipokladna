sp_tables_i <- tibble::tribble(~table_num, ~report_num, ~id,   ~table_code,   ~dataset_id, ~file_regex, ~czech_name, ~note,
                               100,         51,         "budget-local", "finm_budget", "finm",      "FINM201",    "Pln\\u011bn\\u00ed rozpo\\u010dtu m\\u00edstn\\u011b \\u0159\\u00edzen\\u00fdch organizac\\u00ed" , NA,
                               100,         51,         "budget-local-purpose-grants", "finm_ucel",   "finm",      "FINM207",    "\\u00da\\u010delov\\u00e9 financov\\u00e1n\\u00ed m\\u00edstn\\u011b \\u0159\\u00edzen\\u00fdch organizac\\u00ed", NA,
                               0,           0,          "budget-indicators",     "misris_zu",   "misris",    "((MIS-RIS-ZU)|(ZU-MIS-RIS))", "Z\\u00e1vazn\\u00e9 ukazatele st\\u00e1tn\\u00edho rozpo\\u010dtu", "only central orgs",
                               0,           0,          "profit-and-loss",     "vykzz",       "vykzz",     "VYKZZ1?",      "V\\u00fdkaz zisk\\u016f a ztr\\u00e1t" , NA,
                               0,           0,          "profit-and-loss-city-districts",     "vykzzmc",       "vykzz",     "VYKZZ_?MC",      "V\\u00fdkaz zisk\\u016f a ztr\\u00e1t - m\\u011bstsk\\u00e9 \\u010d\\u00e1sti", "only for 2018; in other years city districts are incorporated in balance sheet",
                               0,           0,          "changes-in-equity",  "pozvk", "pozvk", "POZVK", "P\\u0159ehled o zm\\u011bn\\u00e1ch vlastn\\u00edho kapit\\u00e1lu", NA,
                               0,           0,          "cash-flow",  "ppt", "ppt", "PPT", "P\\u0159ehled pen\\u011b\\u017en\\u00edch tok\\u016f", NA,
                               0,           0,          "balance-sheet",     "rozvaha1",    "rozv",      "ROZV[1]?",      "Rozvaha - prvn\\u00ed \\u010d\\u00e1st", NA,
                               0,           0,          "balance-sheet-2",     "rozvaha2",    "rozv",      "ROZV2",      "Rozvaha - druh\\u00e1 \\u010d\\u00e1st", NA,
                               0,           0,          "balance-sheet-city-districts",     "rozvaha1mc",  "rozv",      "ROZV[1_]MC",    "Rozvaha m\\u011bstsk\\u00fdch \\u010d\\u00e1st\\u00ed - prvn\\u00ed \\u010d\\u00e1st", "only for 2018; in other years city districts are incorporated in balance sheet",
                               0,           0,          "balance-sheet-city-districts-2",     "rozvaha2mc",  "rozv",      "ROZVMC2",    "Rozvaha m\\u011bstsk\\u00fdch \\u010d\\u00e1st\\u00ed - druh\\u00e1 \\u010d\\u00e1st", "only for 2018; in other years city districts are incorporated in balance sheet",
                               0,           0,          "budget-central-old",     "finu_budget",  "finu",      "FINU101",    "Pln\\u011bn\\u00ed rozpo\\u010dtu \\u00fast\\u0159edn\\u011b \\u0159\\u00edzen\\u00fdch organizac\\u00ed do 2014", "pre-2015 only",
                               0,           0,          "budget-central-old-purpose-grants",     "finu_ucel",  "finu",      "FINU107",   "\\u00da\\u010delov\\u00e9 financov\\u00e1n\\u00ed - poskytnut\\u00e9 prost\\u0159edky", "pre-2015 only",
                               0,           0,          "budget-central-old-subsidies",     "finu_dotace",  "finu",      "FINU108",    "Dota\\u010dn\\u00ed financov\\u00e1n\\u00ed - poskytnut\\u00e9 prost\\u0159edky", "pre-2015 only",
                               0,           0,          "budget-central",     "misris",      "misris",    "/MIS-RIS",    "Pln\\u011bn\\u00ed rozpo\\u010dtu \\u00fast\\u0159edn\\u011b \\u0159\\u00edzen\\u00fdch organizac\\u00ed od 2015", "post-2014 only",
                               0,           0,          "budget-statefunds",     "finsf_budget",      "finsf",    "FINSF01",    "Pln\\u011bn\\u00ed rozpo\\u010dtu st\\u00e1tn\\u00edch fond\\u016f (\\u010c\\u00e1st I a II: p\\u0159\\u00edjmy a v\\u00fddaje)", "post-2014 only",
                               0,           0,          "profit-and-loss-statefunds",     "finsf_vykzz",      "finsf",    "FINSF02",    "Pln\\u011bn\\u00ed rozpo\\u010dtu st\\u00e1tn\\u00edch fond\\u016f (\\u010c\\u00e1st III: v\\u00fdkaz zisk\\u016f a ztr\\u00e1t)", "post-2014 only",
                               0,           0,          "cash-flow-statefunds",     "finsf_cashflow",      "finsf",    "FINSF03",    "Pln\\u011bn\\u00ed rozpo\\u010dtu st\\u00e1tn\\u00edch fond\\u016f (\\u010c\\u00e1st IV: p\\u0159ehled pen\\u011b\\u017en\\u00edch tok\\u016f)", "post-2014 only",
                               0,           0,          "budget-statefunds-purposegrants",     "finsf_ucel",      "finsf",    "FINSF04",    "Pln\\u011bn\\u00ed rozpo\\u010dtu st\\u00e1tn\\u00edch fond\\u016f (\\u010c\\u00e1st IX: dota\\u010dn\\u00ed financov\\u00e1n\\u00ed - poskytnut\\u00e9 prost\\u0159edky)", "post-2014 only"
                               ) %>%
  dplyr::mutate_if(is.double, as.integer) %>%
  dplyr::arrange(.data$id)
# stringi::stri_escape_unicode("xxx")


#' List of available tables (PARTIAL)
#'
#' Contains IDs and names of all available tables that can be
#' retrieved by sp_get_table. Look inside the XLS documentation for each dataset at <https://monitor.statnipokladna.cz/datovy-katalog/transakcni-data>
#' to see more detailed descriptions. Note that tables do not correspond to the tabulka/`vtab` attribute of the tables, they represent files inside datasets.
#'
#' @format A data frame with 2 rows and 4 variables:
#' \describe{
#'   \item{\code{id}}{character Table id, used as `table_id` argument to `sp_get_table`.}
#'   \item{\code{dataset_id}}{integer Table number.}
#'   \item{\code{czech_name}}{character Czech name of the table.}
#'   \item{\code{note}}{character Note.}
#' }
#' @family Lists of available entities
"sp_tables" <- sp_tables_i %>% dplyr::select("id", "dataset_id", "czech_name", "note") %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
# usethis::use_data(sp_tables, overwrite = TRUE)


#' Get path to a CSV file containing a table.
#'
#' This is normally called inside `sp_get_table()` but can be used separately if
#' finer-grained control of intermediate outputs is needed, e.g. in a `{targets}` workflow.
#'
#' @param table_id Table ID; see `id` column in `sp_tables` for a list of available codelists.
#' @param dataset_path Path to downloaded dataset, as output by `sp_get_dataset()`
#' @param reunzip Whether to overwrite existing CSV files by unzipping the archive downlaoded by `sp_get_dataset()`. Defaults to FALSE.
#'
#' @return Character vector of length one - a path.
#' @family Detailed workflow
#' @examples
#' \dontrun{
#' ds <- sp_get_dataset("rozv", 2018, 12)
#' sp_get_table_file("balance-sheet", ds)
#' }
#' @export
sp_get_table_file <- function(table_id, dataset_path, reunzip = FALSE) {
  stopifnot(file.exists(dataset_path))
  dd <- dirname(dataset_path)
  dslist <- list.files(dd, pattern = "*.CSV|*.csv", full.names = T)
  if(length(dslist) == 0 | reunzip) {
    dslist <- utils::unzip(dataset_path, exdir = dd)
  }

  table_regex <- paste0(sp_tables_i$file_regex[sp_tables_i$id == table_id])

  table_file <- dslist[stringr::str_detect(dslist,
                                           paste0(table_regex, "(_[0-9]*)?\\.(csv|CSV)"))]
  if(length(table_file) != 1) {
    if (length(table_file) > 1) {
      cli::cli_abort(c(x = "More than one CSV files in the archive match.",
                       i = "You might want to report a bug at {.url https://github.com/petrbouchal/statnipokladna/issues}."))
    }  else {
      cli::cli_abort(c(x = "No CSV file inside the downloaded archive matches files needed for the table.",
                       i = "You might want to report a bug at {.url https://github.com/petrbouchal/statnipokladna/issues}."))
    }
  }
  return(table_file)
}


#' Load a statnipokladna table from a CSV file
#'
#' This is normally called inside `sp_get_table()` but can be used separately if
#' finer-grained control of intermediate outputs is needed, e.g. in a `{targets}` workflow.
#'
#' @param path path to a CSV file, as output by `sp_get_table_file()`.
#' @param ico Organisation ID to filter by, if supplied.
#'
#' @return a [tibble][tibble::tibble-package]. See help for `sp_get_table()` for a key to the columns.
#' @family Detailed workflow
#' @examples
#' \dontrun{
#' ds <- sp_get_dataset("rozv", 2018, 12)
#' tf <- sp_get_table_file("balance-sheet", ds)
#' sp_load_table(tf)
#' }
#' @export
sp_load_table <- function(path, ico = NULL) {

  suppressWarnings(suppressMessages(
    dt <- readr::read_csv2(path, col_types = readr::cols(.default = readr::col_character()))))

  dt_new_names <- stringr::str_remove_all(names(dt), "\"[A-\\u017da-\\u017e\\s\\-\\./]*\"") |>
    stringr::str_remove_all("/BIC/")

  dt <- dt %>%
    purrr::set_names(dt_new_names)

  # print(head(dt))
  if(max(stringr::str_length(dt$`ZC_ICO:ZC_ICO`), na.rm = TRUE) == 10) {
    dt <- dplyr::mutate(dt, `ZC_ICO:ZC_ICO` = stringr::str_sub(.data$`ZC_ICO:ZC_ICO`, 3, 10))
  }
  if(!is.null(ico)) dt <- dt[dt$`ZC_ICO:ZC_ICO` %in% ico,]

  dt <- dt |>
    purrr::set_names(stringr::str_remove(names(dt), "^[A-Z_0-9/]*:")) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("ZU_")), ~switch_minus(.) %>% as.numeric(.)) %>%
    tidyr::extract(.data$`0FISCPER`, c("vykaz_year", "vykaz_month"), "([0-9]{4})0([0-9]{2})") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_date")), lubridate::dmy) %>%
    dplyr::mutate(vykaz_date = lubridate::make_date(.data$vykaz_year, .data$vykaz_month),
                  vykaz_date = lubridate::make_date(.data$vykaz_year, .data$vykaz_month,
                                                      lubridate::days_in_month(.data$vykaz_date))) %>%
    dplyr::rename_all(dplyr::recode,
                      ZCMMT_ITM = "polozka",
                      ZC_VYKAZ = "vykaz",
                      ZC_POLVYK = "polvyk",
                      ZC_VTAB = "vtab",
                      ZC_UCJED = "ucjed",
                      ZFUNDS_CT = "finmisto",
                      ZC_FUND = 'zdroj',
                      `0FM_AREA` = "kapitola",
                      `0CI_TYPE` = "polozka_typ",
                      `0PU_MEASURE` = "rozprog",
                      ZC_ICO = "ico",
                      ZC_KRAJ = "kraj",
                      ZC_NUTS = "nuts",
                      ZC_LAU = "okres",
                      ZC_PSUK = "psuk",
                      ZC_UCRIS = "ucris",
                      ZC_EDS = "eds",
                      ZC_PVS = "pvs",
                      ZC_ZREUZ = "ucelznak",
                      ZU_MONET = "previous_net",
                      ZU_AOBTTO = "current_gross",
                      ZU_AONET = "current_net",
                      ZU_AOKORR = "current_correction",
                      ZU_HLCIN = "previous_core",
                      ZU_HOSCIN = "previous_economic",
                      ZU_HLCIBO = "current_core",
                      ZU_HCINBO = "current_economic",
                      FUNC0AREA = "paragraf",
                      ZU_ROZSCH = "budget_adopted",
                      ZU_ROZPZM = "budget_amended",
                      ZU_KROZP = "budget_final",
                      ZU_ROZKZ = "budget_spending",
                      ZU_ROZKZM = "budget_spending",
                      ZU_OBLIG = "budget_oblig",
                      ZU_STAVP = "before",
                      ZU_STAVPO = "after",
                      ZU_ZVYS = "increase",
                      ZU_SNIZ = "decrease",
                      ZU_BEZUO = "current",
                      ZU_SYNUC = "synuc",
                      `0FUNC_AREA` = "paragraf")
  return(dt)
}

#' Get a statnipokladna table
#'
#' Cleans and loads a table. If needed, a dataset containing the table is downloaded.
#'
#' The data is loaded from files downloaded automatically by `sp_get_dataset()`;
#' files persist in a temporary directory per session.
#'
#' How data for different time periods is exported differs by dataset.
#' This has significant implications for how you get to usable full-year numbers or time series in different tables.
#' See `vignette("statnipokladna")` for details on this.
#'
#' Data is processed in the following way:
#'
#' - all columns are given names that are human-readable and facilitato add codelists
#' - ICO (org. IDs) are normalised as in some datasets they are padded with leading zeros
#' - a vykaz_date, vykaz_year and vykaz_month columns are created to identify the time period
#' - value columns are transformed into numeric
#' - other columns are left as character to avoid losing information
#'
#' ## Correspondence between input and output columns
#'
#' **Shared/multiple tables**
#'
#'| Original | Output | English | Czech | Note |
#'  | --- | --- | --- | --- | --- |
#'  | ZC_VTAB | vtab | table number | tabulka | - |
#'  | ZC_UCJED | ucjed | accounting unit | účetní jednotka | NB: ucjed != ico; the two codes are different; ICO is universal, ucjed is specific to SP, and both denote an organisation. |
#'  | ZC_VYKAZ | vykaz | report number | výkaz  |  - |
#'  | ZFUNDS_CT | finmisto | accounting centre | finanční místo |  either kapitola or "organizační složka státu" (core state org) |
#'  | ZC_ICO | ico | org ID | IČO |  see ucjed |
#'  | ZC_FUND | zdroj | funding source | zdroj |  - |
#'  | ZC_KRAJ | kraj | region | kraj |  - |
#'  | ZC_NUTS | nuts | NUTS code | NUTS kód |  - |
#'  | ZC_LAU | okres | NUTS code of LAU1 unit | LAU1 kód | - |
#'
#'  **Tables `budget-*`**
#'
#'  | Original | Output | English | Czech | Note |
#'  | --- | --- | --- | --- | --- |
#'  | ZCMMT_ITM | polozka |  item/line |  položka (druhové členění) | NB: polozka != polvyk |
#'  | 0FM_AREA | kapitola | chapter  |  kapitola | - |
#'  | 0CI_TYPE | polozka_type | item/line type  |  typ položky | - |
#'  | FUNC0AREA | paragraf |  sector line | paragraf (odvětvové členění)  |  - |
#'  | ZC_PVS | pvs | programme code | programové výdaje státu | post-2014 |
#'  | ZC_EDS | eds | subsidy and proprty evidence | Evidenční dotační systém / správa majetku ve vlastnictví státu | post-2014, No codelist - perhaps external via https://www.edssmvs.cz/DocumentsList.aspx?Agenda=CEIS |
#'  | ZC_UCRIS | ucris | purpose | Účel | post-2014, no codelist found |
#'  | 0FUNC_AREA | paragraf |  sector line | paragraf (odvětvové členění)  |  - |
#'  | ZU_ROZSCH | budget_adopted |  budget as originally adopted | schválený rozpočet  |  - |
#'  | ZU_ROZPZM | budget_amended |  budget as amended throughout the year | rozpočet po změnách  |  - |
#'  | ZU_KROZP | budget_final |  final budget |  konečný rozpočet | - |
#'  | ZU_OBLIG | budget_oblig | ? | obligo | - |
#'  | ZU_ROZKZ | budget_spending |  actual spending | skutečnost  |  - |
#'
#'  **Table `budget-indicators`**
#'
#'  | Original | Output | English | Czech | Note |
#'  | ZC_PSUK | psuk | budgetary indicator | Závazný a průřezový indikátor | Use `psuk` codelist |
#'
#' **Table `budget-central-old-subsidies` **
#'
#'  | Original | Output | English | Czech | Note |
#'  | ZC_ZREUZ | ucelznak | Purpose identifier | Účelový znak | - |
#'
#' **Table `budget-central-old-purpose-grants` **
#'
#'  | Original | Output | English | Czech | Note |
#'  | 0PU_MEASURE | rozprog | Budgetary programme | Rozpočtový program | - |
#'
#'  **Tables `balance-sheet*`**
#'
#'  | Original | Output | English | Czech | Note |
#'  | --- | --- | --- | --- | --- |
#'  | ZC_POLVYK | polvyk | item/line  | položka výkazu  | -  |
#'  | ZC_SYNUC | synuc | synthetic account  | syntetický účet  | -  |
#'  | ZU_MONET | previous_net | net, previous period  | netto minulé období | - |
#'  | ZU_AOBTTO | current_gross | gross, current period   | brutto běžné období   | - |
#'  | ZU_AONET | current_net | net, current period  | netto běžné období  | - |
#'  | ZU_AOKORR | current_correction | correction, current period |  korekce běžné období | - |
#'
#'  **Tables `profit-and-loss-*`**
#'
#'  | Original | Output | English | Czech | Note |
#'  | --- | --- | --- | --- | --- |
#'  | ZU_HLCIN | previous_core | core activity, previous period |  hlavní činnost, minulé období |  - |
#'  | ZU_HOSCIN | previous_economic | economic activity, previous period  | hospodářská činnost, minulé období  | - |
#'  | ZU_HLCIBO | current_core | core activity, current period |  hlavní činnost, běžné období |  - |
#'  | ZU_HCINBO | current_economic | economic activity, current period |  hospodářská činnost, běžné období |  - |
#'
#'  **Table `changes-in-equity`**
#'
#'  | Original | Output | English | Czech | Note |
#'  | --- | --- | --- | --- | --- |
#'  | ZU_STAVP | before | previous period |  stav minulé období |  - |
#'  | ZU_STAVPO | after | current period | stav běžné období  | - |
#'  | ZU_ZVYS | increase | increase |  zvýšení stavu |  - |
#'  | ZU_SNIZ | decrease | decrease |  snížení stavu |  - |
#'
#'  **Table `cash-flow`**
#'
#'  | Original | Output | English | Czech | Note |
#'  | --- | --- | --- | --- | --- |
#'  | ZU_BEZUO | current | current period |  běžné účetní období |  - |
#'
#'
#' @param table_id A table ID. See `id` column in `sp_tables` for a list of available tables
#' @param year year, numeric, 2015-2019 for some datasets, 2010-2020 for others.
#' Can be a vector of length > 1 (see Details for how to work with data across time periods.).
#' @param month month, numeric. Must be 3, 6, 9 or 12. Can be a vector of length > 1 (see details).
#' @param ico ID(s) of org to return, character of length one or more. If unset, returns all orgs. ID not checked for correctness/existence. See <https://monitor.statnipokladna.cz/datovy-katalog/prohlizec-ciselniku/ucjed> to look up ID of any org in the dataset.
#' @param dest_dir character. Directory in which downloaded files will be stored.
#' If left unset, will use the `statnipokladna.dest_dir` option if the option is set, and `tempdir()` otherwise. Will be created if it does not exist.
#' @param redownload Redownload even if recent file present? Defaults to FALSE.
#'
#' @keywords internal
#'
#' @return a [tibble][tibble::tibble-package]; see Details for key to the columns
#' @encoding UTF-8
#' @examples
#' \dontrun{
#' allorgs_2018 <- sp_get_table("budget-central", 2018)
#' allorgs_mid2018 <- sp_get_table("budget-central", 2018, 6)
#' oneorg_multiyear <- sp_get_table("budget-central", 2017:2018, 12, ico = "00064581")
#' oneorg_multihalfyears <- sp_get_table("budget-central", 2017:2018, c(6, 12), ico = "00064581")
#' }
#' @export
#' @family Core workflow
#'
sp_get_table <- function(table_id, year, month = 12, ico = NULL,
                         redownload = FALSE, dest_dir = NULL) {
  stopifnot(is.character(ico) | is.null(ico))
  if(interactive() == FALSE & (missing(year) | missing(month))) {
    if(missing(year)) {
      cli::cli_abort(c(x = "{.var year} not set. Please set a value."))

    } else if(missing(month)) {
      cli::cli_alert_warning("{.var month} not set. Using default of {.value {month}}.")

    }
    if(!interactive()) cli::cli_inform("Set period parameters explicitly for reproducibility.")
  }

  if(is.null(dest_dir)) dest_dir <- getOption("statnipokladna.dest_dir",
                                              default = tempdir())

  if(!(table_id %in% sp_tables_i$id)) cli::cli_abort("Not a valid table id. Consult {.code sp_tables}.")
  dataset_id <- sp_tables_i$dataset_id[sp_tables_i$id == table_id]

  downloaded_datasets <- sp_get_dataset(dataset_id, year, month, dest_dir, redownload)

  return_table <- function(dataset_path, table_id, ico) {
    table_file_path <- sp_get_table_file(table_id, dataset_path, reunzip = redownload)
    tbl <- sp_load_table(table_file_path, ico)
    return(tbl)
  }

  dt_fin <- purrr::map_dfr(downloaded_datasets, return_table, table_id, ico)
  # onyr <- c(2018) %>% purrr::map_dfr(~ sp_get_table(51101, year = ., month = 12))
  return(dt_fin)
}
# onyr <- c(2018) %>% purrr::map_dfr(~ sp_get_table(2, year = ., month = 12))
# onyr <- c(2018) %>% purrr::map_dfr(~ sp_get_table(1, year = ., month = 12))

# Deprecated --------------------------------------------------------------

#' Deprecated: Get a statnipokladna table\cr\cr
#' Deprecated, use `sp_get_table()` instead.
#'
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams sp_get_table
#'
#' @keywords internal
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
get_table <- function(table_id, year, month = 12, ico = NULL,
                      redownload = FALSE, dest_dir = NULL) {
  lifecycle::deprecate_warn("0.5.2", "statnipokladna::get_table()", "sp_get_table()")
}
