sp_tables_i <- tibble::tribble(~table_num, ~report_num, ~id,   ~table_code,   ~dataset_id, ~file_regex, ~czech_name, ~note,
                               100,         51,         "budget-local", "finm_budget", "finm",      "FINM201",    "Pln\\u011bn\\u00ed rozpo\\u010dtu m\\u00edstn\\u011b \\u0159\\u00edzen\\u00fdch organizac\\u00ed" , NA,
                               100,         51,         "budget-local-purpose-grants", "finm_ucel",   "finm",      "FINM207",    "\\u00da\\u010delov\\u00e9 financov\\u00e1n\\u00ed m\\u00edstn\\u011b \\u0159\\u00edzen\\u00fdch organizac\\u00ed", NA,
                               0,           0,          "budget-indicators",     "misris_zu",   "misris",    "((MIS-RIS-ZU)|(ZU-MIS-RIS))", "Z\\u00e1vazn\\u00e9 ukazatele st\\u00e1tn\\u00edho rozpo\\u010dtu", "only central orgs",
                               0,           0,          "profit-and-loss",     "vykzz",       "vykzz",     "VYKZZ",      "V\\u00fdkaz zisk\\u016f a ztr\\u00e1t" , NA,
                               0,           0,          "profit-and-loss-city-districts",     "vykzzmc",       "vykzz",     "VYKZZMC",      "V\\u00fdkaz zisk\\u016f a ztr\\u00e1t - m\\u011bstsk\\u00e9 \\u010d\\u00e1sti", "only for 2018; in other years city districts are incorporated in balance sheet",
                               0,           0,          "changes-in-equity",  "pozvk", "pozvk", "POZVK", "P\\u0159ehled o zm\\u011bn\\u00e1ch vlastn\\u00edho kapit\\u00e1lu", NA,
                               0,           0,          "cash-flow",  "ppt", "ppt", "PPT", "P\\u0159ehled pen\\u011b\\u017en\\u00edch tok\\u016f", NA,
                               0,           0,          "balance-sheet",     "rozvaha1",    "rozv",      "ROZV[1]?",      "Rozvaha - prvn\\u00ed \\u010d\\u00e1st", NA,
                               0,           0,          "balance-sheet-2",     "rozvaha2",    "rozv",      "ROZV2",      "Rozvaha - druh\\u00e1 \\u010d\\u00e1st", NA,
                               0,           0,          "balance-sheet-city-districts",     "rozvaha1mc",  "rozv",      "ROZV1MC",    "Rozvaha m\\u011bstsk\\u00fdch \\u010d\\u00e1st\\u00ed - prvn\\u00ed \\u010d\\u00e1st", "only for 2018; in other years city districts are incorporated in balance sheet",
                               0,           0,          "balance-sheet-city-districts-2",     "rozvaha2mc",  "rozv",      "ROZVMC2",    "Rozvaha m\\u011bstsk\\u00fdch \\u010d\\u00e1st\\u00ed - druh\\u00e1 \\u010d\\u00e1st", "only for 2018; in other years city districts are incorporated in balance sheet",
                               0,           0,          "budget-central-old",     "finu_budget",  "finu",      "FINU101",    "Pln\\u011bn\\u00ed rozpo\\u010dtu \\u00fast\\u0159edn\\u011b \\u0159\\u00edzen\\u00fdch organizac\\u00ed do 2014", "pre-2015 only",
                               0,           0,          "budget-central-old-purpose-grants",     "finu_ucel",  "finu",      "FINU107",   "\\u00da\\u010delov\\u00e9 financov\\u00e1n\\u00ed - poskytnut\\u00e9 prost\\u0159edky", "pre-2015 only",
                               0,           0,          "budget-central-old-subsidies",     "finu_dotace",  "finu",      "FINU108",    "Dota\\u010dn\\u00ed financov\\u00e1n\\u00ed - poskytnut\\u00e9 prost\\u0159edky", "pre-2015 only",
                               0,           0,          "budget-central",     "misris",      "misris",    "/MIS-RIS",    "Pln\\u011bn\\u00ed rozpo\\u010dtu \\u00fast\\u0159edn\\u011b \\u0159\\u00edzen\\u00fdch organizac\\u00ed od 2015", "post-2015 only") %>%
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
"sp_tables" <- sp_tables_i %>% dplyr::select(.data$id, .data$dataset_id,
                                             .data$czech_name, .data$note) %>%
  dplyr::mutate_if(is.character, stringi::stri_unescape_unicode)
# usethis::use_data(sp_tables, overwrite = TRUE)

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
#' - a period, per_yr and per_m columns are created to identify the time period
#' - value columns are transformed into numeric
#' - other columns are left as character to avoid losing information
#'
#' ## Correspondence between input and output columns
#'
#' **Shared**
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
#'
#'  **Tables `budget-*`**
#'
#'  | Original | Output | English | Czech | Note |
#'  | --- | --- | --- | --- | --- |
#'  | ZCMMT_ITM | polozka |  item/line |  položka (druhové členění) | NB: polozka != polvyk |
#'  | 0FM_AREA | kapitola | chapter  |  kapitola | - |
#'  | FUNC0AREA | paragraf |  sector line | paragraf (odvětvové členění)  |  - |
#'  | 0FUNC_AREA | paragraf |  sector line | paragraf (odvětvové členění)  |  - |
#'  | ZU_ROZSCH | budget_adopted |  budget as originally adopted | schválený rozpočet  |  - |
#'  | ZU_ROZPZM | budget_amended |  budget as amended throughout the year | rozpočet po změnách  |  - |
#'  | ZU_KROZP | budget_final |  final budget |  konečný rozpočet | - |
#'  | ZU_ROZKZ | budget_spending |  actual spending | skutečnost  |  - |
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
#' @param table_id A table ID. See `id` column in `sp_tables` for a list of available codelists.
#' @param year year, numeric, 2015-2018 for some datasets, 2010-2018 for others.
#' Can be a vector of length > 1 (see Details for how to work with data across time periods.).
#' @param month month, numeric. Must be 3, 6, 9 or 12. Can be a vector of length > 1 (see details).
#' @param ico ID(s) of org to return, character of length one or more. If unset, returns all orgs. ID not checked for correctness/existence. See <http://monitor.statnipokladna.cz/datovy-katalog/prohlizec-ciselniku/ucjed> to look up ID of any org in the dataset.
#' @param dest_dir character. Directory in which downloaded files will be stored.
#' If left unset, will use the `statnipokladna.dest_dir` option if the option is set, and `tempdir()` otherwise. Will be created if it does not exist.
#' @param redownload Redownload even if recent file present? Defaults to FALSE.
#'
#' @return a [tibble][tibble::tibble-package]; see Details for key to the columns
#' @encoding UTF-8
#' @examples
#' \donttest{
#' allorgs_latest <- sp_get_table("budget-central")
#' allorgs_2018 <- sp_get_table("budget-central", 2018)
#' allorgs_mid2018 <- sp_get_table("budget-central", 2018, 6)
#' oneorg_multiyear <- sp_get_table("budget-central", 2017:2018, 12, ico = "00064581")
#' oneorg_multihalfyears <- sp_get_table("budget-central", 2017:2018, c(6, 12), ico = "00064581")
#' }
#' @export
#' @family Core workflow
#'
sp_get_table <- function(table_id, year = 2018, month = 12, ico = NULL,
                      redownload = FALSE, dest_dir = NULL) {
  stopifnot(is.character(ico) | is.null(ico))
  if(interactive() == FALSE & (missing(year) | missing(month))) {
    usethis::ui_warn("Either {usethis::ui_field('year')} or {usethis::ui_field('month')} not set.
                     Using defaults of {usethis::ui_value(year)} and {usethis::ui_value(month)}.")
    usethis::ui_todo("Set these values explicitly for reproducibility as the defaults may change in the future
                     to provide access to the latest data by default.")

  }

  if(is.null(dest_dir)) dest_dir <- getOption("statnipokladna.dest_dir",
                                              default = tempdir())

  if(!(table_id %in% sp_tables_i$id)) usethis::ui_stop("Not a valid table id. Consult {usethis::ui_code('sp_tables')}.")
  dataset_id <- sp_tables_i$dataset_id[sp_tables_i$id == table_id]
  table_regex <- paste0(sp_tables_i$file_regex[sp_tables_i$id == table_id])
  get_one_table <- function(dataset_id, year = year, month = month, dest_dir = dest_dir,
                            redownload = redownload) {
    dslist <- sp_get_dataset(dataset_id, year = year, month = month, dest_dir = dest_dir,
                          redownload = redownload)
    table_file <- dslist[stringr::str_detect(dslist,
                                             paste0(table_regex, "(_[0-9]*)?\\.(csv|CSV)"))]
    stopifnot(length(table_file) == 1)
    usethis::ui_info("Reading data...")
    suppressWarnings(suppressMessages(
      dt <- readr::read_csv2(table_file, col_types = readr::cols(.default = readr::col_character()))))
    # print(head(dt))
    usethis::ui_info("Transforming data...")
    if(max(stringr::str_length(dt$`ZC_ICO:ZC_ICO`), na.rm = TRUE) == 10) {
      dt <- dplyr::mutate(dt, `ZC_ICO:ZC_ICO` = stringr::str_sub(.data$`ZC_ICO:ZC_ICO`, 3, 10))
    }
    if(!is.null(ico)) dt <- dt[dt$`ZC_ICO:ZC_ICO` %in% ico,]
    dt <- dt %>%
      purrr::set_names(stringr::str_remove(names(dt), "^[A-Z_0-9/]*:")) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::starts_with("ZU_")), ~switch_minus(.) %>% as.numeric(.)) %>%
      tidyr::extract(.data$`0FISCPER`, c("per_yr", "per_m"), "([0-9]{4})0([0-9]{2})") %>%
      dplyr::mutate(period_vykaz = lubridate::make_date(.data$per_yr, .data$per_m),
                    period_vykaz = lubridate::make_date(.data$per_yr, .data$per_m,
                                                        lubridate::days_in_month(.data$period_vykaz))) %>%
      dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_date")), lubridate::dmy) %>%
      dplyr::rename_all(dplyr::recode,
                        ZCMMT_ITM = "polozka",
                        ZC_VYKAZ = "vykaz",
                        ZC_POLVYK = "polvyk",
                        ZC_VTAB = "vtab",
                        ZC_UCJED = "ucjed",
                        ZFUNDS_CT = "finmisto",
                        ZC_FUND = 'zdroj',
                        `0FM_AREA` = "kapitola",
                        ZC_ICO = "ico",
                        ZC_KRAJ = "kraj",
                        ZC_NUTS = "nuts",
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
                        ZU_STAVP = "before",
                        ZU_STAVPO = "after",
                        ZU_ZVYS = "increase",
                        ZU_SNIZ = "decrease",
                        ZU_BEZUO = "current",
                        ZU_SYNUC = "synuc",
                        `0FUNC_AREA` = "paragraf")
    return(dt)
  }
  years_months <- expand.grid(y = year, m = month)
  dt_fin <- purrr::map2_dfr(years_months$y, years_months$m,
                            ~get_one_table(dataset_id, .x, .y, dest_dir = dest_dir,
                                           redownload = redownload))
  # onyr <- c(2018) %>% purrr::map_dfr(~ sp_get_table(51101, year = ., month = 12))

  }
# onyr <- c(2018) %>% purrr::map_dfr(~ sp_get_table(2, year = ., month = 12))
# onyr <- c(2018) %>% purrr::map_dfr(~ sp_get_table(1, year = ., month = 12))

#' Deprecated: Get a statnipokladna table\cr\cr
#' Deprecated, use `sp_get_table()` instead.
#'
#' \lifecycle{deprecated}
#'
#' @inheritParams sp_get_table
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
get_table <- function(table_id, year = 2018, month = 12, ico = NULL,
                      redownload = FALSE, dest_dir = NULL) {
  lifecycle::deprecate_warn("0.5.2", "statnipokladna::get_table()", "sp_get_table()")
}
