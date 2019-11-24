sp_tables_i <- tibble::tribble(~table_num, ~report_num, ~id, ~table_code, ~dataset_id, ~file_stub, ~implemented,
                             100,         51,         51100, "finm_budget", "finm", "FINM201", F,
                             100,         51,         51101, "finm_budget2", "finm", "FINM207", F,
                             0,         0,         1, "misris_zu", "misris", "MIS-RIS-ZU", F,
                             0,         0,         3, "vykzz", "vykzz", "VYKZZ", F,
                             0,         0,         4, "rozvaha", "rozv", "VYKZZ", F,
                             0,         0,         2, "misris", "misris", "MIS-RIS", F) %>%
  dplyr::mutate_if(is.double, as.integer) %>%
  dplyr::arrange(id)
# stringi::stri_escape_unicode("xxx")


#' List of available tables
#'
#' Contains IDs and names of all available tables that can be
#' retrieved by get_table. Look inside the XLS documentation for each dataset at <https://monitor.statnipokladna.cz/2019/zdrojova-data/transakcni-data>
#' to see more detailed descriptions. Note that tables do not correspond to the tabulka/`vtab` attribute of the tables, they represent files inside datasets.
#'
#' @format A data frame with 2 rows and 4 variables:
#' \describe{
#'   \item{\code{id}}{integer Table id, used as `id` argument to `get_table`. Comprises report number and table number.}
#'   \item{\code{table_code}}{character. Table code, should be human readable.}
#'   \item{\code{table_num}}{integer Table number.}
#'   \item{\code{report_num}}{integer Number of report (vykaz) containing the table.}
#' }
"sp_tables" <- sp_tables_i %>% dplyr::select(id, table_code, table_num, report_num)
# usethis::use_data(sp_tables, overwrite = T)

#' Get a statnipokladna table
#'
#' Cleans and loads a table. If needed, a dataset containing the table is downloaded.
#'
#' @param table_id A table ID. See `id` column in `sp_tables` for a list of available codelists.
#' @param year year, numeric, 2015-2018 for some datasets, 2010-2018 for others.
#' @param month month, numeric. Must be 3, 6, 9 or 12.
#' @param force_redownload Redownload even if recent file present? Defaults to FALSE.
#' @param ico ID(s) of org to return, character of length one or more. If unset, returns all orgs. ID not checked for correctness/existence. See <http://monitor.statnipokladna.cz/2019/zdrojova-data/prohlizec-ciselniku/ucjed> to look up ID of any org in the dataset.
#'
#' @return a tibble
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
get_table <- function(table_id, year = 2018, month = 12, force_redownload = FALSE, ico = NULL) {
  dataset_id <- sp_tables_i$dataset_id[sp_tables_i$id == table_id]
  table_stub <- paste0(sp_tables_i$file_stub[sp_tables_i$id == table_id], "_")
  dslist <- get_dataset(dataset_id, year = year, month = month, force_redownload = force_redownload)
  table_file <- dslist[stringr::str_detect(dslist, table_stub)]
  dt <- readr::read_csv2(table_file, col_types = readr::cols(`ZC_ICO:ZC_ICO` = "c",
                                                             `0FISCPER:0FISCPER` = "c",
                                                             `ZC_UCJED:ZC_UCJED` = "c",
                                                             `ZU_ROZKZM:ZU_ROZKZM` = 'c',
                                                             `ZU_ROZPZM:ZU_ROZPZM` = 'c',
                                                             `ZU_ROZKZ:ZU_ROZKZ` = 'c',
                                                             `ZU_ROZSCH:ZU_ROZSCH` = 'c',
                                                             `ZU_KROZP:ZU_KROZP` = 'c',
                                                             `ZC_UCJED:ZC_UCJED` = "c",
                                                             `0FM_AREA:0FM_AREA` = 'c',
                                                             `ZCMMT_ITM:ZCMMT_ITM` = "c"))
  print(head(dt))
  dt <- dt %>%
    magrittr::set_names(stringr::str_remove(names(.), "^[A-Z_0-9/]*:")) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("ZU_")), ~switch_minus(.) %>% as.numeric(.)) %>%
    tidyr::extract(`0FISCPER`, c("per_yr", "per_m"), "([0-9]{4})0([0-9]{2})") %>%
    dplyr::mutate(period_vykaz = lubridate::make_date(per_yr, per_m),
                  period_vykaz = lubridate::make_date(per_yr, per_m,
                                                      lubridate::days_in_month(period_vykaz))) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("kon_")), as.logical) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_date")), lubridate::dmy) %>%
    dplyr::rename_all(dplyr::recode,
                      ZCMMT_ITM = "polvyk",
                      ZC_VYKAZ = "vykaz",
                      ZC_VTAB = "vtab",
                      ZC_UCJED = "ucjed",
                      ZFUNDS_CT = "finmisto",
                      ZC_FUND = 'zdroj',
                      `0FM_AREA` = "kapitola",
                      ZC_ICO = "ico",
                      ZC_KRAJ = "kraj",
                      ZC_NUTS = "nuts",
                      FUNC0AREA = "paragraf",
                      `0FUNC_AREA` = "paragraf")
  if(!is.null(ico)) dt <- dt[dt$ico %in% ico,]
  return(dt)
}
# onyr <- c(2018) %>% purrr::map_dfr(~ get_table(51101, year = ., month = 12))
# onyr <- c(2018) %>% purrr::map_dfr(~ get_table(2, year = ., month = 12))
# onyr <- c(2018) %>% purrr::map_dfr(~ get_table(1, year = ., month = 12))
