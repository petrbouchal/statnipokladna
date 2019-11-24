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

