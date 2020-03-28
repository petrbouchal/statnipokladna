obdobi_to_short <- function(year, month) {
  year_stub <- stringr::str_sub(as.character(year), 3, 4)
  month_formatted <- stringr::str_pad(month, 2, side = "left", pad = "0")
  return(paste0(year_stub, month_formatted))
}

get_sp_monitoring <- function(ico, obdobi = "1812") {
  url <- stringr::str_glue("{sp_base_url}/api/ucetni-jednotka/{ico}/monitoring?obdobi={obdobi}")
  moniresp <- httr::GET(url, httr::accept_json(), httr::user_agent("github.com/petrbouchal/"))

  moniresp %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()
}

get_sp_budgetresp <- function(ico, obdobi = "1812") {
  url <- stringr::str_glue("{sp_base_url}/api/rozpocet/odpovednost?ic={ico}&obdobi={obdobi}")
  moniresp <- httr::GET(url, httr::accept_json(), httr::user_agent("github.com/petrbouchal/"))

  moniresp %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()
}

# mesta_ico <- c("00064581", "00075370", "00845451", "44992785")

# resp <- purrr::map_df(mesta_ico, get_sp_budgetresp, .id = "mesto")
# moni <- purrr::map_df(mesta_ico, get_sp_monitoring, .id = "mesto")
#
# moni_enriched <- moni %>%
#   dplyr::left_join(moni %>%
#                      dplyr::filter(name == "Počet obyvatel") %>%
#                      dplyr::select(mesto, pop = value))

sp_get_monitoring <- function(ico, period = "1812") {
  moni <- purrr::map_df(ico, get_sp_monitoring, .id = "ico")

  pocobyv_unescaped <- stringi::stri_unescape_unicode("Po\\u010det obyvatel")

  moni_enriched <- moni %>%
    dplyr::left_join(moni %>%
                       dplyr::filter(.data$name == .data$pocobyv_unescaped) %>%
                       dplyr::select(.data$ico, pop = .data$value))
}
