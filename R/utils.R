
openlinks <- function() {
  utils::browseURL("http://monitor.statnipokladna.cz/datovy-katalog/transakcni-data")
  utils::browseURL("http://monitor.statnipokladna.cz/datovy-katalog/ciselniky")
  utils::browseURL("http://monitor.statnipokladna.cz/")
}

release_questions <- function() {
  c(
    "Have you run all examples?",
    "Have you re-rendered README.Rmd?"
  )
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



check_online <- function(url) {

  if(!curl::has_internet()) ui_stop("No internet. Cannot continue; stopping.")

  url <- as.character(url)

  url_parsed <- httr::parse_url(url)
  url_modified <- url_parsed
  url_modified$path <- NULL
  url_modified$query <- NULL
  url_modified$params <- NULL
  host <- httr::build_url(url_modified)

  host_check <- !httr::http_error(host)

  if (!host_check) {
    host_resp <- httr::HEAD(host)
    host_status <- httr::http_status(host_resp)
    ui_stop("Host {ui_path(host)} not reachable or returns error on '/' (error {ui_value(host_status)}). Stopping.")
  }

  url_check <- !httr::http_error(url)

  if (!url_check) {
    url_resp <- httr::HEAD(url)
    url_status <- httr::http_status(url_resp)
    ui_stop("Resource {ui_path(url)} returns error {ui_value(url_status)}. Stopping.")
  }

  return(TRUE)
}
