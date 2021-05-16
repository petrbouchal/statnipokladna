
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

  # https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r

  sHEAD <- purrr::safely(httr::HEAD)
  hd <- sHEAD(url)

  url_works <- !is.null(hd$result)

  if (url_works) {
    url_status <- httr::status_code(hd$result)
    if(url_status > 200)
    ui_stop("Resource {ui_path(url)} returns code {ui_value(url_status)}. Stopping.")
  } else {
    host <- httr::parse_url(url)[["hostname"]]
    ui_stop("Host {ui_path(host)} not reachable. Stopping.")
  }

  return(TRUE)
}
