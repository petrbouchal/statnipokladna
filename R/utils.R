
openlinks <- function() {
  utils::browseURL("https://monitor.statnipokladna.gov.cz/datovy-katalog/transakcni-data")
  utils::browseURL("https://monitor.statnipokladna.gov.cz/datovy-katalog/ciselniky")
  utils::browseURL("https://monitor.statnipokladna.gov.cz/")
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

  if(!curl::has_internet()) cli::cli_abort("No internet. Cannot continue; stopping.")

  # https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r

  sHEAD <- purrr::safely(httr::HEAD)
  hd <- sHEAD(url)

  url_works <- !is.null(hd$result)

  if (url_works) {
    url_status <- httr::status_code(hd$result)
    if(url_status > 200)
    cli::cli_abort("Resource {.url {url}} returns code {.value {url_status}}. Stopping.")
  } else {
    host <- httr::parse_url(url)[["hostname"]]
    cli::cli_abort("Host {.url {host}} not reachable. Stopping.")
  }

  return(TRUE)
}
