
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
