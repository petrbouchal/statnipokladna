
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
