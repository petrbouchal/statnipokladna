
openlinks <- function() {
  utils::browseURL("http://monitor.statnipokladna.cz/2019/zdrojova-data/transakcni-data")
  utils::browseURL("http://monitor.statnipokladna.cz/2019/zdrojova-data/ciselniky")
  utils::browseURL("http://monitor.statnipokladna.cz/")
}

release_questions <- function() {
  c(
    "Have you run all examples?",
    "Have you re-rendered README.Rmd?"
  )
}
