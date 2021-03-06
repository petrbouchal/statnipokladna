library(jsonlite)
library(httr)

tfs <- httr::GET("https://monitor.statnipokladna.cz/api/transakcni-data") %>%
  content(as = "text") %>%
  fromJSON() %>%
  unnest(cols = c(dataExtracts), .names_repair = "universal")
