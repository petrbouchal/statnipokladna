library(jsonlite)
library(httr)


td <- GET("https://monitor.statnipokladna.cz/api/transakcni-data?aktivni=true",accept_json()) %>%
  content(as = "text") %>%
  fromJSON()

xx <- td %>%
  unnest(dataExtracts, .name_repair = "universal")

xx %>%
  filter(year == 2019) %>%
  View()
