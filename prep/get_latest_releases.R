#!/usr/bin/env Rscript

library(jsonlite)
library(httr)
library(dplyr)
library(tidyr)


td <- GET("https://monitor.statnipokladna.gov.cz/api/transakcni-data?aktivni=true",accept_json()) %>%
  content(as = "text") %>%
  fromJSON()

xx <- td %>%
  unnest(dataExtracts, .name_repair = "universal")

xx %>%
  group_by(titleCS) %>%
  filter(!deleted, year == max(year)) %>%
  filter(month == max(month)) %>%
  select(titleCS, year, month, filenamePeriod)
