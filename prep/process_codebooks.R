source("utils.R")
ciselnik_organizace <- munge_codelist("https://monitor.statnipokladna.gov.cz/data/ucjed.xml")

cis_org_saved <- read_rds("data-output/ciselnik_organizace.rds")

write_rds(ciselnik_organizace, "data-output/ciselnik_organizace.rds", compress = "gz")
