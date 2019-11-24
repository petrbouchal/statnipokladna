vn <- "Species"

iris %>%
  dplyr::filter(get(vn) == "setosa")

mtcars %>%
  dplyr::rename_all(dplyr::recode, cyl = "cylinder", blah = "blah")
