munge_codelist <- function(url) {

  vy <- read_xml(url) %>% xml_children()

  xnames <- map(vy, function(x) x %>% xml_children() %>% xml_name())
  if(length(unique(xnames)) == 1)
  {nms <- xnames[[1]]}
  else
    stop("No single set of names")

  xvals <- map_df(vy, function(x) {x %>% xml_children() %>%
      xml_text() %>%
      as.character() %>%
      t() %>% as_tibble()}) %>%
    set_names(nms)
  return(xvals)
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

add_register <- function(data, register_name, period_column = period) {
  stopifnot(period_column %in% names(data),
            "data.frame" %in% class(data),
            "data.frame" %in% class(ciselnik))
  perd = unique(data[{{ period_column }}])
  stopifnot(length(unique(data[period_column])) == 1)
  ciselnik_filtered <- ciselnik %>%
    filter({{ end_date }} >= perd)

  slepeno <- left_join(data, ciselnik_filtered)

  return(slepeno)
}
