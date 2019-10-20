sp_codelists <- tibble::tribble(~id, ~name,
                               "ucjed", "\\u00da\\u010detn\\u00ed jednotka",
                               "polvyk", "Polo\\u017eka v\\u00fdkazu") %>%
  dplyr::mutate(name = stringi::stri_unescape_unicode(name))

get_codelist <- function(codelist) {
  url <- get_codelist_url(codelist)
  message("Downloading codelist data")
  vy <- httr::with_config(config = httr::config(useragent = usr),
                          xml2::read_xml(url)) %>% xml2::xml_children()
  message("Processing codelist data")
  xnames <- purrr::map(vy, function(x) x %>% xml2::xml_children() %>% xml2::xml_name())
  if(length(unique(xnames)) == 1)
  {nms <- xnames[[1]]}
  else
    stop("No single set of names")

  xvals <- purrr::map_df(vy, function(x) {x %>% xml2::xml_children() %>%
      xml2::xml_text() %>%
      as.character() %>%
      t() %>% tibble::as_tibble()}) %>%
    purrr::set_names(nms)
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

join_codelist <- function(data, codelist_id = NULL, codelist_df = NULL, period_column) {
  if(is.null(codelist_id) & is.null(codelist_df)) {
    stop("At least one of codelist_id and codelist_df must be specified.")
  }
  if(is.null(codelist_df) & is.character(codelist_id)) {
    ciselnik <- get_codelist(codelist_id)
  } else if (is.data.frame(codelist_df) & !is.null(codelist_id)) {
    stop("Both `codelist_df` and `codelist_id` supplied. Please supply only one")
  } else if (is.data.frame(codelist_df) & is.null(codelist_id)) {
    if(!(c("end_date", "start_date") %in% names(codelist_df)) & length(codelist_df) > 2) {
      stop("Wrong column specification of `codelist_df`. It must contain columns named
           `start_date`, `end_date` and at least one more column")
    }
    ciselnik <- codelist_df
  }

  stopifnot(period_column %in% names(data),
            "data.frame" %in% class(data),
            "data.frame" %in% class(ciselnik))
  perd = unique(data[{{ period_column }}])
  stopifnot(length(unique(data [{{ period_column }}])) == 1)
  ciselnik_filtered <- ciselnik %>%
    dplyr::filter({{ end_date }} >= perd)

  slepeno <- dplyr::left_join(data, ciselnik_filtered)

  return(slepeno)
}

get_codelist_url <- function(codelist, check_if_exists = T) {
  if(!(codelist %in% sp_codelists$id)) stop("Not a valid codelist ID")
  codelist_name <- sp_codelists[sp_codelists$id == codelist, "name"]
  message(stringr::str_glue("Building URL for codelist {codelist} - {codelist_name}"))
  x <- stringr::str_glue("{sp_base_url}/data/{codelist}.xml")
  if(check_if_exists) {
    iserror <- httr::http_error(x, httr::config(followlocation = 0L), USE.NAMES = FALSE)
    if(iserror) stop("Codelist XML for a codelist with this ID does not exist")
  }
  return(x)
}
