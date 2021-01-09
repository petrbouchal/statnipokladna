
#' List all files currently available from the data provider
#'
#' Queries the SPARQL endpoint at <https://opendata.mfcr.cz/lod/monitor>
#' and
#'
#' @return A [tibble][tibble::tibble-package] with one row per downloadable file
#' @examples
#' \donttest{
#' sp_get_catalogue()
#' }
#' @format A data frame with these variables:
#' \describe{
#'   \item{\code{table}}{character. Table name incl. period (long name, does not correspond to dataset label in `sp_tables`).}
#'   \item{\code{dataset}}{character. Dataset (long name, does not correspond to dataset label in `sp_datasets`).}
#'   \item{\code{start}}{date. Start date of temporal coverage for this file.}
#'   \item{\code{end}}{date End date of temporal coverage for this file.}
#'   \item{\code{filetype}}{character. Filetyp. Currently 'csv' for all files.}
#'   \item{\code{compression}}{character. Type of compression. Currently 'zip' for all files.}
#'   \item{\code{url}}{character. Link to downloadable file/archive.}
#'   \item{\code{doc}}{character. Link to documantation Currently empty as not provided by endpoint.}
#'   \item{\code{schema}}{character. Link to schema. Currently empty as not provided by endpoint.}
#' }
#' @family Lists of available entities
#' @export
sp_list_datasets <- function() {

  sparql_url <- "https://opendata.mfcr.cz/lod/sparql"

  sparqlquery_datasets_byczso <- stringr::str_c("
    PREFIX dct: <http://purl.org/dc/terms/>
    PREFIX dcterm: <http://purl.org/dc/terms/>
    PREFIX dcterms: <http://purl.org/dc/terms/>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX purl: <http://purl.org/dc/terms/>
    PREFIX dcat: <http://www.w3.org/ns/dcat#>
    PREFIX foaf: <http://xmlns.com/foaf/0.1/>
    SELECT ?dist_iri ?subds_iri ?dl_url ?start ?end ?media_type
           ?subds_title ?ds_title ?schema ?compression ?dist_title ?doc
    WHERE
    {
      {?ds_iri dct:isPartOf <https://opendata.mfcr.cz/lod/monitor/MONITOR> .
       ?ds_iri purl:title ?ds_title}

      VALUES ?cat_iri {<https://opendata.mfcr.cz/lod/monitor/>}

      {?subds_iri dct:isPartOf ?ds_iri}

      {?subds_iri dcat:distribution ?dist_iri .
       ?subds_iri purl:title ?subds_title .
       ?dist_iri dcat:accessURL ?dl_url .
        OPTIONAL {?subds_iri foaf:page ?doc . }
       ?subds_iri dct:temporal ?tmprl .
        OPTIONAL {?dist_iri dct:title ?dist_title . }
       ?dist_iri dcat:compressFormat ?compression .
        OPTIONAL {?dist_iri dct:conformsTo ?schema .}
       ?tmprl dcat:startDate ?start .
       ?tmprl dcat:endDate ?end .
       {?dist_iri dcat:mediaType ?media_type .}

      }
    }
    LIMIT 2000") %>%
    stringi::stri_unescape_unicode()

  params = list(`default-graph-uri` = "",
                query = sparqlquery_datasets_byczso,
                # format = "application/sparql-results+json",
                format = "text/csv",
                timeout = 30000,
                debug = "on",
                run = "Run Query")
  if(!curl::has_internet()) usethis::ui_stop(c("No internet connection. Cannot continue. Retry when connected."))
  usethis::ui_info("Reading data from data.gov.cz")
  cat_rslt <- httr::GET(sparql_url, query = params,
                        # accept("application/sparql-results+json"),
                        httr::user_agent(usr),
                        httr::add_headers(c("Accept-Charset" = "utf-8")),
                        httr::accept("text/csv;charset=UTF-8")) %>%
    httr::stop_for_status()

  # print(params$query)

  if(httr::status_code(cat_rslt) > 200) {
    print(httr::http_status(cat_rslt))
    rslt <- httr::content(cat_rslt, as = "text")
  } else
    rslt <- cat_rslt %>% httr::content(as = "text")
  rslt <- readr::read_csv(rslt, col_types = readr::cols(start = "D",
                                                        end = "D",
                                                        doc = "c",
                                                        schema = "c"))
  usethis::ui_done("Done downloading and reading data")
  usethis::ui_info("Transforming data")
  rslt <- rslt %>%
    dplyr::mutate(filetype = stringr::str_extract(media_type, "(?<=/)[a-zA-Z]*$"),
                  compression = stringr::str_extract(compression, "(?<=/)[a-zA-Z]*$")) %>%
    dplyr::select(dataset_edition = subds_title, dataset_name = ds_title,
                  start, end,
                  filetype, compression,
                  url = dl_url, doc, schema)
  return(rslt)
}

# spd <- sp_get_catalogue()
# unique(spd$filetype)
# unique(spd$compression)
# spd
#
# spd %>% group_by(ds_title) %>% slice_max(end)
