PREFIX dct: <http://purl.org/dc/terms/>
PREFIX dcterm: <http://purl.org/dc/terms/>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX purl: <http://purl.org/dc/terms/>
PREFIX dcat: <http://www.w3.org/ns/dcat#>

SELECT ?ds_iri
WHERE
{
  ?cat_iri a dcat:Catalog ;
  purl:title ?cat_title .

  FILTER(LANG(?cat_title) = 'cs') .

  {?ds_iri dct:isPartOf <https://opendata.mfcr.cz/lod/monitor/MONITOR> .}

  VALUES ?cat_iri {<https://opendata.mfcr.cz/lod/monitor/>}

}
LIMIT 2000

-- =======

PREFIX dct: <http://purl.org/dc/terms/>
PREFIX dcterm: <http://purl.org/dc/terms/>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX purl: <http://purl.org/dc/terms/>
PREFIX dcat: <http://www.w3.org/ns/dcat#>
SELECT ?subds_iri
WHERE
{
  ?cat_iri a dcat:Catalog ;
  purl:title ?cat_title .

  FILTER(LANG(?cat_title) = 'cs') .

  {?ds_iri dct:isPartOf <https://opendata.mfcr.cz/lod/monitor/MONITOR> .}

  VALUES ?cat_iri {<https://opendata.mfcr.cz/lod/monitor/>}

  {?subds_iri dct:isPartOf ?ds_iri}

}
LIMIT 2000

-- =======

  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX dcterm: <http://purl.org/dc/terms/>
  PREFIX dcterms: <http://purl.org/dc/terms/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX purl: <http://purl.org/dc/terms/>
  PREFIX dcat: <http://www.w3.org/ns/dcat#>
SELECT ?dist_iri ?subds_iri ?dl_url ?start ?end ?media_type
WHERE
{
  ?cat_iri a dcat:Catalog ;
  purl:title ?cat_title .

  FILTER(LANG(?cat_title) = 'cs') .

  {?ds_iri dct:isPartOf <https://opendata.mfcr.cz/lod/monitor/MONITOR> .}

  VALUES ?cat_iri {<https://opendata.mfcr.cz/lod/monitor/>}

  {?subds_iri dct:isPartOf ?ds_iri}

  {?subds_iri dcat:distribution ?dist_iri .
    ?dist_iri dcat:accessURL ?dl_url .
    ?subds_iri dct:temporal ?tmprl .
    ?tmprl dcat:startDate ?start .
    ?tmprl dcat:endDate ?end .
    {?dist_iri dcat:mediaType ?media_type .}

  }
}
LIMIT 2000

-- =======

  PREFIX dct: <http://purl.org/dc/terms/>
  PREFIX dcterm: <http://purl.org/dc/terms/>
  PREFIX dcterms: <http://purl.org/dc/terms/>
  PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX purl: <http://purl.org/dc/terms/>
  PREFIX dcat: <http://www.w3.org/ns/dcat#>
SELECT ?dist_iri ?subds_iri ?dl_url ?start ?end ?media_type ?subds_title ?ds_title ?schema ?compression ?dist_title
WHERE
{
  ?cat_iri a dcat:Catalog ;
  purl:title ?cat_title .

  FILTER(LANG(?cat_title) = 'cs') .

  {?ds_iri dct:isPartOf <https://opendata.mfcr.cz/lod/monitor/MONITOR> .
    ?ds_iri purl:title ?ds_title}

  VALUES ?cat_iri {<https://opendata.mfcr.cz/lod/monitor/>}

  {?subds_iri dct:isPartOf ?ds_iri}

  {?subds_iri dcat:distribution ?dist_iri .
    ?subds_iri purl:title ?subds_title .
    ?dist_iri dcat:accessURL ?dl_url .
    ?subds_iri dct:temporal ?tmprl .
    OPTIONAL {?dist_iri dct:title ?dist_title . }
    ?dist_iri dcat:compressFormat ?compression .
    OPTIONAL {?dist_iri dct:conformsTo ?schema .}
    ?tmprl dcat:startDate ?start .
    ?tmprl dcat:endDate ?end .
    {?dist_iri dct:format ?media_type .}

  }
}
LIMIT 2000

