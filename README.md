
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statnipokladna <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/statnipokladna)](https://CRAN.R-project.org/package=statnipokladna)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/petrbouchal/statnipokladna.svg?branch=master)](https://travis-ci.org/petrbouchal/statnipokladna)
<!-- badges: end -->

The goal of statnipokladna is to provide programmatic access to open
data from the Státní pokladna system. Státní pokladna is a comprehensive
budgeting, reporting and accounting system for Czech public
organisations. This package provides user-friendly ways to access the
open data from that system available at
<https://monitor.statnipokladna.cz>.

## Installation

The package is not on CRAN.

You can install the current released version of statnipokladna from
[GitHub](https://github.com/petrbouchal/statnipokladna) with:

``` r
remotes::install_github("petrbouchal/statnipokladna",
                        build_vignettes = T,
                        ref = github_release())
```

or the latest development version with

``` r
remotes::install_github("petrbouchal/statnipokladna",
                         build_vignettes = T)
```

## What this package aims to enable you to do:

  - get cleaned-up, ready to analyse data frames based on open data
    dumps from the public finance database
      - the package draws on the online data and returns a clean data
        frame (*available in experimental form*)
      - the resulting data is ready to merge into time series (*to do*)
      - time series is built based on user input
  - do this through a consistent API which supplements some of the
    documentation that is missing from the official endpoints (*partly
    available*)
  - access registers published alongside the data (e.g. lists of public
    organisations with their identifiers and metadata), some of which
    can be useful in other contexts (*done*)
  - augment the core data with the desired type of register (*to do*)

## How does this compare to the [official analytical interface](http://monitor.statnipokladna.cz/)?

  - no limit on the number of data points
  - no limits on the number of organisations, unlike the official
    interface which forces you to use a filter on some tables
  - different reports (local gov, central gov…) in one place in
    consistent form
  - no need to go between
  - much faster for analysis (the current version of the online
    interface takes long to render)
  - reproducible\!\!\! The online interface has no facility to save an
    analysis, so even a browser reload (a) kills your work and (b) kills
    an trace of how a table was produced
  - no need for the web =\> excel =\> R dance
  - drawback: for some reports, the data is published in different forms
    for different time periods (pre- and post-2015)
  - drawback: consolidation must be done manually

Note the official analysis GUI is due to be [overhauled in
November 2018](https://twitter.com/otevrenadatamf/status/1190329092916289536).

## Getting started

``` r
library(statnipokladna)
```

Get data from a particular part (file) of a dataset (“výkaz”):

``` r
local_budgets <- get_table(table_id = "budget-local", # table ID, see `sp_tables`
                           year = 2018,
                           month = 12)
#> Building URL for dataset `finm`: FIN 2-12 M - Plnění rozpočtu MŘO, 2018-12
#> http://monitor.statnipokladna.cz/data/2018_12_Data_CSUIS_FINM.zip
#> Get the dataset documentation at http://monitor.statnipokladna.cz/data/struktura/finm.xlsx
#> Files already in /var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp2h9tls/statnipokladna/finm, not downloading. Set `force_redownload` to TRUE if needed.
```

The data is automatically downloaded to a cache directory, so it will be
reused by future calls to `get_table()` made in the same session, unless
you set `force_redownload = T`.

It is a rather raw-looking data frame…

``` r
head(local_budgets)
#> # A tibble: 6 x 15
#>   vykaz vtab  per_yr per_m ucjed ico   kraj  nuts  `0CI_TYPE` paragraf polozka ZU_ROZSCH ZU_ROZPZM
#>   <chr> <chr> <chr>  <chr> <chr> <chr> <chr> <chr>      <dbl> <chr>    <chr>       <dbl>     <dbl>
#> 1 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 3412     6341            0 25313145.
#> 2 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5011     13012000 14252023 
#> 3 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5021        90000    90000 
#> 4 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5024      3288000  2140497 
#> 5 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5031      3253000  3535818 
#> 6 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5032      1172000  1283013 
#> # … with 2 more variables: ZU_ROZKZ <dbl>, period_vykaz <date>
```

…but it has been cleaned up, and can be enriched with any of the
metadata codelists:

``` r
functional_categories <- get_codelist("paragraf")
#> Building URL for codelist paragraf - Paragraf
#> Downloading codelist data
#> Processing codelist data
```

``` r
functional_categories
#> # A tibble: 550 x 9
#>    paragraf skupina    oddil    pododdil      nazev       kr_nazev  str_nazev   start_date end_date  
#>    <chr>    <chr>      <chr>    <chr>         <chr>       <chr>     <chr>       <date>     <date>    
#>  1 0000     Příjmy     Příjmy   Příjmy        Pro příjmy… "Pro pří… "Pro příjm… 1900-01-01 9999-12-31
#>  2 1011     Zemědělst… Zeměděl… Zemědělská a… Udržování … ""        ""          1900-01-01 9999-12-31
#>  3 1012     Zemědělst… Zeměděl… Zemědělská a… Podnikání … "Podn.,r… "Podnikání… 1900-01-01 9999-12-31
#>  4 1013     Zemědělst… Zeměděl… Zemědělská a… Genetický … "Genet.p… "Genetický… 1900-01-01 9999-12-31
#>  5 1014     Zemědělst… Zeměděl… Zemědělská a… Ozdravován… "Ozdrav.… "Ozdrav.ho… 1900-01-01 9999-12-31
#>  6 1019     Zemědělst… Zeměděl… Zemědělská a… Ostatní ze… "Ost.zem… "Ostatní z… 1900-01-01 9999-12-31
#>  7 1021     Zemědělst… Zeměděl… Regulace zem… Organizace… "Regul.t… "Regulace … 1900-01-01 9999-12-31
#>  8 1022     Zemědělst… Zeměděl… Regulace zem… Organizace… "Regul.t… "Reg.trhu … 1900-01-01 9999-12-31
#>  9 1023     Zemědělst… Zeměděl… Regulace zem… Organizace… "Regul.t… "Organizac… 1900-01-01 9999-12-31
#> 10 1024     Zemědělst… Zeměděl… Regulace zem… Organizace… "Regul.t… "Reg.trhu … 1900-01-01 9999-12-31
#> # … with 540 more rows
```

This contains all codes for this codelist, some of which are not valid
for the time period of our core data. The function `add_codelist()`
resolves this automatically.

As you can see below, you can

  - add multiple codelists in one pipe
  - add a codelist without downloading it first - just pass its ID to
    the function instead of an object.

<!-- end list -->

``` r
local_budgets %>% 
  add_codelist(functional_categories) %>% 
  add_codelist("polozka")
#> Joining, by = "paragraf"Joining, by = "paragraf"Building URL for codelist polozka - Rozpočtová položka
#> Downloading codelist data
#> Processing codelist data
#> Joining, by = "polozka"Joining, by = "polozka"
#> # A tibble: 1,245,418 x 36
#>    vykaz vtab  per_yr per_m ucjed ico   kraj  nuts  `0CI_TYPE` paragraf polozka ZU_ROZSCH ZU_ROZPZM
#>    <chr> <chr> <chr>  <chr> <chr> <chr> <chr> <chr>      <dbl> <chr>    <chr>       <dbl>     <dbl>
#>  1 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 3412     6341            0 25313145.
#>  2 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5011     13012000 14252023 
#>  3 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5021        90000    90000 
#>  4 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5024      3288000  2140497 
#>  5 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5031      3253000  3535818 
#>  6 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5032      1172000  1283013 
#>  7 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5038        56000    60542.
#>  8 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5042            0    46580.
#>  9 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5133         1000     1000 
#> 10 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5136        14000    14000 
#> # … with 1,245,408 more rows, and 23 more variables: ZU_ROZKZ <dbl>, period_vykaz <date>,
#> #   skupina <chr>, oddil <chr>, pododdil <chr>, functional_categories_nazev <chr>,
#> #   functional_categories_kr_nazev <chr>, functional_categories_str_nazev <chr>,
#> #   functional_categories_start_date <date>, functional_categories_end_date <date>,
#> #   polozka_start_date <date>, polozka_end_date <date>, druh <chr>, trida <chr>, seskupeni <chr>,
#> #   podseskupeni <chr>, polozka_nazev <chr>, polozka_kr_nazev <chr>, polozka_str_nazev <chr>,
#> #   kon_pol <lgl>, kon_okr <lgl>, kon_kraj <lgl>, kon_rep <lgl>
```

Download a whole “výkaz” (dataset):

``` r
get_dataset("finm") # dataset ID, see `sp_datasets`
#> Building URL for dataset `finm`: FIN 2-12 M - Plnění rozpočtu MŘO, 2018-12
#> http://monitor.statnipokladna.cz/data/2018_12_Data_CSUIS_FINM.zip
#> Get the dataset documentation at http://monitor.statnipokladna.cz/data/struktura/finm.xlsx
#> Files already in /var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp2h9tls/statnipokladna/finm, not downloading. Set `force_redownload` to TRUE if needed.
#> [1] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp2h9tls/statnipokladna/finm/FINM201_2018012.csv"
#> [2] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp2h9tls/statnipokladna/finm/FINM202_2018012.csv"
#> [3] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp2h9tls/statnipokladna/finm/FINM203_2018012.csv"
#> [4] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp2h9tls/statnipokladna/finm/FINM204_2018012.csv"
#> [5] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp2h9tls/statnipokladna/finm/FINM205_2018012.csv"
#> [6] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp2h9tls/statnipokladna/finm/FINM207_2018012.csv"
```

and look at its documentation:

``` r
statnipokladna::get_dataset_doc("finm")
#> Getting dataset documentation from http://monitor.statnipokladna.cz/data/struktura/finm.xlsx
#> File downloaded to ./finm.xlsx.
```

## Background information

Note that while the package provides a bridge from complicated data
dumps to a clean data structure, you still need quite a bit of domain
knowledge to be able analyse the data safely.

See the [“How the data
works”](https://petrbouchal.github.io/statnipokladna/articles/how-the-data-works.html)
vignette (in Czech only, the terminology is impossible to translate) for
an overview of the structure of the data on which this package draws.
This also contains some notes useful for interpreting the data.

A basic glossary of some of the terms used in the data sets is at
<http://monitor.statnipokladna.cz/2019/metodika/>.
