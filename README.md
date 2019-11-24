
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statnipokladna <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/statnipokladna)](https://CRAN.R-project.org/package=statnipokladna)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
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

You can install the latest development release version of statnipokladna
from [GitHub](https://github.com/petrbouchal/statnipokladna) with:

``` r
remotes::install_github("petrbouchal/statnipokladna", ref = github_release())
```

or the current development version with

``` r
remotes::install_github("petrbouchal/statnipokladna")
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
local_budgets <- get_table(table_id = "51100", # table ID, see `sp_tables`
                           year = 2018,
                           month = 12)
#> Building URL for dataset `finm`: FIN 2-12 M - Plnění rozpočtu MŘO
#> http://monitor.statnipokladna.cz/data/2018_12_Data_CSUIS_FINM.zip
#> Get the dataset documentation at http://monitor.statnipokladna.cz/data/struktura/finm.xlsx
#> Storing downloaded archive in and extracting to /var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmplxacUE/statnipokladna/finm
```

It is a rather raw-looking data frame…

``` r
head(local_budgets)
#> # A tibble: 6 x 15
#>   vykaz vtab  per_yr per_m ucjed ico   kraj  nuts  `0CI_TYPE` paragraf polozka
#>   <chr> <chr> <chr>  <chr> <chr> <chr> <chr> <chr>      <dbl> <chr>    <chr>  
#> 1 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 3412     6341   
#> 2 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5011   
#> 3 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5021   
#> 4 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5024   
#> 5 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5031   
#> 6 051   0002… 2018   12    1000… 7508… CZ04  CZ04           3 6174     5032   
#> # … with 4 more variables: ZU_ROZSCH <dbl>, ZU_ROZPZM <dbl>, ZU_ROZKZ <dbl>,
#> #   period_vykaz <date>
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
#>    paragraf skupina oddil pododdil nazev kr_nazev str_nazev start_date
#>    <chr>    <chr>   <chr> <chr>    <chr> <chr>    <chr>     <date>    
#>  1 0000     Příjmy  Příj… Příjmy   Pro … Pro pří… Pro příj… 1900-01-01
#>  2 1011     Zemědě… Země… Zeměděl… Udrž… ""       ""        1900-01-01
#>  3 1012     Zemědě… Země… Zeměděl… Podn… Podn.,r… Podnikán… 1900-01-01
#>  4 1013     Zemědě… Země… Zeměděl… Gene… Genet.p… Genetick… 1900-01-01
#>  5 1014     Zemědě… Země… Zeměděl… Ozdr… Ozdrav.… Ozdrav.h… 1900-01-01
#>  6 1019     Zemědě… Země… Zeměděl… Osta… Ost.zem… Ostatní … 1900-01-01
#>  7 1021     Zemědě… Země… Regulac… Orga… Regul.t… Regulace… 1900-01-01
#>  8 1022     Zemědě… Země… Regulac… Orga… Regul.t… Reg.trhu… 1900-01-01
#>  9 1023     Zemědě… Země… Regulac… Orga… Regul.t… Organiza… 1900-01-01
#> 10 1024     Zemědě… Země… Regulac… Orga… Regul.t… Reg.trhu… 1900-01-01
#> # … with 540 more rows, and 1 more variable: end_date <date>
```

(The functions to safely join these two together is not yet there, so be
careful if doing this manually.)

Download a whole “výkaz” (dataset):

``` r
get_dataset("finm") # dataset ID, see `sp_datasets`
#> Building URL for dataset `finm`: FIN 2-12 M - Plnění rozpočtu MŘO
#> http://monitor.statnipokladna.cz/data/2018_12_Data_CSUIS_FINM.zip
#> Get the dataset documentation at http://monitor.statnipokladna.cz/data/struktura/finm.xlsx
#> Files already in /var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmplxacUE/statnipokladna/finm, not downloading. Set `force_redownload` to TRUE if needed.
#> [1] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmplxacUE/statnipokladna/finm/FINM201_2018012.csv"
#> [2] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmplxacUE/statnipokladna/finm/FINM202_2018012.csv"
#> [3] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmplxacUE/statnipokladna/finm/FINM203_2018012.csv"
#> [4] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmplxacUE/statnipokladna/finm/FINM204_2018012.csv"
#> [5] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmplxacUE/statnipokladna/finm/FINM205_2018012.csv"
#> [6] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmplxacUE/statnipokladna/finm/FINM207_2018012.csv"
```

and look at its documentation:

``` r
statnipokladna::get_dataset_doc("finm")
#> Getting dataset documentation from http://monitor.statnipokladna.cz/data/struktura/finm.xlsx
#> File downloaded to ./finm.xlsx.
```

# Background information

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
