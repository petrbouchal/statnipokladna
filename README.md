
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
        frame (*done*)
      - the resulting data is ready to merge into time series (*done*)
      - time series is built based on user input (*done*)
  - do this through a consistent API which supplements some of the
    documentation that is missing from the official endpoints (*partly
    available*)
  - access registers published alongside the data (e.g. lists of public
    organisations with their identifiers and metadata), some of which
    can be useful in other contexts (*done*)
  - augment the core data with the desired type of register (*done*)

## How does this compare to the [official analytical interface](http://monitor.statnipokladna.cz/)?

  - no limit on the number of data points
  - no limits on the number of organisations, unlike the official
    interface which forces you to use a filter on some tables
  - different reports (local gov, central gov…) in one place in
    consistent form
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
                           year = 2019,
                           month = 9)
#> Building URL for dataset `finm`: FIN 2-12 M - Plnění rozpočtu MŘO, 2019-09
#> http://monitor.statnipokladna.cz/data/2019_09_Data_CSUIS_FINM.zip
#> Get the dataset documentation at http://monitor.statnipokladna.cz/data/struktura/finm.xlsx
#> Storing downloaded archive in and extracting to /var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/finm/2019/09
#> Reading data...
#> Transforming data...
```

The data is automatically downloaded to a cache directory, so it will be
reused by future calls to `get_table()` made in the same session, unless
you set `force_redownload = T`.

It is a rather raw-looking data frame…

``` r
head(local_budgets)
#> # A tibble: 6 x 15
#>   vykaz vtab  per_yr per_m ucjed ico   kraj  nuts  `0CI_TYPE` paragraf polozka ZU_ROZSCH ZU_ROZPZM
#>   <chr> <chr> <chr>  <chr> <chr> <chr> <chr> <chr> <chr>      <chr>    <chr>       <dbl>     <dbl>
#> 1 051   0002… 2019   09    1000… 7508… CZ03  CZ03  3          6409     5364      0.        1.57e 5
#> 2 051   0002… 2019   09    1000… 7508… CZ03  CZ03  3          6409     5909      0.        1.83e 6
#> 3 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1111      1.51e10   1.51e10
#> 4 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1112      3.10e 8   3.10e 8
#> 5 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1113      1.04e 9   1.04e 9
#> 6 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1121      1.21e10   1.21e10
#> # … with 2 more variables: ZU_ROZKZ <dbl>, period_vykaz <date>
```

…but it has been cleaned up, and can be enriched with any of the
metadata codelists:

``` r
functional_categories <- get_codelist("paragraf")
#> Codelist file already in /var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/, not downloading. Set `force_redownload` to TRUE if needed.
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
#> Joining, by = "paragraf"Joining, by = "paragraf"Codelist file already in /var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/, not downloading. Set `force_redownload` to TRUE if needed.
#> Processing codelist data
#> Joining, by = "polozka"Joining, by = "polozka"
#> # A tibble: 1,189,627 x 36
#>    vykaz vtab  per_yr per_m ucjed ico   kraj  nuts  `0CI_TYPE` paragraf polozka ZU_ROZSCH ZU_ROZPZM
#>    <chr> <chr> <chr>  <chr> <chr> <chr> <chr> <chr> <chr>      <chr>    <chr>       <dbl>     <dbl>
#>  1 051   0002… 2019   09    1000… 7508… CZ03  CZ03  3          6409     5364      0.        1.57e 5
#>  2 051   0002… 2019   09    1000… 7508… CZ03  CZ03  3          6409     5909      0.        1.83e 6
#>  3 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1111      1.51e10   1.51e10
#>  4 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1112      3.10e 8   3.10e 8
#>  5 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1113      1.04e 9   1.04e 9
#>  6 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1121      1.21e10   1.21e10
#>  7 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1122      4.00e 8   1.48e 9
#>  8 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1211      2.80e10   2.80e10
#>  9 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1332      4.55e 4   4.55e 4
#> 10 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1333      0.        0.     
#> # … with 1,189,617 more rows, and 23 more variables: ZU_ROZKZ <dbl>, period_vykaz <date>,
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
#> Files already in /var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/finm/2018/12, not downloading. Set `force_redownload` to TRUE if needed.
#> [1] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/finm/2018/12/FINM201_2018012.csv"
#> [2] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/finm/2018/12/FINM202_2018012.csv"
#> [3] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/finm/2018/12/FINM203_2018012.csv"
#> [4] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/finm/2018/12/FINM204_2018012.csv"
#> [5] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/finm/2018/12/FINM205_2018012.csv"
#> [6] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//RtmpLrwx4P/statnipokladna/finm/2018/12/FINM207_2018012.csv"
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
