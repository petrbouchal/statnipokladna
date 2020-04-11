
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statnipokladna <img src='man/figures/logo.png' align="right" height="138" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/statnipokladna)](https://CRAN.R-project.org/package=statnipokladna)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/statnipokladna)](https://CRAN.R-project.org/package=statnipokladna)
[![CRAN monthly
downloads](https://cranlogs.r-pkg.org/badges/last-month/statnipokladna)](https://CRAN.R-project.org/package=statnipokladna)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Travis build
status](https://travis-ci.org/petrbouchal/statnipokladna.svg?branch=master)](https://travis-ci.org/petrbouchal/statnipokladna)
<!-- badges: end -->

The goal of statnipokladna is to provide programmatic access to open
data from the Státní pokladna system. Státní pokladna is a comprehensive
budgeting, reporting and accounting system for Czech public
organisations. This package provides user-friendly ways to access the
open data from that system available at
<https://monitor.statnipokladna.cz>. The vignettes in the package also
provide an introduction to the underlying data.

## Installation

You can install the released version from CRAN:

``` r
install.packages("statnipokladna")
```

You can install the current development release of statnipokladna from
[GitHub](https://github.com/petrbouchal/statnipokladna) with:

``` r
remotes::install_github("petrbouchal/statnipokladna",
                        build_vignettes = TRUE,
                        ref = github_release())
```

or the latest in-development version with

``` r
remotes::install_github("petrbouchal/statnipokladna",
                         build_vignettes = TRUE)
```

I also keep binaries in a `drat` repo, which you can access by

``` r
install.packages("statnipokladna", repos = "https://petrbouchal.github.io/drat")
```

## Bug reports

Please report bugs at
<https://github.com/petrbouchal/statnipokladna/issues>.

## What this package enables you to do:

  - get cleaned-up, ready to analyse data frames based on open data
    dumps from the public finance database
      - the package draws on the online data and returns a clean data
        frame
      - the resulting data is ready to merge into time series
      - time series is built based on user input
  - do this through a consistent API which supplements some of the
    documentation that is missing from the official endpoints (*still
    subject to some change*)
  - access registers published alongside the data (e.g. lists of public
    organisations with their identifiers and metadata), some of which
    can be useful in other contexts
  - augment the core data with the desired type of register

See the [Get
started](https://petrbouchal.github.io/statnipokladna/articles/statnipokladna.html)
vignette for background on the underlying data.

See also [Background information](#background-information) below.

## How does this compare to the [official analytical interface](http://monitor.statnipokladna.cz/)?

  - no limit on the number of data points
  - no limits on the number of organisations, unlike the official
    interface which forces you to use a filter on some tables
  - different reports (local gov, central gov…) in one place in
    consistent form
  - much faster for analysis (the current version of the online
    interface takes long to render)
  - reproducible\!\!\! While the online interface provides a permanent
    link to your analysis, this must be copied manually and does not
    necessarily provide an easily legible overview of how the analysis
    was produced (filters, columns etc.)
  - no need for the web =\> excel =\> R dance
  - drawback: for some reports, the data is published in different forms
    for different time periods (pre- and post-2015)
  - drawback: consolidation must be done manually

### Future development

the official system has been partially overhauled in February 2020 and I
am trying to find out about which parts of its new API will remain
stable and can be used externally. Depending on the result, some
functionality in this package can be streamlined and some can be added -
e.g. 

  - listing of available releases
  - checking against existing releases and data sets
  - retrieving some previously unpublished data e.g. calculated
    indicators and budget responsibility monitoring

## Getting started

``` r
library(statnipokladna)
```

Get data from a particular part (file) of a dataset (“výkaz”):

``` r
local_budgets <- sp_get_table(table_id = "budget-local", # table ID, see `sp_tables`
                           year = 2019,
                           month = 9)
#> ℹ Building URL for dataset 'finm': FIN 2-12 M - Plnění rozpočtu MŘO, '2019-09'
#> ℹ Get the dataset documentation at 'http:/monitor.statnipokladna.cz/data/struktura/finm.xlsx'
#> ✓ Storing downloaded archive in and extracting to '/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T/Rtmp52TEmf/statnipokladna/finm/2019/09/'
#> ℹ Set dest_dir for more control over downloaded files.
#> ℹ Reading data...
#> ℹ Transforming data...
```

The data is automatically downloaded to a temp directory, so it will be
reused by future calls to `sp_get_table()` made in the same session,
unless you set `force_redownload = TRUE`. You set the `dest_dir`
parameter e.g. to `"."`, a directory will be created in your current
working directory and the data will be downloaded into it so that it can
persist across sessions.

It is a rather raw-looking data frame…

``` r
head(local_budgets)
#> # A tibble: 6 x 15
#>   vykaz vtab  per_yr per_m ucjed ico   kraj  nuts  `0CI_TYPE` paragraf polozka
#>   <chr> <chr> <chr>  <chr> <chr> <chr> <chr> <chr> <chr>      <chr>    <chr>  
#> 1 051   0002… 2019   09    1000… 7508… CZ03  CZ03  3          6409     5364   
#> 2 051   0002… 2019   09    1000… 7508… CZ03  CZ03  3          6409     5909   
#> 3 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1111   
#> 4 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1112   
#> 5 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1113   
#> 6 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1121   
#> # … with 4 more variables: budget_adopted <dbl>, budget_amended <dbl>,
#> #   budget_spending <dbl>, period_vykaz <date>
```

but it has been cleaned up, and can be enriched with any of the metadata
codelists:

``` r
functional_categories <- sp_get_codelist("paragraf")
#> ℹ Building URL for codelist 'paragraf' - Paragraf
#> ✓ Storing codelist in '/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T/Rtmp52TEmf/statnipokladna/'
#> ℹ Set dest_dir for more control over downloaded files.
#> ℹ Processing codelist data
#> Warning: The `x` argument of `as_tibble.matrix()` must have column names if `.name_repair` is omitted as of tibble 2.0.0.
#> Using compatibility `.name_repair`.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_warnings()` to see where this warning was generated.
```

``` r
functional_categories
#> # A tibble: 550 x 9
#>    paragraf skupina oddil pododdil nazev kr_nazev str_nazev start_date
#>    <chr>    <chr>   <chr> <chr>    <chr> <chr>    <chr>     <date>    
#>  1 0000     Příjmy  Příj… Příjmy   Pro … "Pro př… "Pro pří… 1900-01-01
#>  2 1011     Zemědě… Země… Zeměděl… Udrž… ""       ""        1900-01-01
#>  3 1012     Zemědě… Země… Zeměděl… Podn… "Podn.,… "Podniká… 1900-01-01
#>  4 1013     Zemědě… Země… Zeměděl… Gene… "Genet.… "Genetic… 1900-01-01
#>  5 1014     Zemědě… Země… Zeměděl… Ozdr… "Ozdrav… "Ozdrav.… 1900-01-01
#>  6 1019     Zemědě… Země… Zeměděl… Osta… "Ost.ze… "Ostatní… 1900-01-01
#>  7 1021     Zemědě… Země… Regulac… Orga… "Regul.… "Regulac… 1900-01-01
#>  8 1022     Zemědě… Země… Regulac… Orga… "Regul.… "Reg.trh… 1900-01-01
#>  9 1023     Zemědě… Země… Regulac… Orga… "Regul.… "Organiz… 1900-01-01
#> 10 1024     Zemědě… Země… Regulac… Orga… "Regul.… "Reg.trh… 1900-01-01
#> # … with 540 more rows, and 1 more variable: end_date <date>
```

This contains all codes for this codelist, some of which are not valid
for the time period of our core data. The function `add_codelist()`
resolves this automatically.

As you can see below, you can

  - add multiple codelists in one pipe
  - add a codelist without downloading it first - just pass its ID to
    the function as a character instead of an object.

Codelists are also cached, but you have one in your namespace, you can
pass it as an object, provided that it has the right columns.

``` r
local_budgets %>% 
  sp_add_codelist(functional_categories) %>% 
  sp_add_codelist("polozka")
#> Joining, by = "paragraf"
#> Joining, by = "paragraf"
#> ℹ Building URL for codelist 'polozka' - Rozpočtová položka
#> ✓ Storing codelist in '/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T/Rtmp52TEmf/statnipokladna/'
#> ℹ Set dest_dir for more control over downloaded files.
#> ℹ Processing codelist data
#> Joining, by = "polozka"
#> Joining, by = "polozka"
#> # A tibble: 1,189,627 x 36
#>    vykaz vtab  per_yr per_m ucjed ico   kraj  nuts  `0CI_TYPE` paragraf polozka
#>    <chr> <chr> <chr>  <chr> <chr> <chr> <chr> <chr> <chr>      <chr>    <chr>  
#>  1 051   0002… 2019   09    1000… 7508… CZ03  CZ03  3          6409     5364   
#>  2 051   0002… 2019   09    1000… 7508… CZ03  CZ03  3          6409     5909   
#>  3 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1111   
#>  4 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1112   
#>  5 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1113   
#>  6 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1121   
#>  7 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1122   
#>  8 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1211   
#>  9 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1332   
#> 10 051   0001… 2019   09    1000… 0006… CZ010 CZ01… 2          0000     1333   
#> # … with 1,189,617 more rows, and 25 more variables: budget_adopted <dbl>,
#> #   budget_amended <dbl>, budget_spending <dbl>, period_vykaz <date>,
#> #   skupina <chr>, oddil <chr>, pododdil <chr>,
#> #   functional_categories_nazev <chr>, functional_categories_kr_nazev <chr>,
#> #   functional_categories_str_nazev <chr>,
#> #   functional_categories_start_date <date>,
#> #   functional_categories_end_date <date>, polozka_start_date <date>,
#> #   polozka_end_date <date>, druh <chr>, trida <chr>, seskupeni <chr>,
#> #   podseskupeni <chr>, polozka_nazev <chr>, polozka_kr_nazev <chr>,
#> #   polozka_str_nazev <chr>, kon_pol <lgl>, kon_okr <lgl>, kon_kraj <lgl>,
#> #   kon_rep <lgl>
```

Download a whole “výkaz” (dataset/data dump):

``` r
sp_get_dataset("finm") # dataset ID, see `sp_datasets`
#> Warning: Either year or month not set.
#> Using defaults of 2018 and 12.
#> ● Set these values explicitly for reproducibility as the defaults may change in the future
#>   to provide access to the latest data by default.
#> ℹ Building URL for dataset 'finm': FIN 2-12 M - Plnění rozpočtu MŘO, '2018-12'
#> ℹ Get the dataset documentation at 'http:/monitor.statnipokladna.cz/data/struktura/finm.xlsx'
#> ✓ Storing downloaded archive in and extracting to '/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T/Rtmp52TEmf/statnipokladna/finm/2018/12/'
#> ℹ Set dest_dir for more control over downloaded files.
#> [1] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp52TEmf/statnipokladna/finm/2018/12/FINM201_2018012.csv"
#> [2] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp52TEmf/statnipokladna/finm/2018/12/FINM202_2018012.csv"
#> [3] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp52TEmf/statnipokladna/finm/2018/12/FINM203_2018012.csv"
#> [4] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp52TEmf/statnipokladna/finm/2018/12/FINM204_2018012.csv"
#> [5] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp52TEmf/statnipokladna/finm/2018/12/FINM205_2018012.csv"
#> [6] "/var/folders/c8/pj33jytj233g8vr0tw4b2h7m0000gn/T//Rtmp52TEmf/statnipokladna/finm/2018/12/FINM207_2018012.csv"
```

This will put the files in a temp directory.

Then look at its documentation:

``` r
statnipokladna::sp_get_dataset_doc("finm")
#> Getting dataset documentation from http://monitor.statnipokladna.cz/data/struktura/finm.xlsx
#> File downloaded to ./finm.xlsx.
```

You can get details of all the available tables in the `sp_tables` data
frame; for datasets, see `sp_datasets`.

## Background information

Note that while the package provides a bridge from complicated data
dumps to a clean data structure, you still need quite a bit of domain
knowledge to be able analyse the data safely.

See the [“How the data
works”](https://petrbouchal.github.io/statnipokladna/articles/how-the-data-works-cz.html)
vignette (in Czech only, the terminology is impossible to translate) for
an overview of the structure of the data on which this package draws.
This also contains some notes useful for interpreting the data.

A subset of this information is in the [Get
started](https://petrbouchal.github.io/statnipokladna/articles/statnipokladna.html)
vignette.

There is also a log of various data gotchas I discovered, also in Czech
only, stored in the [data issues
vignette](https://petrbouchal.github.io/statnipokladna/articles/data-issues-cz.html).

A basic glossary of some of the terms used in the data sets is at
<http://monitor.statnipokladna.cz/metodika/>.

### Note

Not created or endorsed by the Czech Ministry of Finance, who produce
the data - but they definitely deserve credit for releasing the data and
maintaining the application.

## See also

### R Packages

  - [CzechData](https://github.com/JanCaha/CzechData) by @JanCaha for
    (mainly) geospatial data and data about
  - [RCzechia](https://cran.r-project.org/package=RCzechia) for another
    approach to Czech geospatial data and access to the official public
    geocoder and reverse geocoder
  - [czso](https://github.com/petrbouchal/czso) for access to Czech
    statistical open data
  - [eurostat](https://cran.r-project.org/package=eurostat) for access
    to Eurostat data
  - [OECD](https://cran.r-project.org/package=OECD) for access to OECD
    data, incl. a large amount of financial and economic data

### Other Czech public data

  - [National Open Data Catalogue](https://data.gov.cz)
  - [KNOD](https://github.com/kokes/knod) by Ondřej Kokeš for an
    overview of public data
  - [Hlídač státu](https://hlidacstatu.cz/) by @michalblaha for easy
    (web and API) access to a large suite of transparency-focused
    datasets and their integration (public disclosures of contracts,
    tenders, political contributions…)
  - [CEDR](https://cedr.mfcr.cz/) for a database of public subsidies,
    incl. to public bodies

## Acknowledgments

Thanks to @smallhillcz and the Státní pokladna/Monitor developers and
maintainers for responding to queries and generally keeping the thing
running.

## Contributing

See
[CONTRIBUTING.md](https://github.com/petrbouchal/statnipokladna/blob/master/.github/CONTRIBUTING.md)
for a guide on how to contribute to the project.

Please note that the ‘statnipokladna’ project is released with a
[Contributor Code of
Conduct](https://petrbouchal.github.io/statnipokladna/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
