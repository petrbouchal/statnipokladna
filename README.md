
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
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/petrbouchal/statnipokladna/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/petrbouchal/statnipokladna/actions/workflows/R-CMD-check.yaml)
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
install.packages("statnipokladna", repos = "https://petrbouchal.xyz/drat")
```

## Bug reports

Please report bugs at
<https://github.com/petrbouchal/statnipokladna/issues>.

## What this package enables you to do:

- get cleaned-up, ready to analyse data frames based on open data dumps
  from the public finance database
  - the package draws on the online data and returns a clean data frame
  - the resulting data is ready to merge into time series
  - time series is built based on user input
- do this through a consistent API which supplements some of the
  documentation that is missing from the official endpoints (*still
  subject to some change*)
- access registers published alongside the data (e.g. lists of public
  organisations with their identifiers and metadata), some of which can
  be useful in other contexts
- augment the core data with the desired type of register

See the [Get
started](https://petrbouchal.xyz/statnipokladna/articles/statnipokladna.html)
vignette for background on the underlying data.

See also [Background information](#background-information) below.

## How does this compare to the [official analytical interface](https://monitor.statnipokladna.cz/)?

- no limit on the number of data points
- no limits on the number of organisations, unlike the official
  interface which forces you to use a filter on some tables
- different reports (local gov, central gov…) in one place in consistent
  form
- much faster for analysis (the current version of the online interface
  takes long to render)
- reproducible!!! While the online interface provides a permanent link
  to your analysis, this must be copied manually and does not
  necessarily provide an easily legible overview of how the analysis was
  produced (filters, columns etc.)
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
- retrieving some previously unpublished data e.g. calculated indicators
  and budget responsibility monitoring

## Getting started

``` r
library(statnipokladna)
```

Get data from a particular part (file) of a dataset (“výkaz”):

``` r
local_budgets <- sp_get_table(table_id = "budget-local", # table ID, see `sp_tables`
                           year = 2019,
                           month = 9)
#> ✔ Storing downloaded archive in '/var/folders/fr/6f85xds52pq7g55fpmk4z7f80000gn/T//RtmpoP6RVe/finm/2019/09'
#> • Set `dest_dir` for more control over downloaded files.
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
#> # A tibble: 6 × 15
#>   vykaz vtab   vykaz_year vykaz_month ucjed      ico     kraj  nuts  polozka_typ
#>   <chr> <chr>  <chr>      <chr>       <chr>      <chr>   <chr> <chr> <chr>      
#> 1 051   000200 2019       09          1000003163 750869… CZ03  CZ03  3          
#> 2 051   000200 2019       09          1000003163 750869… CZ03  CZ03  3          
#> 3 051   000100 2019       09          1000017768 000645… CZ010 CZ01… 2          
#> 4 051   000100 2019       09          1000017768 000645… CZ010 CZ01… 2          
#> 5 051   000100 2019       09          1000017768 000645… CZ010 CZ01… 2          
#> 6 051   000100 2019       09          1000017768 000645… CZ010 CZ01… 2          
#> # ℹ 6 more variables: paragraf <chr>, polozka <chr>, budget_adopted <dbl>,
#> #   budget_amended <dbl>, budget_spending <dbl>, vykaz_date <date>
```

but it has been cleaned up, and can be enriched with any of the metadata
codelists:

``` r
functional_categories <- sp_get_codelist("paragraf")
#> ℹ Storing codelist in '/var/folders/fr/6f85xds52pq7g55fpmk4z7f80000gn/T//RtmpoP6RVe'
#> ℹ Set `dest_dir` for more control over downloaded files.
```

``` r
functional_categories
#> # A tibble: 841 × 8
#>    paragraf start_date end_date   nazev          skupina oddil pododdil poznamka
#>    <chr>    <date>     <date>     <chr>          <chr>   <chr> <chr>    <chr>   
#>  1 0000     2010-01-01 9999-12-31 Pro příjmy (t… Příjmy  Příj… Příjmy   Pro pří…
#>  2 1011     2010-01-01 9999-12-31 Udržování výr… Zemědě… Země… Zeměděl… Zeměděl…
#>  3 1012     2010-01-01 9999-12-31 Podnikání a r… Zemědě… Země… Zeměděl… Podniká…
#>  4 1013     2010-01-01 9999-12-31 Genetický pot… Zemědě… Země… Zeměděl… Genetic…
#>  5 1014     2010-01-01 9999-12-31 Ozdravování h… Zemědě… Země… Zeměděl… Ozdrav.…
#>  6 1019     2010-01-01 9999-12-31 Ostatní zeměd… Zemědě… Země… Zeměděl… Ostatní…
#>  7 1021     2010-01-01 9999-12-31 Organizace tr… Zemědě… Země… Regulac… Regulac…
#>  8 1022     2010-01-01 9999-12-31 Organizace tr… Zemědě… Země… Regulac… Org. tr…
#>  9 1023     2010-01-01 9999-12-31 Organizace tr… Zemědě… Země… Regulac… Organiz…
#> 10 1024     2010-01-01 9999-12-31 Organizace tr… Zemědě… Země… Regulac… Reg.trh…
#> # ℹ 831 more rows
```

This contains all codes for this codelist, some of which are not valid
for the time period of our core data. The function `add_codelist()`
resolves this automatically.

As you can see below, you can

- add multiple codelists in one pipe
- add a codelist without downloading it first - just pass its ID to the
  function as a character instead of an object.

Codelists are also cached, but you have one in your namespace, you can
pass it as an object, provided that it has the right columns.

``` r
local_budgets %>% 
  sp_add_codelist(functional_categories) %>% 
  sp_add_codelist("polozka")
#> ℹ Storing codelist in '/var/folders/fr/6f85xds52pq7g55fpmk4z7f80000gn/T//RtmpoP6RVe'
#> ℹ Set `dest_dir` for more control over downloaded files.
#> ℹ Joining on 2 columns: polozka, poznamka.This may indicate a problem with the data.Set `by` if needed.
#> # A tibble: 1,189,627 × 34
#>    vykaz vtab   vykaz_year vykaz_month ucjed      ico    kraj  nuts  polozka_typ
#>    <chr> <chr>  <chr>      <chr>       <chr>      <chr>  <chr> <chr> <chr>      
#>  1 051   000200 2019       09          1000003163 75086… CZ03  CZ03  3          
#>  2 051   000200 2019       09          1000003163 75086… CZ03  CZ03  3          
#>  3 051   000100 2019       09          1000017768 00064… CZ010 CZ01… 2          
#>  4 051   000100 2019       09          1000017768 00064… CZ010 CZ01… 2          
#>  5 051   000100 2019       09          1000017768 00064… CZ010 CZ01… 2          
#>  6 051   000100 2019       09          1000017768 00064… CZ010 CZ01… 2          
#>  7 051   000100 2019       09          1000017768 00064… CZ010 CZ01… 2          
#>  8 051   000100 2019       09          1000017768 00064… CZ010 CZ01… 2          
#>  9 051   000100 2019       09          1000017768 00064… CZ010 CZ01… 2          
#> 10 051   000100 2019       09          1000017768 00064… CZ010 CZ01… 2          
#> # ℹ 1,189,617 more rows
#> # ℹ 25 more variables: paragraf <chr>, polozka <chr>, budget_adopted <dbl>,
#> #   budget_amended <dbl>, budget_spending <dbl>, vykaz_date <date>,
#> #   functional_categories_start_date <date>,
#> #   functional_categories_end_date <date>, functional_categories_nazev <chr>,
#> #   skupina <chr>, oddil <chr>, pododdil <chr>, poznamka <chr>,
#> #   polozka_id <chr>, polozka_start_date <date>, polozka_end_date <date>, …
```

Download a whole “výkaz” (dataset/data dump):

``` r
sp_get_dataset("finm", year = 2019) # dataset ID, see `sp_datasets`
#> ! `month` not set. Using default of 12.
#> ✔ Storing downloaded archive in '/var/folders/fr/6f85xds52pq7g55fpmk4z7f80000gn/T//RtmpoP6RVe/finm/2019/12'
#> • Set `dest_dir` for more control over downloaded files.
```

This will put the files in a temp directory.

Then look at its documentation:

``` r
statnipokladna::sp_get_dataset_doc("finm")
#> ℹ Getting dataset documentation from <https://monitor.statnipokladna.cz/data/struktura/finm.xlsx>
#> ℹ File downloaded to '/var/folders/fr/6f85xds52pq7g55fpmk4z7f80000gn/T//RtmpoP6RVe/finm.xlsx'.
```

You can get details of all the available tables in the `sp_tables` data
frame; for datasets, see `sp_datasets`.

## Workflows and reproducibility

The above examples present a simple all-in-one workflow, which is
concise but can be too opaque when transparency and reproducibility
matter. It is primarity aimed at workflows which prioritise updating
data: every time the script is run, data is redownloaded, unless cached
via the `dest_dir` parameter.

In other situations, the priority might be to keep track of individual
source files as they are downloaded from the data provider, or checking
for changes at the data provider and keeping track of individual URLs
from which the data was downloaded. For these situations, a workflow
composed of lower-level functions is available, offering finer control
of the steps. See the
[workflow](https://petrbouchal.xyz/statnipokladna/articles/workflow.html)
vignette (`vignette("workflow", package = "statnipokladna")`).

## Background information

Note that while the package provides a bridge from complicated data
dumps to a clean data structure, you still need quite a bit of domain
knowledge to be able analyse the data safely.

See the [“How the data
works”](https://petrbouchal.xyz/statnipokladna/articles/how-the-data-works-cz.html)
vignette (in Czech only, the terminology is impossible to translate) for
an overview of the structure of the data on which this package draws.
This also contains some notes useful for interpreting the data.

A subset of this information is in the [Get
started](https://petrbouchal.xyz/statnipokladna/articles/statnipokladna.html)
vignette.

There is also a log of various data gotchas I discovered, also in Czech
only, stored in the [data issues
vignette](https://petrbouchal.xyz/statnipokladna/articles/data-issues-cz.html).

A basic glossary of some of the terms used in the data sets is at
<https://monitor.statnipokladna.cz/metodika/>.

### Note

Not created or endorsed by the Czech Ministry of Finance, who produce
the data - but they definitely deserve credit for releasing the data and
maintaining the application.

## See also

### R Packages

- [CzechData](https://jancaha.github.io/CzechData/) by @JanCaha for
  (mainly) geospatial data about the Czech Republic (both admin.
  boundaries and topology and geography)
- [RCzechia](https://cran.r-project.org/package=RCzechia) for another
  approach to Czech geospatial data and access to the official public
  geocoder and reverse geocoder
- [czso](https://github.com/petrbouchal/czso) for access to Czech
  statistical open data
- [eurostat](https://cran.r-project.org/package=eurostat) for access to
  Eurostat data
- [OECD](https://cran.r-project.org/package=OECD) for access to OECD
  data, incl. a large amount of financial and economic data

### Other Czech public data

- [National Open Data Catalogue](https://data.gov.cz/)
- [KNOD](https://github.com/kokes/knod) by Ondřej Kokeš for an overview
  of public data
- [Hlídač státu](https://www.hlidacstatu.cz/) by @michalblaha for easy
  (web and API) access to a large suite of transparency-focused datasets
  and their integration (public disclosures of contracts, tenders,
  political contributions…)
- CEDR (Centrální registr dotací) for a database of public subsidies,
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
Conduct](https://petrbouchal.xyz/statnipokladna/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
