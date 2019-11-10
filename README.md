
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

# What you will be able to do when this is done

  - get cleaned-up, ready to analyse data frames based on open data
    dumps from the public finance database (*in development*)
      - the package draws on the online data and returns a clean data
        frame
      - the resulting data is ready to merge into time series
  - do this through a consistent API which supplements some of the
    documentation that is missing from the official endpoints (*in
    development*)
  - access registers published alongside the data (e.g. lists of public
    organisations with their identifiers and metadata), some of which
    can be useful in other contexts (*ready*)
  - augment the core data with the desired type of register (*in
    development*)

## What will this do compared to the [official analytical interface](http://monitor.statnipokladna.cz/)?

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

Note the official analysis GUI is due to be overhauled in November 2018.

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
