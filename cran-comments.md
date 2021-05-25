## New minor version

This version includes several improvements and bug fixes and fixes an issue flagged up by CRAN around accessing remote resources.

## Test environments

* local MacOS installation, R 4.0.4
* win-builder, R 4.1
* fedora-clang gfortran (devel on r-hub)
* Ubuntu (release on r-hub)
* Win (release on r-hub)

## R CMD check results

0 errors | 0 warnings | 2 notes on win-devel:

Found the following (possibly) invalid URLs:
  Namespace in Imports field not imported from: 'tidyselect'
  All declared Imports should be used.
  
Response from maintainer: tidyselect is in fact used via an import in codelists.R (`utils::globalVariables("where")`).

Note: found 52 marked UTF-8 strings

Response from maintainer: this is a result of non-ASCII characters in a data object which are needed to align data descriptors inside the package with those assigned by the data provider in the Czech locale.

## Reverse dependencies

The package has no reverse dependencies.
