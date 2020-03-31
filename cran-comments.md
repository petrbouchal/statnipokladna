## Resubmission no. 3

This is a resubmission following a previous resubmission for this packages.

Corrections made:

- replaced T and F with TRUE and FALSE in whole package
- replaced \dontrun{} with \donttest{} in all examples

## Test environments

* local R installation on MacOS, R 3.6.3
* ubuntu 16.04 (on travis-ci and r-hub), R 3.6.3
* fedora-clang (devel on r-hub)
* win-builder on r-hub (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

CRAN-type check on r-hub also adds a note when "checking CRAN incoming feasibility":

Possibly mis-spelled words in DESCRIPTION:
     St�tn� (11:40)
     pokladna (11:47)
     
This is a reference to the data source to which the package provides access. If possible, I would like to keep it including the non-ASCII characters (the file has UTF-8 encoding). This will aid discoverability.

## Reverse dependencies

The package has no reverse dependencies.
