## New patch version

This is an update with a quick fix responding to a change in the URL scheme on the publisher's website.
It also contains several other minor improvements.

## Test environments

* local R installation on MacOS, R 3.6.3
* ubuntu 16.04 (on travis-ci and r-hub), R 3.6.3
* fedora-clang (devel on r-hub)
* win-builder on r-hub (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

Possibly mis-spelled words in DESCRIPTION:
     St�tn� (11:40)
     pokladna (11:47)
     
This is a reference to the data source to which the package provides access. If possible, I would like to keep it including the non-ASCII characters (the file has UTF-8 encoding). This will aid discoverability.

## Reverse dependencies

The package has no reverse dependencies.
