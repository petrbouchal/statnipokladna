## New minor version

This version includes 

* several minor improvements and bug fixes
* a single new feature: the `sp_get_catalogue()` function drawing on the new SPARQL endpoint made accessible by the data provider at <https://opendata.mfcr.cz/lod/monitor>.
* a new option can be set to store data dumps downloaded from the external source in a custom directory so as to avoid redownloading them later. By default, `tempdir()` is used and the user has to actively set a parameter or option for the package to store any data outside working or temporary directories.

## Test environments

* local R installation on MacOS, R 4.0.2
* ubuntu 16.04 (on travis-ci and r-hub), R 4.0.2
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
