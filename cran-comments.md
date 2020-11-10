## New minor version

This version includes several minor improvements and bug fixes and a single new feature: an option can be set to store data dumps downloaded from the external source in a custom directory so as to avoid redownloading them later. By default, `tempdir()` is used and the user has to actively set a parameter or option for the package to store any data outside working or temporary directories.

## Test environments

* local MacOS installation, R 4.0.3
* win-builder, R 4.0.3
* win (devel on r-hub)
* fedora-clang gfortran (devel on r-hub)

## R CMD check results

0 errors | 0 warnings | 1 notes on win-devel:

Found the following (possibly) invalid URLs:
  URL: data.gov.cz
    From: README.md
    Status: Error
    Message: libcurl error code 35:
      	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).

## Reverse dependencies

The package has no reverse dependencies.
