# statnipokladna 0.7.0

## BREAKING CHANGES

* `sp_get_dataset()` no longer unzips the downloaded archive. It therefore returns the path to the downloaded zip file, not to the unzipped files. This is needed to support a more modular workflow (see below and `vignette("workflow", package = "statnipokladna")`).
* The `year` param in `sp_get_table()` and `sp_get_dataset()` now has no default. This is more sensible when there is no easy way to determine the latest available table/dataset and in any case better for reproducibility. The new `sp_get_dataset_url()` also has this updated behaviour.
* the columns signifying the time period of the result of `sp_get_table()` have been renamed for more clarity and consistency:
  - `period_vykaz` to `vykaz_date` 
  - `per_m` to `vykaz_month` 
  - `per_yr` to `vykaz_year` 
* `sp_add_codelist()` no longer creates messy column names in the form of `[codelist name]_nazev_nazev`

## New features

* the core functions have been rewritten into a more modular architecture and their constituent modules exported. This allows more fine-grained control over workflows using lower-level functions to accommodate caching and reproducibility e.g. via {targets} or {drake}.
  * sp_get_[dataset|table|codelist] are now effectively wrappers around several lower-level functions
  * those previously using these core functions should see no change except for one breaking change in `sp_get_dataset()` (see above).
  * the lower-level functions enable step-by-step workflows with transparency of intermediate steps (URLs, downloaded ZIP archives, pointers to specific CSV files, etc.) See `vignette("workflow", package = "statnipokladna")`

## Improvements

* better error messages around mismatches between table ID and file in archive
* more informative errors when online files are not available
* improve date parsing in `sp_get_codelist()` to handle inconsistent date formats in some codelists
* faster loading of tables from previously downloaded and unzipped datasets
* empty strings in codelists are now returned as NA
* more columns are now properly named in the output so they can be linked to codelists, incl. columns related to programme spend and rifngfenced ('purpose'-marked) spend
* `rozprog` (programme spend ID) as well as `nastroj` and `nastrojanal` codelists are now recognised

## Bug fixes

* when setting `dest_dir` in `sp_get_*()` functions, files are now put into the right directory even if `dest_dir` does not contain a trailing slash
* `sp_get_dataset_doc()` now creates `dest_dir` if it does not exist, as advertised in the documentation 
* upgrade dplyr dependency to aviod mysterious "unused argument" error in codelist functions
* balance sheets of city districts can now be loaded for all years where they are available
* examples are now safe to run on CRAN

# statnipokladna 0.6.0

## Improvements

* directory for downloading raw data and documentation in all `sp_get_*()` functions can now be set in the option `statnipokladna.dest_dir`. Set this at the beginning of your script or once per session, or in your `.Rprofile` (e.g. `setOption(statnipokladna.dest_dir = "~/sp_data")` for cross-session storage or `setOption(statnipokladna.dest_dir = "sp_data")` for storing all data downloaded by statnipokladna in an `sp_data` subfolder of the current project.)
* downloaded data is no longer nested in a `statnipokladna` directory inside `dest_dir`.
* minor improvements and updates in vignettes.
* properly included the tibble package to ensure correct print methods for output tibbles
* `sp_get_dataset()` and `sp_get_dataset_doc()` now return their outputs invisibly and provide better messages

## Bug fixes

* adapt `sp_get_table()` to a new structure of some data dumps on the part of the data provider
* `sp_get_codelist()` no longer issues a tibble-related warning
* `sp_get_codelist()` now parses all dates correctly

## Deprecations

Functions with names not starting with `sp_` are deprecated and emit a warning. Use equivalent functions named `sp_*`.

# statnipokladna 0.5.7

## Bug fixes

* adapt to new URL scheme for data exports at statnipokladna.cz, fixes #74
* simple codelists with no `*_date` columns now work in `sp_add_codelist()`, fixes #66

## Minor improvements

* `sp_add_codelist()` returns a message if joining by multiple columns
* `sp_add_codelist()` gains a `by` parameter

## Documentation updates

* fixed all outdated links to statnipokladna.cz
* nicer vignette index entries
* vignette and functions now document working with multiple time periods, which differs between datasets/tables
* several clarifications on how to use codelists

# statnipokladna 0.5.6

* CRAN fix: replace \dontrun{} with \donttest{} and T/F with TRUE/FALSE
* updated examples so they do not use deprecated functions
* fixed bug in sp_get_table() - missing `usethis::`
* changed default in `sp_get_dataset()`
* update examples so nothing breaks
* fixed bug in sp_add_codelist introduced when fixing CRAN note


# statnipokladna 0.5.5

## Fixes for CRAN

- fix last URL in README by making it absolute

# statnipokladna 0.5.4

## Fixes for CRAN

- added URL into Description field in DESCRIPTION
- fixed URLs in README

# statnipokladna 0.5.3

## Changes in documentation

- There is now a basic Get Started vignette, which is complementary to the README and contains basic background to the data
- the original Czech data vignette was renamed to "how-the-data-works-cz.Rmd/html" and the data gotchas part was hived off into "data-issues-cz.Rmd/html"

## Bug fixes and minor improvements

* deprecation warnings now show correct package name
* added informative error messages when internet is unavailable
* `sp_get_dataset_doc()` now checks that dataset exists before pointing/downloading.
* updated README with a bit more detail

## Prepare for CRAN release

* fixed dplyr-related NOTE on unavailable objects
* added CRAN notes
* updated LICENSE to work for CRAN
* added cran-comments.md

# statnipokladna 0.5.2

* all exported functions renamed to `sp_*` to avoid conflicts with other packages and for better discoverability via autocomplete; original functions are soft-deprecated.

# statnipokladna 0.5.1

## Improvements

* `get_table()` now provides access to changes-in-equity data (přehled o změnách vlastního kapitálu, use `"changes-in-equity"` table id) and cash flow statements (přehled o peněžních tocích, use `"cash-flow"` id)

# statnipokladna 0.5.0

## New features

* all functions downloading data now have `dest_dir` parameter which allows you to store the downloaded files anywhere, not just in temp dir; this in effect enables cross-session storage and avoids unnecessary redownloads

## Improvements

* adapt month parameter check to upgrade in the published data which now contains monthly releases for some reports
* argument `force_redownload` in `get_[table|dataset|codelist]()` renamed to `redownload`
* improved UI: messaging and guidance

# statnipokladna 0.4.2

* added examples to documentation
* `get_table()` now fully documents the columns in its output, see `help("get_table")`

# statnipokladna 0.4.1

* fixed bug where `get_codelist()` did not create the right temp directory

# statnipokladna 0.4.0

## Breaking changes

* value columns (previously starting with ZU_) now renamed to human-readable English names

## New features

* all key tables, except state funds (FINSF) now included in sp_tables and thus available in `get_table()`

## Improvements

* improved documentation - added details to explain what each function assumes and does
* updated README
* fixed issue so package now also handles pre-2014 data
* minor improvement to messages
* speed improvement to data processing by `get_table()` when `ico` is set
* codelists downloaded by `*_codelist()` are now stored in tempdir

# statnipokladna 0.3.0

## Breaking changes

* the `ico` parameter in `get_table()` now directly follows the key parameters; this will break cases where the function was called with unnamed parameters but will make use more straighforward.
* the `table_id` parameter in `get_table()` is now a human-readable alphanumeric code, not a meaningless number. This will break all uses of get_table but make future use more sensible.

## New features

* new `add_codelist()` function for joining codelists to core financial data
* `get_codelist()` gains `n` parameter, allowing only a limited number of rows of a codelist to be retrieved (to save time)
* `get_codelist()` has a new paramater which opens the online codelist viewer
* filtering using `ico` parameter in `get_table()` now works

## Improvements

* updated table of tables (now covers all key tables, includes notes)
* `get_codelist()` now returns a codelist df ready to join to core data

# statnipokladna 0.2.3

* handle transformation of consolidation columns in `get_codelist()`

# statnipokladna 0.2.2

* bug fix in get_table and get_codelist: corrected logic around druhove trideni (polozka codelist)
* clearer README
* correction in vignette regarding consolidation

# statnipokladna 0.2.1

* improved documentation
* updated README

# statnipokladna 0.2.0

* new `get_dataset()` function for retrieving files 
* new `get_table()` function for loading data - **only experimental**
* new `get_dataset_doc()` function for accessing/downloading official documentation for each dataset
* exported data frames with descriptions of available tables, datasets and codelists
* minor updates to data vignette

# statnipokladna 0.1.1

* added feedback info to data vignette
* fixed typos and testing infrastructure

# statnipokladna 0.1.0

* exported `get_codelist()`
* added vignette explaining the underlying open data

# statnipokladna 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
