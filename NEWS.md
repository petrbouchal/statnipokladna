# statnipokladna (development version)

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
