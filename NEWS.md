# statnipokladna (development version)

## Breaking changes

* the `ico` parameter in `get_table()` now directly follows the key parameters; this will break cases where the function was called with unnamed parameters but will make use more straighforward.
* the `table_id` parameter in `get_table()` is now a human-readable alphanumeric code, not a meaningless number. This will break all uses of get_table but make future use more sensible.

## New features

* new `add_codelist()` function for joining codelists to core financial data
* `get_codelist()` gains `n` parameter, allowing only a limited number of rows of a codelist to be retrieved (to save time)
* `get_codelist()` has a new paramater which opens the online codelist viewer
* filtering using `ico` parameter in `get_table()` now works

## Improvements

* updated table of tables (now covers all key tables)
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
