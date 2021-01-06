## Datasets

[x] sp_get_dataset
  should accept multiple time periods, as sp_get_table does now
  should return a vector of paths to ZIP files (not unzip outputs)

- export sp_get_dataset_url

This should be the only breaking change for the user - sp_get_dataset no longer returns

## Tables

- sp_get_table
  [do we need sp_get_table_file to return path to the right local unzipped file?]
  should call sp_get_dataset on its inputs (multiple time periods if exist)
  should then iterate over the output of sp_get_dataset using sp_load_table(), which takes an n >= 1 vector of paths to ZIP files as an input
- New: sp_get_table_path: new function that takes ZIP path and returns CSV path
- New: sp_load_table: new function that takes file path and table ID as input (or just dest_dir? That would be sufficient but would not improve targets-style workflow)

## Codelists

new sp_get_codelist_url fn broken out, takes codelist ID as input and returns URL to XML as output
new sp_get_codelist_file fn broken out of sp_get_codelist, takes codelist ID as input and returns XML file path as output
new sp_load_codelist fn broken out; takes codelist XML file path as input and outputs tibble

sp_get_codelist becomes a wrapper around these two

No changes visible to users of sp_[get|add]_codelist

## Targets workflow

## Architecture/API:

for tables and you can either call the two/three steps sequentially or use a higher-level wrapper

## Documentation

- note in readme and in getting started vignette
- cross refs between functions in See also (if section can be edited) - or better label the lower-level fns with "Fine-grained control workflow"

### vignette for reproducible workflows

- set time periods, don't rely on defaults
- think about whether you need to keep intermediate files: 
- if so, 
  - use sp_get_dataset as input to your make/targets workflow
  - set dest_dir in function call or sp.dest_dir in your project-specific options 
- if not, 
  - still build custom functions that save output of sp_get_table|codelist into file and use the file path in your make/targets workflow - that gives you visibility of the data files on which your analysis depends
  - make sure you do not rerun sp_get_table unnecessarily - that takes time and bandwidth and more importantly can break your results if the source data change 
- think about how you use redownload
