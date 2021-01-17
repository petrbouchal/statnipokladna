# hopefully to be lopped of into another package {purrrow}

marrow_dir <- function(.x, .f, ..., .path, .partitioning = c(), .format = "parquet") {
  arrow_temp_dirs <- file.path(tempdir(),
                               stringi::stri_rand_strings(length(.x), length = 20))

  purrr::walk2(.x, arrow_temp_dirs, ~{
    ft <- .f(.x)
    arrow::write_dataset(ft, path = .y, format = "parquet")
  })

  arrow_datasets <- purrr::map(arrow_temp_dirs, arrow::open_dataset, format = "parquet")
  arrow_combined <- arrow::open_dataset(arrow_datasets)
  arrow::write_dataset(arrow_combined, path = .path,
                       partitioning = .partitioning, format = .format)
  return(.path)
}


marrow_ds <- function(.x, .f, ...,  .arrow_path) {

}

marrow2_dir <- function(.x, .y, .f, ..., .path, .partitioning = c(), .format = "parquet") {
  arrow_temp_dirs <- file.path(tempdir(),
                               stringi::stri_rand_strings(length(.x), length = 20))

  purrr::pwalk(.l = list(.x, .y, arrow_temp_dirs), ~{
    ft <- .f(..1, ..2)
    stopifnot(is.data.frame(ft))
    arrow::write_dataset(ft, path = ..3, format = "parquet")
  })

  arrow_datasets <- purrr::map(arrow_temp_dirs, arrow::open_dataset, format = "parquet")
  arrow_combined <- arrow::open_dataset(arrow_datasets)
  arrow::write_dataset(arrow_combined, path = .path,
                       partitioning = .partitioning, format = .format)
  return(.path)
}
