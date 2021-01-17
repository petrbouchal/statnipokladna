# possibly pointless function, unless we want to enable writing CSV arrow datasets without arrow package

write_to_arrow_dataset <- function(df, dir, ..., hive = TRUE, format = c("parquet", "csv")) {
  usethis::ui_info("Writing files...")
  format <- match.arg(format)

  # hp <- function(x) paste0(as.character(substitute(x)), "=", x)

  # year <- 2012
  # month <- 12
  # year_hp <- hp(year)
  # month_hp <- hp(month)
  # l <- list(year_hp, month_hp)

  df_split <- dplyr::group_split(df, ...)

  dots <- rlang::quos(...)

  make_path <- function(x) do.call(file.path, x)

  pths <- purrr::map_chr(df_split, function(x) {
    df_sub <- dplyr::distinct(x, ...) %>%
      dplyr::relocate(...)

    df_for_writing <- x %>% dplyr::select(-c(UQS(dots)))

    add_dirs <- purrr::map(names(df_sub), ~ifelse(hive,
                                                  paste0(., "=", df_sub[[.]]),
                                                  df_sub[[.]]))
    final_dirs <- append(dir, add_dirs)
    dir_path <- make_path(final_dirs)
    dir.create(dir_path, recursive = T, showWarnings = F)
    if(format == "csv") {
      data_file_path <- file.path(dir_path, "file.csv")
      readr::write_csv(df_for_writing, data_file_path)
    } else if (format == "parquet") {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        usethis::ui_stop("Package 'arrow' needed for the 'parquet' format. Please install it or use the 'csv' format.")
        }

      data_file_path <- file.path(dir_path, "file.parquet")
      arrow::write_parquet(df_for_writing, data_file_path)
    } else {stop()}
    return(data_file_path)
  })
  return(pths)
}

# write_to_arrow_dataset(mtcars, "crs", gear, cyl, hive = T)
# write_to_arrow_dataset(mtcars, "crscsv", gear, cyl, hive = T, format = "csv")

#' @importFrom rlang UQS

sp_save_one_table <- function(table_id, year, month, ..., dest_dir, .ico = NULL,
                              redownload, dir, format, hive) {

  hp <- function(x) paste0(as.character(substitute(x)), "=", x)

  ttt <- sp_get_table(table_id, year, month, dest_dir = dest_dir, ico = .ico)

  write_to_arrow_dataset(ttt, dir, .data$per_yr, .data$per_m, ..., hive = hive, format = format)
}

sp_create_arrow_dataset <- function(table_id, year, month, ..., dir = table_id,
                                    format = c("parquet", "csv"), hive = T,
                                    dest_dir = NULL, redownload = FALSE) {
  if(is.null(dest_dir)) dest_dir <- getOption("statnipokladna.dest_dir",
                                              default = tempdir())
  format <- match.arg(format)

  years_months <- tidyr::crossing(y = year, m = month)

  paths <- purrr::map2(years_months$y, years_months$m,
              function(x, y) {
                sp_save_one_table(table_id, x, y, dir = dir,
                                  ...,
                                  hive = hive,
                                  format = format,
                                  dest_dir = dest_dir,
                                  redownload = redownload)
              })
  return(unlist(paths))
}

sp_write_arrow <- function(table_id, year, month = 12, ico = NULL,
                           redownload = FALSE, dest_dir = NULL,
                           path, partitioning = c("vykaz_period", "vykaz_year", "vykaz_month"),
                           format = c("parquet", "csv"),
                           result = c("dataset", "dir", "files")) {
  format = match.arg(format)
  result = match.arg(result)

  yearmon <- tidyr::crossing(yr = year, m = month)
  print(yearmon)

  arrow_dir <- marrow2_dir(.x = yearmon$yr, .y = yearmon$m,
              .f = function(x, y) {sp_get_table(table_id = table_id, year = x,
                                                month = y, ico = ico,
                                                dest_dir = dest_dir,
                                                redownload = redownload)},
              .path = path, .partitioning = partitioning, .format = format)
  rslt <- switch (result,
    # this switch should be handled by calling different marrow_* functions
    # once these are in place

    # except for this option where we might want to input
    # sp-specific partitioning params - or perhaps issue a message
    # pointing the user to sp_read_arrow()
    dataset = arrow::open_dataset(arrow_dir),
    dir = arrow_dir,
    files = list.files(arrow_dir, recursive = T, full.names = T),
  )
}
