source("scripts/00_setup.R")

df <- readr::read_csv("data/processed/clean.csv", show_col_types = FALSE)

target_cols <- c("fatals", "persons")
missing_cols <- setdiff(target_cols, names(df))
if (length(missing_cols)) {
  stop("Could not locate required columns: ", paste(missing_cols, collapse = ", "))
}

selected <- dplyr::select(df, dplyr::all_of(target_cols))

stats <- tibble::tibble(
  variable = names(selected),
  mean = purrr::map_dbl(selected, ~ mean(.x, na.rm = TRUE)),
  median = purrr::map_dbl(selected, ~ stats::median(.x, na.rm = TRUE)),
  variance = purrr::map_dbl(selected, ~ stats::var(.x, na.rm = TRUE)),
  std_dev = purrr::map_dbl(selected, ~ stats::sd(.x, na.rm = TRUE)),
  trimmed_mean_10 = purrr::map_dbl(selected, ~ mean(.x, trim = 0.1, na.rm = TRUE))
)

readr::write_csv(stats, "data/processed/summary_stats.csv")
message("Summary statistics written")
