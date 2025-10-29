source("scripts/00_setup.R")
df <- readr::read_csv("data/processed/raw_loaded.csv", show_col_types = FALSE)
num <- names(dplyr::select(df, where(is.numeric)))
for (nm in num) {
  if (any(is.na(df[[nm]]))) {
    df[[nm]][is.na(df[[nm]])] <- median(df[[nm]], na.rm = TRUE)
  }
}
flag <- function(x) {
  if (!is.numeric(x)) return(rep(FALSE, length(x)))
  q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
  i <- q[2] - q[1]
  x < q[1] - 1.5 * i | x > q[2] + 1.5 * i
}
flags <- tibble::as_tibble(purrr::map(df, flag))
df$.any_outlier <- apply(as.data.frame(flags), 1, any)
readr::write_csv(
  tibble::tibble(
    variable = num,
    n_flagged = sapply(df[num], function(v) sum(flag(v)))
  ),
  "data/processed/outlier_counts_by_column.csv"
)
readr::write_csv(df, "data/processed/clean.csv")
writeLines(
  "Median-imputed numeric NAs; outliers kept but flagged via IQR rule in .any_outlier.",
  "data/processed/clean_PROVENANCE.txt"
)
message("Clean saved")
