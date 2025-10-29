source("scripts/00_setup.R")
df <- readr::read_csv("data/processed/clean.csv", show_col_types = FALSE)
num <- dplyr::select(df, where(is.numeric))
stats <- tibble::tibble(
  variable = names(num),
  skew = sapply(num, e1071::skewness, na.rm = TRUE),
  kurt = sapply(num, e1071::kurtosis, na.rm = TRUE)
)
readr::write_csv(stats, "data/processed/skew_kurtosis.csv")
for (nm in names(num)) {
  png(paste0("outputs/plots/qq_", nm, ".png"), 700, 500)
  qqnorm(num[[nm]], main = paste("QQ:", nm))
  qqline(num[[nm]], col = 2)
  dev.off()
}
message("Diagnostics saved")
