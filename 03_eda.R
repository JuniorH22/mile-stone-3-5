source("scripts/00_setup.R")
df <- readr::read_csv("data/processed/clean.csv", show_col_types = FALSE)
num <- names(dplyr::select(df, where(is.numeric)))
for (nm in num) {
  p1 <- ggplot(df, aes(x = .data[[nm]])) +
    geom_histogram(bins = 30) +
    labs(title = paste("Hist", nm))
  ggsave(paste0("outputs/plots/hist_", nm, ".png"), p1, width = 6, height = 4, dpi = 120)
  p2 <- ggplot(df, aes(y = .data[[nm]])) +
    geom_boxplot() +
    labs(title = paste("Box", nm))
  ggsave(paste0("outputs/plots/box_", nm, ".png"), p2, width = 4, height = 4, dpi = 120)
}
message("EDA plots saved")
