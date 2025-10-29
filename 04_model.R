source("scripts/00_setup.R")
df <- readr::read_csv("data/processed/clean.csv", show_col_types = FALSE)
y <- intersect(c("fatals","deaths","persons","injuries","y"), names(df))[1]
if (!is.na(y) && nzchar(y)) {
  preds <- setdiff(names(dplyr::select(df, where(is.numeric))), y)
  if (length(preds) >= 2) {
    f <- as.formula(paste(y, "~", paste(head(preds, 2), collapse = " + ")))
    fit <- lm(f, df)
    broom::tidy(fit) |> readr::write_csv("data/processed/model_coef.csv")
    broom::glance(fit) |> readr::write_csv("data/processed/model_glance.csv")
    p <- ggplot(tibble::tibble(fitted = fitted(fit), resid = resid(fit)), aes(fitted, resid)) +
      geom_point(alpha = 0.5) +
      geom_hline(yintercept = 0, lty = 2)
    ggsave("outputs/plots/residuals_vs_fitted.png", p, width = 6, height = 4, dpi = 120)
    writeLines(paste("Fitted linear:", deparse(f)), "data/processed/model_NOTE.txt")
  } else {
    writeLines("Not enough numeric predictors for demo model.", "data/processed/model_NOTE.txt")
  }
} else {
  writeLines("No obvious numeric target; skipping demo model.", "data/processed/model_NOTE.txt")
}
message("Model step done")
