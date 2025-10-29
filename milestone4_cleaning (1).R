# Milestone 4 — Outliers & Missing Data (FARS 2022)
# Author: Junior Hernandez
# Date: 2025-10-24
# Notes: This script performs missing-data handling and outlier treatment,
#        then regenerates the plots from Milestone 3 on both RAW and CLEANED datasets.
#        Keep both versions for transparency on your project page.

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
})

# ---- User-configurable paths -------------------------------------------------
# Point this to your local folder containing the FARS 2022 Accidents file.
# Download from NHTSA if needed (Final Release). Common filenames included below.
data_dir <- "data/fars2022"
candidates <- c(
  file.path(data_dir, "accident.csv"),
  file.path(data_dir, "ACCIDENT.CSV"),
  file.path(data_dir, "accident.csv.gz"),
  file.path(data_dir, "ACCIDENT.CSV.GZ")
)

first_existing <- function(paths) {
  for (p in paths) if (file.exists(p)) return(p)
  stop("Could not find accident.csv in ", data_dir,
       ". Place the FARS 2022 ACCIDENT CSV there or update 'data_dir'.")
}

accident_path <- first_existing(candidates)

# ---- Read & Select Columns ---------------------------------------------------
df_raw <- readr::read_csv(accident_path, show_col_types = FALSE)

# Keep columns used in Milestone 1/3
keep_cols <- c("STATE", "WEATHER", "HOUR", "PERSONS", "FATALS")
missing_cols <- setdiff(keep_cols, names(df_raw))
if (length(missing_cols) > 0) {
  stop("The following required columns are missing from the file: ",
       paste(missing_cols, collapse = ", "),
       "\nCheck that you are using the 'ACCIDENT' file for 2022 Final Release.")
}

df <- df_raw %>% select(all_of(keep_cols))

# ---- Recode "unknown/not reported" to NA ------------------------------------
# FARS commonly uses 98/99 for unknown. HOUR=99 for unknown; WEATHER 98/99.
df <- df %>% mutate(
  WEATHER = as.integer(WEATHER),
  HOUR    = as.integer(HOUR),
  PERSONS = as.integer(PERSONS),
  FATALS  = as.integer(FATALS),
  WEATHER = ifelse(WEATHER %in% c(98L, 99L), NA_integer_, WEATHER),
  HOUR    = ifelse(HOUR == 99L, NA_integer_, HOUR)
)

# Map WEATHER codes to labels (subset of codebook)
weather_map <- c(
  `1`  = "Clear/No Adverse",
  `2`  = "Rain",
  `3`  = "Sleet/Hail/Freezing Rain",
  `4`  = "Snow",
  `5`  = "Fog/Smog/Smoke",
  `6`  = "Severe Crosswinds",
  `7`  = "Blowing Sand/Soil/Dirt",
  `8`  = "Other",
  `10` = "Cloudy",
  `11` = "Blowing Snow",
  `12` = "Freezing Drizzle",
  `98` = "Not Reported",
  `99` = "Unknown"
)

df <- df %>% mutate(
  WEATHER_LABEL = factor(weather_map[as.character(WEATHER)],
                         levels = unique(weather_map))
)

# ---- Missing-Data Strategy ---------------------------------------------------
# Rationale:
# * HOUR and WEATHER describe exposure/context. If unknown, we DROP those rows
#   (not imputed) to avoid biasing time-of-day and weather analyses.
# * PERSONS: if missing, replace with median (overall) to preserve sample size
#   in exposure summaries; this variable is a size proxy.
# * FATALS: do NOT impute (critical outcome). If rare NAs exist, drop those rows.

n_before <- nrow(df)

# Drop rows missing critical context variables (HOUR or WEATHER)
df_step1 <- df %>% filter(!is.na(HOUR), !is.na(WEATHER))

# Impute PERSONS with median (computed on available values)
persons_med <- median(df_step1$PERSONS, na.rm = TRUE)
df_step2 <- df_step1 %>%
  mutate(PERSONS = ifelse(is.na(PERSONS), persons_med, PERSONS))

# Drop rows with missing FATALS (should be rare in FARS ACCIDENTS)
df_step3 <- df_step2 %>% filter(!is.na(FATALS))

n_after_missing <- nrow(df_step3)
rows_removed_missing <- n_before - n_after_missing

# ---- Outlier Strategy --------------------------------------------------------
# We keep a RAW version for transparency.
# For CLEAN plots, we remove EXTREME outliers using 3*IQR rule on PERSONS and FATALS.
# (A winsorized alternative is also provided below.)

iqr_f <- IQR(df_step3$FATALS, na.rm = TRUE)
q1_f  <- quantile(df_step3$FATALS, 0.25, na.rm = TRUE)
q3_f  <- quantile(df_step3$FATALS, 0.75, na.rm = TRUE)
upper_f_extreme <- q3_f + 3*iqr_f

iqr_p <- IQR(df_step3$PERSONS, na.rm = TRUE)
q1_p  <- quantile(df_step3$PERSONS, 0.25, na.rm = TRUE)
q3_p  <- quantile(df_step3$PERSONS, 0.75, na.rm = TRUE)
upper_p_extreme <- q3_p + 3*iqr_p

df_clean <- df_step3 %>%
  filter(FATALS <= upper_f_extreme, PERSONS <= upper_p_extreme)

rows_removed_outliers <- nrow(df_step3) - nrow(df_clean)

# Optional: winsorized version (99th percentile caps) for sensitivity
p99_f <- quantile(df_step3$FATALS, 0.99, na.rm = TRUE)
p99_p <- quantile(df_step3$PERSONS, 0.99, na.rm = TRUE)
df_winsor <- df_step3 %>%
  mutate(
    FATALS  = pmin(FATALS,  as.numeric(p99_f)),
    PERSONS = pmin(PERSONS, as.numeric(p99_p))
  )

# ---- Save cleaned datasets ---------------------------------------------------
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
write_csv(df_clean,  "outputs/fars2022_clean.csv")
write_csv(df_winsor, "outputs/fars2022_winsor.csv")

# ---- Plots (Raw vs Clean) ----------------------------------------------------
dir.create("outputs/plots", showWarnings = FALSE, recursive = TRUE)

# 1) Average fatalities per crash by HOUR (RAW)
p_hour_raw <- df_step3 %>%
  group_by(HOUR) %>%
  summarize(avg_fatals = mean(FATALS), n = n(), .groups = "drop") %>%
  ggplot(aes(HOUR, avg_fatals)) +
  geom_line() + geom_point() +
  labs(title = "Average Fatalities per Crash by Hour (RAW)",
       x = "Hour of Day (0–23)", y = "Average Fatalities per Crash")

ggsave("outputs/plots/avg_fatals_by_hour_RAW.png", p_hour_raw, width = 8, height = 5, dpi = 160)

# 2) Average fatalities per crash by HOUR (CLEAN)
p_hour_clean <- df_clean %>%
  group_by(HOUR) %>%
  summarize(avg_fatals = mean(FATALS), n = n(), .groups = "drop") %>%
  ggplot(aes(HOUR, avg_fatals)) +
  geom_line() + geom_point() +
  labs(title = "Average Fatalities per Crash by Hour (CLEAN - Extreme Outliers Removed)",
       x = "Hour of Day (0–23)", y = "Average Fatalities per Crash")

ggsave("outputs/plots/avg_fatals_by_hour_CLEAN.png", p_hour_clean, width = 8, height = 5, dpi = 160)

# 3) Distribution of PERSONS (RAW vs CLEAN shown separately)
p_persons_raw <- df_step3 %>%
  ggplot(aes(PERSONS)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  labs(title = "Distribution of PERSONS per Crash (RAW)",
       x = "Persons per Crash", y = "Count of Crashes")
ggsave("outputs/plots/persons_hist_RAW.png", p_persons_raw, width = 8, height = 5, dpi = 160)

p_persons_clean <- df_clean %>%
  ggplot(aes(PERSONS)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  labs(title = "Distribution of PERSONS per Crash (CLEAN)",
       x = "Persons per Crash", y = "Count of Crashes")
ggsave("outputs/plots/persons_hist_CLEAN.png", p_persons_clean, width = 8, height = 5, dpi = 160)

# 4) Weather Distribution (CLEAN), labeled categories
p_weather_clean <- df_clean %>%
  mutate(WEATHER_LABEL = fct_explicit_na(WEATHER_LABEL, na_level = "Unknown/NA")) %>%
  count(WEATHER_LABEL) %>%
  ggplot(aes(x = reorder(WEATHER_LABEL, -n), y = n)) +
  geom_col() + coord_flip() +
  labs(title = "Weather at Time of Fatal Crash (CLEAN)",
       x = "Weather", y = "Number of Crashes")
ggsave("outputs/plots/weather_counts_CLEAN.png", p_weather_clean, width = 8, height = 6, dpi = 160)

# ---- Console Summary ---------------------------------------------------------
cat("\n=== Missing-Data & Outlier Summary ===\n")
cat("Rows before cleaning: ", n_before, "\n")
cat("Rows after dropping missing context (HOUR/WEATHER) and FATALS: ", n_after_missing, "\n")
cat("Rows removed due to missingness: ", rows_removed_missing, "\n")
cat("Rows removed as extreme outliers (3*IQR on FATALS or PERSONS): ", rows_removed_outliers, "\n")
cat("PERSONS median used for imputation: ", persons_med, "\n")
cat("Extreme thresholds => FATALS <= ", as.numeric(upper_f_extreme),
    ", PERSONS <= ", as.numeric(upper_p_extreme), "\n")
cat("Winsorization caps (99th pct) => FATALS: ", as.numeric(p99_f),
    ", PERSONS: ", as.numeric(p99_p), "\n")
