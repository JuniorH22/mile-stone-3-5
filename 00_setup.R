options(repos = c(CRAN = "https://cloud.r-project.org"))
pkgs <- c("tidyverse","janitor","skimr","readr","ggplot2","broom","e1071","car","rmarkdown")
ti <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if (length(ti)) install.packages(ti, dep = TRUE, quiet = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))
for (d in c("data/raw","data/processed","outputs/plots","report")) if (!dir.exists(d)) dir.create(d, TRUE)
message("Setup complete")
