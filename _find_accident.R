find_accident <- function(){
  acc <- list.files("data/raw", pattern = "^accident.*\\.csv$", ignore.case = TRUE,
                    recursive = TRUE, full.names = TRUE)
  if (length(acc) == 0) stop("Place accident.csv under data/raw and re-run.")
  Sys.setenv(RAW_CSV = acc[1])
  message("Using: ", acc[1])
  acc[1]
}
