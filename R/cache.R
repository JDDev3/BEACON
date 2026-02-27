beacon_cache_dir <- function() {
  dir <- rappdirs::user_cache_dir("BEACON")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}