beacon_index <- function(record_id = beacon_zenodo_record_id(), refresh = FALSE) {
  # If user is on the default record and we have a shipped index, use it.
  if (!refresh && identical(as.integer(record_id), as.integer(BEACON_DEFAULT_RECORD_ID))) {
    idx_path <- system.file("extdata", "index.csv", package = "BEACON")
    if (nzchar(idx_path)) return(read.csv(idx_path, stringsAsFactors = FALSE))
  }
  
  # Otherwise use / build a cached index keyed by record_id
  cache_path <- file.path(beacon_cache_dir(), sprintf("index_%s.csv", record_id))
  if (!refresh && file.exists(cache_path)) {
    return(read.csv(cache_path, stringsAsFactors = FALSE))
  }
  
  idx <- beacon_build_index_from_zenodo(record_id)
  write.csv(idx, cache_path, row.names = FALSE)
  idx
}

beacon_build_index_from_zenodo <- function(record_id) {
  man <- beacon_manifest(record_id, refresh = TRUE)
  
  # drop params (your stated preference)
  man <- man[!grepl("_Params", man$key, ignore.case = TRUE), , drop = FALSE]
  
  # classify expr vs meta
  expr <- man[grepl("(\\.expr_matrix.*\\.txt$)|(_Expr\\.txt$)|(_Expression\\.txt$)", man$key, ignore.case = TRUE), , drop = FALSE]
  meta <- man[grepl("(_Meta_.*\\.txt$)|(_meta_.*\\.txt$)|(_Clinical\\.txt$)", man$key, ignore.case = TRUE), , drop = FALSE]
  
  # derive study_id from filenames
  expr$study_id <- expr$key
  expr$study_id <- sub("\\.expr_matrix.*\\.txt$", "", expr$study_id, ignore.case = TRUE)
  expr$study_id <- sub("(_Expr|_Expression)\\.txt$", "", expr$study_id, ignore.case = TRUE)
  
  meta$study_id <- meta$key
  meta$study_id <- sub("(_Meta_.*|_meta_.*)\\.txt$", "", meta$study_id, ignore.case = TRUE)
  meta$study_id <- sub("_Clinical\\.txt$", "", meta$study_id, ignore.case = TRUE)
  
  # if multiple meta files, prefer latest _Meta_YYYYMMDD when present
  meta$meta_date <- regmatches(meta$key, regexpr("(?<=_Meta_)\\d{8}", meta$key, perl = TRUE))
  meta <- meta[order(meta$study_id, meta$meta_date, meta$key, decreasing = TRUE), ]
  meta <- meta[!duplicated(meta$study_id), ]
  
  # join
  idx <- merge(
    expr[, c("study_id", "key", "checksum")],
    meta[, c("study_id", "key", "checksum")],
    by = "study_id",
    suffixes = c("_expr", "_meta")
  )
  names(idx)[names(idx) == "key_expr"] <- "expr_key"
  names(idx)[names(idx) == "key_meta"] <- "meta_key"
  names(idx)[names(idx) == "checksum_expr"] <- "expr_md5"
  names(idx)[names(idx) == "checksum_meta"] <- "meta_md5"
  
  idx[order(idx$study_id), , drop = FALSE]
}