beacon_zenodo_record_id <- function() {
  getOption("BEACON.zenodo_record_id", BEACON_DEFAULT_RECORD_ID)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

beacon_manifest <- function(record_id = beacon_zenodo_record_id(), refresh = FALSE) {
  cache <- file.path(beacon_cache_dir(), sprintf("manifest_%s.rds", record_id))
  if (!refresh && file.exists(cache)) return(readRDS(cache))
  
  url <- sprintf("https://zenodo.org/api/records/%s", record_id)
  resp <- httr2::request(url) |> httr2::req_perform()
  x <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  
  files <- x$files
  out <- data.frame(
    key      = files$key %||% files$filename,
    size     = files$size,
    checksum = files$checksum,
    url      = files$links$self %||% files$links$download,
    stringsAsFactors = FALSE
  )
  
  saveRDS(out, cache)
  out
}

beacon_download <- function(key, destdir = beacon_cache_dir(), overwrite = FALSE, verify_md5 = TRUE) {
  man <- beacon_manifest()
  hit <- man[man$key == key, , drop = FALSE]
  if (!nrow(hit)) stop("File not found on Zenodo: ", key)
  
  if (!dir.exists(destdir)) dir.create(destdir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(destdir, key)
  
  if (!overwrite && file.exists(dest)) return(dest)
  
  httr2::request(hit$url[1]) |> httr2::req_perform(path = dest)
  
  if (verify_md5 && nzchar(hit$checksum[1])) {
    md5_expected <- sub("^md5:", "", hit$checksum[1])
    md5_actual <- digest::digest(file = dest, algo = "md5")
    if (!identical(tolower(md5_actual), tolower(md5_expected))) {
      stop("MD5 mismatch for ", key)
    }
  }
  
  dest
}