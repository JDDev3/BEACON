#' Load all BEACON studies
#'
#' Convenience wrapper that loads every study returned by `beacon_studies()`
#' using `beacon_load()`. Downloads/caches files as needed.
#'
#' @param what The type of file information desired. se = summarized experiment, expr = expression matrix, meta = clinical meta information
#' @param show_progress If TRUE, show a text progress bar.
#'
#' @return A named list of objects (SE/data.frame). Any failures are stored
#'   in `attr(x, "errors")` as a named list of error messages.
#'
#' @export
beacon_load_all <- function(what = c("se", "expr", "meta"),
                            show_progress = TRUE) {
  what <- match.arg(what)
  
  ids <- beacon_studies() 
  if (!length(ids)) {
    out <- list()
    attr(out, "errors") <- list()
    return(out)
  }
  
  pb <- NULL
  if (isTRUE(show_progress)) {
    pb <- utils::txtProgressBar(min = 0, max = length(ids), style = 3)
    on.exit(try(close(pb), silent = TRUE), add = TRUE)
  }
  
  out  <- vector("list", length(ids))
  names(out) <- ids
  errs <- list()
  
  for (i in seq_along(ids)) {
    id <- ids[[i]]
    
    res <- tryCatch(
      beacon_load(id, what = what),
      error = function(e) e
    )
    
    if (inherits(res, "error")) {
      errs[[id]] <- conditionMessage(res)
      out[[id]] <- NULL
    } else {
      out[[id]] <- res
    }
    
    if (!is.null(pb)) utils::setTxtProgressBar(pb, i)
  }
  
  if (length(errs)) {
    warning(sprintf("Loaded with %d error(s). See attr(result, 'errors').", length(errs)),
            call. = FALSE)
  }
  
  attr(out, "errors") <- errs
  out
}