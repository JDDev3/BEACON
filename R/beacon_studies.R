#' List available BEACON studies
#'
#' Returns the study IDs available under the current Zenodo record. 
#'
#' @param detail If TRUE, return the full index data.frame. If FALSE (default), return a character vector of study IDs.
#' @param record_id Zenodo record ID to use. 
#' @param refresh If TRUE, force a rebuild/reload of the index for this record_id.
#'
#' @return If detail=FALSE: character vector of study IDs.
#'   If detail=TRUE: data.frame (the index).
#'
#' @export
beacon_studies <- function(detail = FALSE,
                           record_id = beacon_zenodo_record_id(),
                           refresh = FALSE) {
  idx <- beacon_index(record_id = record_id, refresh = refresh)
  
  if (!is.data.frame(idx)) stop("beacon_index() did not return a data.frame.")
  if (!"study_id" %in% names(idx)) stop("Index is missing required column: study_id")
  
  if (isTRUE(detail)) {
    idx <- idx[order(idx$study_id), , drop = FALSE]
    rownames(idx) <- NULL
    return(idx)
  }
  
  sort(unique(idx$study_id))
}