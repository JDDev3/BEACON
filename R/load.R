
#' Title
#'
#' @param study_id The desired BEACON study id to load
#' @param what The type of file information desired. se = summarized experiment, expr = expression matrix, meta = clinical meta information
#'
#' @return 
#' if what = SE, a summarized experiment
#' if what = expr: an expressino dataframe
#' if what = meta: a clinical dataframe
#' 
#' @export
#' 
beacon_load <- function(study_id, what = c("se", "expr", "meta")) {
  what <- match.arg(what)
  
  idx <- beacon_index()
  row <- idx[idx$study_id == study_id, , drop = FALSE]
  if (!nrow(row)) stop("Unknown study_id: ", study_id, "\nUse beacon_studies() to list valid IDs.")
  
  if (what == "expr") {
    expr_path <- beacon_download_one(row$expr_key[1])
    expr <- vroom::vroom(expr_path, delim = "\t", show_col_types = FALSE)
    expr_df <- as.data.frame(expr)
    rn <- expr_df[[1]]
    mat <- as.matrix(expr_df[, -1, drop = FALSE])
    rownames(mat) <- rn
    return(mat)
  }
  
  if (what == "meta") {
    meta_path <- beacon_download_one(row$meta_key[1])
    meta <- vroom::vroom(meta_path, delim = "\t", show_col_types = FALSE)
    return(as.data.frame(meta))
  }
  
  # what == "se"
  expr_path <- beacon_download(row$expr_key[1])
  meta_path <- beacon_download(row$meta_key[1])
  
  expr <- vroom::vroom(expr_path, delim = "\t", show_col_types = FALSE)
  meta <- vroom::vroom(meta_path, delim = "\t", show_col_types = FALSE)
  
  expr_df <- as.data.frame(expr)
  rn <- expr_df[[1]]
  mat <- as.matrix(expr_df[, -1, drop = FALSE])
  rownames(mat) <- rn
  
  meta_df <- as.data.frame(meta)
  
  SummarizedExperiment::SummarizedExperiment(
    assays  = list(expr = mat),
    colData = S4Vectors::DataFrame(meta_df)
  )
}