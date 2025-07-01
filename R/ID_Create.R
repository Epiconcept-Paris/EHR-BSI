#' Helper function for complex ID creation
#'
#' @param DT Input data.table
#' @param cols Columns to include into the new ID
#' @param new_col Name of the new ID column
#'
#' @return Returns the data with the newly defined ID
#'
ID_Create <- function(DT, cols, new_col = "ParentId") {
  DT[, (new_col) := do.call(paste, c(.SD, sep = "_")), .SDcols = cols]
  DT
}
