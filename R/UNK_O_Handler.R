#' Helper function for "UNK"/"N/A"/"O" recodes
#'
#' @param DT Input data.table
#' @param cols Columns to recode; defaults to all character columns
#'
#'
#' @return Returns the data with relevant recodes
#'
UNK_O_Handler <- function(DT, cols = NULL){
  if(is.null(cols)) cols <- names(DT)
  DT[, (cols) := lapply(.SD, function(x) {
    if (is.character(x)) {
      # Replace UNK/N/A with NA and O with OTH
      fifelse(x %chin% c("UNK", "N/A", "Unk", "unk"), NA_character_,
              fifelse(x %chin% c("O"), "OTH", x))

    } else {
      x
    }
  }), .SDcols = cols]
}
