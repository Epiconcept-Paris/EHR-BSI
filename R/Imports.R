#' @import shiny
#' @importFrom data.table data.table copy setDT fread setorder fifelse %chin%
#' @import dplyr
#' @import openxlsx
#' @import readxl
#' @importFrom tidyr pivot_wider
NULL

# Declare globals used in non-standard evaluation to appease R CMD check/lintr
if (getRversion() >= "2.15.1") utils::globalVariables(c(
  "EpisodeClass", "Count", "Composition", "organism_label", "Specialty"
))