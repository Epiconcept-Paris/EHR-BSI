#' @import shiny
#' @import bslib
#' @importFrom data.table data.table copy setDT fread setorder fifelse %chin%
#' @import dplyr
#' @import openxlsx
#' @import readxl
#' @import stringr
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom stats aggregate
#' @importFrom utils read.csv modifyList adist
#' @importFrom tools file_ext
#' @importFrom rlang .data := !!
NULL

# Declare globals used in non-standard evaluation to appease R CMD check/lintr
if (getRversion() >= "2.15.1") utils::globalVariables(c(
  "EpisodeClass", "Count", "Composition", "organism_label", "Specialty",
  "Type", "Percentage", "Organism", "Combination", "NumSpecialties",
  "SpecialtyShort", "PathogenShort", "Gender", "AgeGroup", "Category",
  "RecordId", "ParentId", "RecordIdAb",
  "DateOfSpecCollection", "DateOfHospitalAdmission", "DateOfHospitalDischarge",
  "HospitalId", "PatientId", "PatientSpecialty", "patientType", "OutcomeOfCase",
  "Specimen", "IsolateId", "MicroorganismCode", "Antibiotic", "SIR",
  "ResultPCRmec", "ResultPbp2aAggl", "ResultESBL", "ResultCarbapenemase",
  "record_id_isolate", "record_id_patient", "record_id_bsi",
  "admit_date_formatted", "specimen_date_formatted",
  "test_tag", "SusceptibilitySign", "Value", "."
))