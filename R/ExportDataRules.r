#' Apply final export rules to EHR-BSI tables
#'
#' Collates last-step data checks and standardizations before output is returned.
#'
#' @param result_list List containing ehrbsi, patient, isolate, res tables
#'
#' @return Updated result list after export rules are applied
apply_export_data_rules <- function(result_list) {
  if (is.null(result_list) || !is.list(result_list)) {
    return(result_list)
  }
  
  standard_cols <- list(
    ehrbsi = c(
      "RecordId", "RecordType", "RecordTypeVersion", "Subject", "Status",
      "DataSource", "ReportingCountry", "DateUsedForStatistics", "HospitalId",
      "LaboratoryCode", "GeoLocation", "HospitalSize", "HospitalType", "ESurvBSI",
      "AggregationLevel", "EpisodeDuration", "ClinicalTerminology",
      "ClinicalTerminologySpec", "MicrobiologicalTerminology",
      "MicrobiologicalTerminologySpec", "NumberOfBloodCultureSets",
      "NumberOfHospitalDischarges", "NumberOfHospitalPatientDays",
      "ProportionPopulationCovered", "NumberOfHOHABSIs",
      "NumberOfImportedHABSIs", "NumberOfTotalBSIs"
    ),
    patient = c(
      "RecordId", "ParentId", "RecordType", "UnitId", "UnitSpecialtyShort",
      "PatientSpecialty", "DateOfAdmissionCurrentWard", "PatientId", "Age",
      "Sex", "patientType", "DateOfHospitalAdmission",
      "DateOfHospitalDischarge", "OutcomeOfCase", "HospitalisationCode",
      "HospitalisationCodeLabel", "HospitalisationAdmissionCodeSystem",
      "HospitalisationCodeSystemVersion",
      "HospitalisationAdmissionCodeSystemSpec", "PreviousAdmission"
    ),
    isolate = c(
      "RecordId", "ParentId", "RecordType", "DateOfSpecimenCollection",
      "LaboratoryCode", "IsolateId", "Specimen", "MicroorganismCode",
      "MicroorganismCodeLabel", "MicroorganismCodeSystem",
      "MicroorganismCodeSystemSpec", "MicroorganismCodeSystemVersion"
    ),
    res = c(
      "ParentId", "RecordId", "RecordType", "Antibiotic", "SIR",
      "ResultPCRmec", "ResultPbp2aAggl", "ResultESBL", "ResultCarbapenemase",
      "ZoneValue", "ZoneSusceptibilitySign", "MICSusceptibilitySign",
      "MICValue", "GradSusceptibilitySign", "GradValue", "ZoneTestDiskLoad",
      "ReferenceGuidelinesSIR"
    )
  )
  
  # Standardize formatting across tables
  result_list <- standardize_all_table_dates(result_list)
  result_list <- standardize_all_table_sex(result_list)
  result_list <- standardize_all_table_mic_sign(result_list)
  result_list <- standardize_all_table_unit_specialty(result_list)
  
  # Final res-table rules
  if ("res" %in% names(result_list) && !is.null(result_list$res)) {
    res <- result_list$res
    
    # Remove res records with no SIR interpretation
    if ("SIR" %in% names(res)) {
      sir_norm <- toupper(trimws(as.character(res$SIR)))
      res <- res[!is.na(sir_norm) & sir_norm %in% c("S", "I", "R"), , drop = FALSE]
    }
    
    # Remove res records with no parent in isolate table
    if ("ParentId" %in% names(res) &&
        "isolate" %in% names(result_list) &&
        !is.null(result_list$isolate) &&
        "RecordId" %in% names(result_list$isolate)) {
      res <- res[res$ParentId %in% result_list$isolate$RecordId, , drop = FALSE]
    }
    
    result_list$res <- res
  }
  
  # Remove non-standard columns from each table
  for (tbl_name in names(standard_cols)) {
    if (tbl_name %in% names(result_list) && !is.null(result_list[[tbl_name]])) {
      keep_cols <- intersect(standard_cols[[tbl_name]], names(result_list[[tbl_name]]))
      result_list[[tbl_name]] <- result_list[[tbl_name]][, keep_cols, drop = FALSE]
    }
  }
  
  return(result_list)
}
