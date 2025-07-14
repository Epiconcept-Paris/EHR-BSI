#' Common functions for country-specific BSI data recoding
#'
#' This file contains shared functions used by both Estonia and Malta
#' recoding scripts to avoid code duplication and ensure consistency.

#' Validate that required columns exist in the data
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param context_name String describing the context for error messages
#'
#' @return Nothing if validation passes, throws error if columns missing
validate_required_columns <- function(data, required_cols, context_name = "dataset") {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in ", context_name, ": ", 
         paste(missing_cols, collapse = ", "))
  }
}

#' Parse date columns with dictionary fallback
#'
#' @param data Data frame containing date columns
#' @param fallback_cols Character vector of date column names to use if dictionary unavailable
#' @param date_format Format string for date parsing
#'
#' @return Data frame with parsed date columns
parse_dates_with_fallback <- function(data, fallback_cols, date_format = "%d/%m/%Y") {
  if (exists("getAnyDictionaryValue")) {
    # Use dictionary if available
    date_variables <- getAnyDictionaryValue(varname = "date", search = "type", value = "generic_name")
    available_date_cols <- intersect(date_variables, names(data))
  } else {
    # Fallback to specified columns
    available_date_cols <- intersect(fallback_cols, names(data))
  }
  
  if (length(available_date_cols) > 0) {
    data[available_date_cols] <- lapply(
      data[available_date_cols],
      function(x) as.Date(x, format = date_format)
    )
  }
  
  return(data)
}

#' Create lookup vector from lookup table
#'
#' @param lookup_table Data frame with lookup values
#' @param value_col Name of column containing target values
#' @param key_col Name of column containing source keys
#'
#' @return Named vector for use with dplyr::recode
create_lookup_vector <- function(lookup_table, value_col, key_col) {
  if (is.null(lookup_table) || nrow(lookup_table) == 0) {
    return(character(0))
  }
  
  # Use copy() to ensure we don't modify package data
  lookup_copy <- copy(lookup_table)
  return(setNames(lookup_copy[[value_col]], lookup_copy[[key_col]]))
}

#' Create base EHRBSI table with common fields
#'
#' @param data Source data frame
#' @param country_code Two-letter country code (e.g., "EE", "MT")
#' @param reporting_year Year for DateUsedForStatistics
#' @param episode_duration Episode duration in days
#' @param record_id_col Name of column containing record IDs
#'
#' @return Data frame with base EHRBSI structure
create_base_ehrbsi_table <- function(data, country_code,
                                   episode_duration, record_id_col = "record_id_bsi") {
  base_ehrbsi <- data %>%
    dplyr::mutate(
      AggregationLevel = "HOSP",
      DataSource = paste0(country_code, "-EHRBSI"),
      DateUsedForStatistics = format(DateOfSpecCollection, "%Y"),
      EpisodeDuration = episode_duration,
      HospitalId = HospitalId,
      LaboratoryCode = NA_character_,
      MicrobiologicalTerminology = "SNOMED-CT",
      MicrobiologicalTerminologySpec = NA_character_,
      NumberOfBloodCultureSets = NA_real_,
      NumberOfHOHABSIs = NA_real_,
      NumberOfHospitalDischarges = NA_real_,
      NumberOfHospitalPatientDays = NA_real_,
      NumberOfImportedHABSIs = NA_real_,
      NumberOfTotalBSIs = NA_real_,
      RecordId = .data[[record_id_col]],
      RecordType = "EHRBSI",
      RecordTypeVersion = NA_character_,
      ReportingCountry = country_code,
      Status = "New/Update",
      Subject = "EHRBSI"
    )
  
  return(base_ehrbsi)
}

#' Apply common table finalization steps
#'
#' @param data Data frame to finalize
#' @param select_cols Character vector of columns to select (NULL to keep all)
#' @param arrange_cols Character vector of columns to arrange by (NULL for no arrangement)
#'
#' @return Finalized data frame with distinct rows
finalize_table <- function(data, select_cols = NULL, arrange_cols = NULL) {
  result <- data
  
  if (!is.null(select_cols)) {
    # Only select columns that exist in the data
    available_cols <- intersect(select_cols, names(data))
    result <- result %>% dplyr::select(dplyr::all_of(available_cols))
  }
  
  if (!is.null(arrange_cols)) {
    # Only arrange by columns that exist in the data
    available_arrange_cols <- intersect(arrange_cols, names(data))
    if (length(available_arrange_cols) > 0) {
      result <- result %>% dplyr::arrange(dplyr::across(dplyr::all_of(available_arrange_cols)))
    }
  }
  
  result <- result %>% dplyr::distinct()
  
  return(result)
}

#' Create hierarchical record IDs with date-time components
#'
#' @param data Data frame containing source data
#' @param hospital_col Name of hospital ID column
#' @param patient_col Name of patient ID column
#' @param admission_date_col Name of admission date column
#' @param specimen_date_col Name of specimen collection date column (optional)
#' @param isolate_col Name of isolate ID column (optional)
#' @param organism_col Name of organism code column (optional)
#'
#' @return Data frame with added record ID columns
create_hierarchical_record_ids <- function(data, hospital_col = "HospitalId", 
                                         patient_col = "PatientId",
                                         admission_date_col = "DateOfHospitalAdmission",
                                         specimen_date_col = "DateOfSpecCollection",
                                         isolate_col = "IsolateId",
                                         organism_col = "MicroorganismCode",
                                         record_id_bsi_col = "record_id_bsi") {
  
  result <- data %>%
    dplyr::mutate(
      record_id_bsi = .data[[record_id_bsi_col]]
    )
  
  # Create patient record ID with admission date
  if (admission_date_col %in% names(data)) {
    result <- result %>%
      dplyr::mutate(
        admit_date_formatted = format(.data[[admission_date_col]], "%d%m%Y"),
        record_id_patient = paste0(.data[[patient_col]], "-", admit_date_formatted)
      ) %>%
      dplyr::select(-admit_date_formatted)
  } else {
    result <- result %>%
      dplyr::mutate(
        record_id_patient = .data[[patient_col]]
      )
  }
  
  # Create isolate record ID if specimen date available
  if (specimen_date_col %in% names(data)) {
    result <- result %>%
      dplyr::mutate(
        specimen_date_formatted = format(.data[[specimen_date_col]], "%d%m%Y"),
        record_id_isolate = paste0(.data[[patient_col]], "-", specimen_date_formatted)
      ) %>%
      dplyr::select(-specimen_date_formatted)
    
    # If isolate and organism columns exist, create more specific isolate ID
    if (isolate_col %in% names(data) && organism_col %in% names(data)) {
      result <- result %>%
        dplyr::mutate(
          record_id_isolate = paste0(.data[[isolate_col]], "_", .data[[organism_col]])
        )
    }
  }
  
  return(result)
}

#' Recode values using lookup table with fallback
#'
#' @param data Data frame to recode
#' @param column_name Name of column to recode
#' @param lookup_vector Named vector for recoding
#' @param fallback_value Value to use for unmapped items (default keeps original)
#'
#' @return Data frame with recoded column
recode_with_lookup <- function(data, column_name, lookup_vector, fallback_value = NULL) {
  if (length(lookup_vector) == 0 || !column_name %in% names(data)) {
    return(data)
  }
  
  if (is.null(fallback_value)) {
    # Keep original values for unmapped items
    data[[column_name]] <- dplyr::recode(data[[column_name]], !!!lookup_vector, 
                                       .default = data[[column_name]])
  } else {
    # Use specified fallback value
    data[[column_name]] <- dplyr::recode(data[[column_name]], !!!lookup_vector, 
                                       .default = fallback_value)
  }
  
  return(data)
}

#' Create standard patient table structure
#'
#' @param data Source data frame
#' @param record_id_col Name of patient record ID column
#' @param parent_id_col Name of parent (BSI) record ID column
#' @param country_defaults List of country-specific default values
#'
#' @return Data frame with standard patient table structure
create_standard_patient_table <- function(data, record_id_col = "record_id_patient",
                                         parent_id_col = "record_id_bsi",
                                         country_defaults = list()) {
  
  # Set default values
  defaults <- list(
    PatientSpecialty = NA_character_,
    patientType = "INPAT",
    OutcomeOfCase = NA_character_,
    HospitalisationCode = NA_character_,
    HospitalisationCodeLabel = NA_character_,
    HospitalisationAdmissionCodeSystem = "ICD-10",
    HospitalisationCodeSystemVersion = NA_character_,
    HospitalisationAdmissionCodeSystemSpec = NA_character_
  )
  
  # Override with country-specific defaults
  defaults <- modifyList(defaults, country_defaults)
  
  # Create base patient table
  patient <- data %>%
    dplyr::mutate(
      RecordId = .data[[record_id_col]],
      ParentId = .data[[parent_id_col]],
      PatientSpecialty = defaults$PatientSpecialty,
      patientType = if("patientType" %in% names(.)) patientType else defaults$patientType,
      OutcomeOfCase = if("OutcomeOfCase" %in% names(.)) OutcomeOfCase else defaults$OutcomeOfCase,
      HospitalisationCode = defaults$HospitalisationCode,
      HospitalisationCodeLabel = defaults$HospitalisationCodeLabel,
      HospitalisationAdmissionCodeSystem = defaults$HospitalisationAdmissionCodeSystem,
      HospitalisationCodeSystemVersion = defaults$HospitalisationCodeSystemVersion,
      HospitalisationAdmissionCodeSystemSpec = defaults$HospitalisationAdmissionCodeSystemSpec
    )
  
  return(patient)
}

#' Create standard isolate table structure
#'
#' @param data Source data frame
#' @param record_id_col Name of isolate record ID column
#' @param parent_id_col Name of parent (patient) record ID column
#' @param country_defaults List of country-specific default values
#'
#' @return Data frame with standard isolate table structure
create_standard_isolate_table <- function(data, record_id_col = "record_id_isolate",
                                         parent_id_col = "PatientId",
                                         country_defaults = list()) {
  
  # Set default values
  defaults <- list(
    LaboratoryCode = NA_character_,
    Specimen = NA_character_,
    MicroorganismCodeSystem = "SNOMED-CT",
    MicroorganismCodeSystemSpec = NA_character_,
    MicroorganismCodeSystemVersion = NA_character_
  )
  
  # Override with country-specific defaults
  defaults <- modifyList(defaults, country_defaults)
  
  # Create base isolate table
  isolate <- data %>%
    dplyr::mutate(
      RecordId = .data[[record_id_col]],
      ParentId = .data[[parent_id_col]],
      LaboratoryCode = defaults$LaboratoryCode,
      Specimen = if("Specimen" %in% names(.)) Specimen else defaults$Specimen,
      MicroorganismCodeSystem = defaults$MicroorganismCodeSystem,
      MicroorganismCodeSystemSpec = defaults$MicroorganismCodeSystemSpec,
      MicroorganismCodeSystemVersion = defaults$MicroorganismCodeSystemVersion
    )
  
  return(isolate)
}

#' Get standard column selection for EHR-BSI tables
#'
#' @param table_type Type of table: "patient", "isolate", "res", or "ehrbsi"
#'
#' @return Character vector of standard column names for the specified table type
get_standard_table_columns <- function(table_type) {
  
  switch(table_type,
    "patient" = c(
      "RecordId", "ParentId", "UnitId", "UnitSpecialtyShort", "PatientSpecialty", 
      "DateOfAdmissionCurrentWard", "PatientId", "Age", "Sex", "patientType",
      "DateOfHospitalAdmission", "DateOfHospitalDischarge", "OutcomeOfCase",
      "HospitalisationCode", "HospitalisationCodeLabel",
      "HospitalisationAdmissionCodeSystem", "HospitalisationCodeSystemVersion",
      "HospitalisationAdmissionCodeSystemSpec", "PreviousAdmission"
    ),
    
    "isolate" = c(
      "RecordId", "ParentId", "DateOfSpecCollection", "LaboratoryCode", "IsolateId", "Specimen",
      "MicroorganismCode", "MicroorganismCodeLabel", "MicroorganismCodeSystem", 
      "MicroorganismCodeSystemSpec", "MicroorganismCodeSystemVersion"
    ),
    
    "res" = c(
      "ParentId", "RecordId", "Antibiotic", "SIR", "ResultPCRmec", "ResultPbp2aAggl", 
      "ResultESBL", "ResultCarbapenemase", "ZoneSIR", "ZoneValue", "ZoneSusceptibilitySign", 
      "MICSusceptibilitySign", "MICValue", "MICSIR", "GradSusceptibilitySign", "GradValue", 
      "GradSIR", "ZoneTestDiskLoad", "ReferenceGuidelinesSIR"
    ),
    
    "ehrbsi" = c(
      "AggregationLevel", "ClinicalTerminology", "ClinicalTerminologySpec", "DataSource",
      "DateUsedForStatistics", "EpisodeDuration", "ESurvBSI", "GeoLocation", "HospitalId",
      "HospitalSize", "HospitalType", "LaboratoryCode", "MicrobiologicalTerminology",
      "MicrobiologicalTerminologySpec", "NumberOfBloodCultureSets", "NumberOfHOHABSIs",
      "NumberOfHospitalDischarges", "NumberOfHospitalPatientDays", "NumberOfImportedHABSIs",
      "NumberOfTotalBSIs", "ProportionPopulationCovered", "RecordId", "RecordType",
      "RecordTypeVersion", "ReportingCountry", "Status", "Subject"
    ),
    
    stop("Unknown table_type: ", table_type, ". Must be one of: patient, isolate, res, ehrbsi")
  )
} 