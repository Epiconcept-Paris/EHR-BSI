#' Process Malta BSI data from raw format to EHR-BSI format
#'
#' @param raw_data Received from genericRecodeOrchestrator.R
#'
#' @return Returns a list containing the four EHR-BSI data tables: ehrbsi, patient, isolate, res
#' @export
#'


# Internal helper functions (not exported)
.process_malta_basic_cleaning <- function(raw_data, reporting_year) {
  # Validate required columns exist
  required_cols <- c("HospitalId", "PatientId", "DateOfHospitalAdmission")
  validate_required_columns(raw_data, required_cols, "Malta BSI data")
  
  # Find columns ending with "_noncdm"
  noncdm_cols <- grep("_noncdm$", names(raw_data), value = TRUE)
  
  # Print info about non-CDM columns if they exist
  if (length(noncdm_cols) > 0) {
    cat(length(noncdm_cols), "columns require recoding to match generic coded values\n")
  }
  
  # Recode all dates based on Malta's date formatting
  fallback_date_cols <- c("DateOfSpecCollection", "DateOfHospitalAdmission", "DateOfHospitalDischarge", "EpisodeStartDate_noncdm")
  raw_data <- parse_dates_with_fallback(raw_data, fallback_date_cols, "%d/%m/%Y")
  
  # Create lookup vectors using shared function
  malta_unit_lookup <- create_lookup_vector(Malta_UnitSpecialty_Lookup, "generic_code", "malta_code")
  malta_outcome_lookup <- create_lookup_vector(Malta_Outcome_Lookup, "generic_code", "malta_code")
  malta_hosptype_lookup <- create_lookup_vector(Malta_HospType_Lookup, "hosptype_code", "malta_hosptype")

  # Malta-specific recoding using temporary, non-CDM vars imported from raw
  recoded_data <- raw_data %>%
    dplyr::mutate(
      DateOfSpecCollection = if ("EpisodeStartDate_noncdm" %in% names(.)) EpisodeStartDate_noncdm else DateOfSpecCollection,
      patientType = if ("patientType_noncdm" %in% names(.)) {
        dplyr::case_when(
          patientType_noncdm == "TRUE" ~ "INPAT",
          patientType_noncdm == "FALSE" & 
            (grepl("OP", sourceLocation_noncdm) | grepl("outpatients", tolower(sourceLocation_noncdm))) ~ "OUTPAT",
          patientType_noncdm == "FALSE" ~ "OTH",
          TRUE ~ NA
        )
      } else "INPAT",
      OutcomeOfCase = if ("OutcomeOfCase_noncdm" %in% names(.)) {
        dplyr::case_when(
          OutcomeOfCase_noncdm %in% names(malta_outcome_lookup) ~ malta_outcome_lookup[OutcomeOfCase_noncdm],
          OutcomeOfCase_noncdm != "" ~ "A", # anything else is 'ALIVE'
          TRUE ~ NA
        )
      } else OutcomeOfCase,
      HospitalType = if ("HospitalId" %in% names(.)) {
        dplyr::case_when(
          HospitalId %in% names(malta_hosptype_lookup) ~ malta_hosptype_lookup[HospitalId],
          is.na(HospitalId) ~ NA,
          TRUE ~ "NOT CODED"
        )
      } else HospitalType,
      PreviousAdmission = if ("PreviousAdmission_noncdm" %in% names(.)) {
        dplyr::case_when(
          PreviousAdmission_noncdm == TRUE ~ "OTH",
          PreviousAdmission_noncdm == FALSE ~ "NO",
          TRUE ~ NA
        )
      } else PreviousAdmission
    )
  
  # Apply Malta-specific lookup recoding using shared function
  if ("UnitSpecialtyShort_noncdm" %in% names(recoded_data)) {
    recoded_data <- recode_with_lookup(recoded_data, "UnitSpecialtyShort_noncdm", malta_unit_lookup)
    recoded_data <- recoded_data %>%
      dplyr::mutate(UnitSpecialtyShort = UnitSpecialtyShort_noncdm)
  }
  
  # Delete all non-CDM variables from the final dataset
  noncdm_cols_to_remove <- intersect(
    c("UnitSpecialtyShort_noncdm", "sourceLocation_noncdm", "OutcomeOfCase_noncdm", 
      "HospitalType_noncdm", "patientType_noncdm", "PreviousAdmission_noncdm", "EpisodeStartDate_noncdm"),
    names(recoded_data)
  )
  
  if (length(noncdm_cols_to_remove) > 0) {
    recoded_data <- recoded_data %>% dplyr::select(-dplyr::all_of(noncdm_cols_to_remove))
  }
  
  # Create unique, relatable ID for each table's level
  recoded_data <- create_hierarchical_record_ids(
    recoded_data,
    hospital_col = "HospitalId",
    patient_col = "PatientId",
    admission_date_col = "DateOfHospitalAdmission",
    specimen_date_col = "DateOfSpecCollection"
  )
  
  return(recoded_data)
}

.create_malta_patient_table <- function(recoded_data) {
  # Create base patient table using shared function with Malta-specific defaults
  malta_defaults <- list(
    HospitalisationAdmissionCodeSystem = "SNOMED-CT"
  )
  
  patient <- create_standard_patient_table(recoded_data, country_defaults = malta_defaults)
  
  # Add Malta-specific fields
  patient <- patient %>%
    dplyr::mutate(
      DateOfAdmissionCurrentWard = NA_character_ # only have one admission date for overall record
    )
  
  # Finalize table with standard column selection
  patient <- finalize_table(patient, get_standard_table_columns("patient"))
  
  return(patient)
}

.create_malta_isolate_table <- function(recoded_data) {
  # Create lookup vector using shared function
  malta_pathogen_lookup <- create_lookup_vector(Malta_PathogenCode_Lookup, "microorganism_code", "malta_pathogen_name")
  
  # Create base isolate table using shared function
  isolate <- create_standard_isolate_table(recoded_data)
  
  # Add Malta-specific organism code mapping
  isolate <- isolate %>%
    dplyr::mutate(
      MicroorganismCode = if ("MicroorganismCodeLabel" %in% names(.)) {
        dplyr::case_when(
          MicroorganismCodeLabel %in% names(malta_pathogen_lookup) ~ malta_pathogen_lookup[MicroorganismCodeLabel],
          !is.na(MicroorganismCodeLabel) ~ paste0("UNMAPPED: ", MicroorganismCodeLabel),
          TRUE ~ NA_character_
        )
      } else NA_character_
    )
  
  # Finalize table with standard column selection
  isolate <- finalize_table(isolate, get_standard_table_columns("isolate"))
  
  return(isolate)
}

.create_malta_res_table <- function(recoded_data) {
  res <- recoded_data %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("ab_"),   # every variable that begins with "ab_"
      names_to = "Antibiotic",    # new key column
      values_to = "SIR",           # new value column
      values_drop_na = TRUE       # keeps the output compact
    ) %>% 
    dplyr::mutate(
      Antibiotic = Antibiotic %>%                # work on the column you just created
        stringr::str_remove("^ab_") %>%                   # 1) drop the leading "ab_"
        stringr::str_remove_all("\\d+")                   # 2) remove every digit that remains
    ) %>%
    dplyr::mutate(
      RecordId = paste0(record_id_isolate, "_", Antibiotic),
      ParentId = record_id_isolate,
      ResultPCRmec = NA, # request?
      ResultPbp2aAggl = if ("ResultPbp2aAggl" %in% names(.)) ResultPbp2aAggl else NA,
      ResultESBL = if ("ResultESBL" %in% names(.)) ResultESBL else NA,
      ResultCarbapenemase = if ("ResultCarbapenemase" %in% names(.)) ResultCarbapenemase else NA,
      ZoneValue = NA, # request?
      ZoneSIR = NA, # request?
      ZoneSusceptibilitySign = NA, # request?
      MICSusceptibilitySign = NA, # request?
      MICValue = NA, # request?
      MICSIR = NA, # request?
      GradSusceptibilitySign = NA, # request?
      GradValue = NA, # request?
      GradSIR = NA, # request?
      ZoneTestDiskLoad = NA, # request?
      ReferenceGuidelinesSIR = NA # request?
    ) %>%
    dplyr::select(
      RecordId, ParentId, Antibiotic, SIR, ResultPCRmec, ResultPbp2aAggl, ResultESBL, ResultCarbapenemase,
      ZoneSIR, ZoneValue, ZoneSusceptibilitySign, MICSusceptibilitySign, MICValue, MICSIR, 
      GradSusceptibilitySign, GradValue, GradSIR, ZoneTestDiskLoad, ReferenceGuidelinesSIR
    ) %>%
    dplyr::filter(SIR != "") # remove empty records, where no sensitivity result was reported for that antibiotic
  
  # Finalize table using shared function
  res <- finalize_table(res)
  
  return(res)
}

.create_malta_ehrbsi_table <- function(recoded_data, reporting_year, episode_duration) {
  

    # Calculate reporting year from admission dates in the data
  # Use the most recent year if data spans multiple years
  # NOTE: MUST BE UPDATED, NEED REPORTING YEAR TO BE PASSED TO AGGREGATE FUNCTION
  # THUS GIVING A NEW RECORD FOR EACH HOSP-YEAR INTHE DATA
  reporting_year <- max(as.numeric(format(recoded_data$DateOfHospitalAdmission, "%Y")), na.rm = TRUE)
  
  # Create base EHRBSI table using shared function
  ehrbsi <- create_base_ehrbsi_table(recoded_data, "MT", reporting_year, episode_duration)
  
  # Add Malta-specific fields
  ehrbsi <- ehrbsi %>%
    dplyr::mutate(
      ClinicalTerminology = "SNOMED-CT",
      ClinicalTerminologySpec = NA_character_,
      ESurvBSI = NA_real_, # level of automation? Full/semi/denom/manual/etc
      GeoLocation = NA_character_, # Can we get NUT2?
      HospitalSize = NA_real_, # how many beds?
      HospitalType = HospitalType,
      ProportionPopulationCovered = NA_real_ # Request coverage estimate
    )
  
  # Finalize table with standard column selection
  ehrbsi <- finalize_table(ehrbsi, get_standard_table_columns("ehrbsi"))
  
  return(ehrbsi)
}


