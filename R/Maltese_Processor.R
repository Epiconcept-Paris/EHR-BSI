#' Process Malta BSI data from raw format to EHR-BSI format
#'
#' @param input_file Name of the input CSV file, defaults to "BSI_REPORT_Malta.csv"
#' @param input_file_path Path to the input file, defaults to the working directory
#' @param dictionary_path Path to the data dictionary Excel file
#' @param value_maps_path Path to the value maps R script
#' @param reporting_year Year for the DateUsedForStatistics field, defaults to current year
#' @param episode_duration Duration for episode calculation in days, defaults to 14
#' @param write_to_file Whether to write output files to disk
#' @param write_to_file_path Path for output files, defaults to working directory
#' @param return_format Whether to return "list" (default) or "separate" objects
#'
#' @return Returns a list containing the four EHR-BSI data tables: ehrbsi, patient, isolate, res
#' @export
#'
#' @examples
#' \dontrun{
#' result <- process_malta_bsi(
#'   input_file_path = "Malta/data/raw",
#'   dictionary_path = "Malta/data/reference/dictionary_raw_BSI_Malta.xlsx",
#'   write_to_file = TRUE,
#'   write_to_file_path = "Malta/data/formatted"
#' )
#' }
process_malta_bsi <- function(input_file = "BSI_REPORT_Malta.csv",
                             input_file_path = NULL,
                             dictionary_path = "reference/dictionary_raw_BSI_Malta.xlsx",
                             value_maps_path = "reference/Lookup_Tables.r",
                             reporting_year = as.numeric(format(Sys.Date(), "%Y")),
                             episode_duration = 14,
                             write_to_file = FALSE,
                             write_to_file_path = NULL,
                             return_format = "list") {
  
  # Parameter validation
  if (is.null(input_file_path)) {
    input_file_path <- getwd()
  }
  
  if (is.null(write_to_file_path)) {
    write_to_file_path <- getwd()
  }
  
  if (!file.exists(file.path(input_file_path, input_file))) {
    stop("Input file not found: ", file.path(input_file_path, input_file))
  }
  
  if (!is.null(dictionary_path) && !file.exists(dictionary_path)) {
    stop("Dictionary file not found: ", dictionary_path)
  }
  
  # Load required libraries (should be in Imports)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readxl", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tidyverse", quietly = TRUE)
  requireNamespace("epiuf", quietly = TRUE)
  
  # Load data
  raw_data <- read.csv(file.path(input_file_path, input_file))
  
  # Load value maps if provided
  if (file.exists(value_maps_path)) {
    source(value_maps_path, local = TRUE)
  }
  
  # Apply dictionary if provided
  if (!is.null(dictionary_path)) {
    epiuf::openDictionary(dictionary_path)
    raw_data <- epiuf::applyDictionary(dictionary = NULL, raw_data)
  }
  
  # Process the data using internal helper functions
  recoded_data <- .process_malta_basic_cleaning(raw_data, reporting_year)
  
  # Create the four tables
  patient <- .create_malta_patient_table(recoded_data)
  isolate <- .create_malta_isolate_table(recoded_data)
  res <- .create_malta_res_table(recoded_data)
  ehrbsi <- .create_malta_ehrbsi_table(recoded_data, reporting_year, episode_duration)
  
  # Create output list
  result <- list(
    ehrbsi = ehrbsi,
    patient = patient,
    isolate = isolate,
    res = res
  )
  
  # Write files if requested
  if (write_to_file) {
    .write_malta_output_files(result, write_to_file_path)
  }
  
  return(result)
}

# Internal helper functions (not exported)
.process_malta_basic_cleaning <- function(raw_data, reporting_year) {
  # Validate required columns exist
  required_cols <- c("HospitalId", "PatientId", "DateOfHospitalAdmission")
  missing_cols <- setdiff(required_cols, names(raw_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Find columns ending with "_noncdm"
  noncdm_cols <- grep("_noncdm$", names(raw_data), value = TRUE)
  
  # Print info about non-CDM columns if they exist
  if (length(noncdm_cols) > 0) {
    cat(length(noncdm_cols), "columns require recoding to match generic coded values\n")
  }
  
  # Recode all dates based on Malta's date formatting
  if (exists("getAnyDictionaryValue")) {
    DateVariables <- getAnyDictionaryValue(varname = "date", search = "type", value = "generic_name")
    raw_data[, DateVariables] <- lapply(raw_data[, DateVariables], function(x) as.Date(x, format = "%d/%m/%Y"))
  } else {
    # Fallback if dictionary function not available
    date_cols <- c("DateOfSpecCollection", "DateOfHospitalAdmission", "DateOfHospitalDischarge", "EpisodeStartDate_noncdm")
    available_date_cols <- intersect(date_cols, names(raw_data))
    raw_data[available_date_cols] <- lapply(
      raw_data[available_date_cols],
      function(x) as.Date(x, format = "%d/%m/%Y")
    )
  }
  
  # Load Malta lookup tables if they exist
  malta_unit_lookup <- if (exists("Malta_UnitSpecialty_Lookup")) {
    setNames(Malta_UnitSpecialty_Lookup$generic_code, Malta_UnitSpecialty_Lookup$malta_code)
  } else NULL
  
  malta_outcome_lookup <- if (exists("Malta_Outcome_Lookup")) {
    setNames(Malta_Outcome_Lookup$generic_code, Malta_Outcome_Lookup$malta_code)
  } else NULL
  
  malta_hosptype_lookup <- if (exists("Malta_HospType_Lookup")) {
    setNames(Malta_HospType_Lookup$hosptype_code, Malta_HospType_Lookup$malta_hosptype)
  } else NULL

  # Malta-specific recoding using temporary, non-CDM vars imported from raw
  recoded_data <- raw_data %>%
    dplyr::mutate(
      DateOfSpecCollection = if ("EpisodeStartDate_noncdm" %in% names(.)) EpisodeStartDate_noncdm else DateOfSpecCollection,
      UnitSpecialtyShort = if ("UnitSpecialtyShort_noncdm" %in% names(.) && !is.null(malta_unit_lookup)) {
        dplyr::recode(UnitSpecialtyShort_noncdm, !!!malta_unit_lookup, .default = UnitSpecialtyShort_noncdm)
      } else UnitSpecialtyShort,
      patientType = if ("patientType_noncdm" %in% names(.)) {
        dplyr::case_when(
          patientType_noncdm == "TRUE" ~ "INPAT",
          patientType_noncdm == "FALSE" & 
            (grepl("OP", sourceLocation_noncdm) | grepl("outpatients", tolower(sourceLocation_noncdm))) ~ "OUTPAT",
          patientType_noncdm == "FALSE" ~ "OTH",
          TRUE ~ NA
        )
      } else "INPAT",
      OutcomeOfCase = if ("OutcomeOfCase_noncdm" %in% names(.) && !is.null(malta_outcome_lookup)) {
        dplyr::case_when(
          OutcomeOfCase_noncdm %in% names(malta_outcome_lookup) ~ malta_outcome_lookup[OutcomeOfCase_noncdm],
          OutcomeOfCase_noncdm != "" ~ "A", # anything else is 'ALIVE'
          TRUE ~ NA
        )
      } else OutcomeOfCase,
      HospitalType = if ("HospitalType_noncdm" %in% names(.) && !is.null(malta_hosptype_lookup)) {
        dplyr::case_when(
          HospitalType_noncdm %in% names(malta_hosptype_lookup) ~ malta_hosptype_lookup[HospitalType_noncdm],
          is.na(HospitalType_noncdm) ~ NA,
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
  recoded_data <- recoded_data %>% 
    dplyr::mutate(
      record_id_bsi = paste0(HospitalId, "-", format(DateOfHospitalAdmission, "%d%m%Y")),
      record_id_patient = paste0(PatientId, "-", format(DateOfHospitalAdmission, "%d%m%Y")),
      record_id_isolate = paste0(PatientId, "-", format(DateOfSpecCollection, "%d%m%Y"))
    )
  
  return(recoded_data)
}

.create_malta_patient_table <- function(recoded_data) {
  patient <- recoded_data %>%
    dplyr::mutate(
      RecordId = record_id_patient,
      ParentId = record_id_bsi,
      DateOfAdmissionCurrentWard = NA, # only have one admission date for overall record
      HospitalisationCode = NA, # Needs to be coded using text/label
      HospitalisationAdmissionCodeSystem = "SNOMED-CT", 
      HospitalisationCodeSystemVersion = NA,  
      HospitalisationAdmissionCodeSystemSpec = NA
    ) %>%
    dplyr::select(
      RecordId, ParentId, UnitId, UnitSpecialtyShort, PatientSpecialty, DateOfAdmissionCurrentWard,
      PatientId, Age, Sex, patientType, 
      DateOfHospitalAdmission, DateOfHospitalDischarge, OutcomeOfCase, 
      HospitalisationCode, HospitalisationCodeLabel,
      HospitalisationAdmissionCodeSystem, HospitalisationCodeSystemVersion,
      HospitalisationAdmissionCodeSystemSpec
    ) %>%
    dplyr::distinct()
  
  return(patient)
}

.create_malta_isolate_table <- function(recoded_data) {
  # Load Malta pathogen lookup table if it exists
  malta_pathogen_lookup <- if (exists("Malta_PathogenCode_Lookup")) {
    setNames(Malta_PathogenCode_Lookup$microorganism_code, Malta_PathogenCode_Lookup$malta_pathogen_name)
  } else NULL
  
  isolate <- recoded_data %>%
    dplyr::mutate(
      RecordId = record_id_isolate,
      ParentId = PatientId,
      LaboratoryCode = NA, # Not included in extract
      Specimen = NA, # Not included in extract
      MicroorganismCode = if (!is.null(malta_pathogen_lookup) && "MicroorganismCodeLabel" %in% names(.)) {
        dplyr::case_when(
          MicroorganismCodeLabel %in% names(malta_pathogen_lookup) ~ malta_pathogen_lookup[MicroorganismCodeLabel],
          !is.na(MicroorganismCodeLabel) ~ paste0("UNMAPPED: ", MicroorganismCodeLabel),
          TRUE ~ NA_character_
        )
      } else NA_character_,
      MicroorganismCodeSystem = "SNOMED-CT",
      MicroorganismCodeSystemSpec = NA, # always NA
      MicroorganismCodeSystemVersion = NA # as above - to be decided
    ) %>%
    dplyr::select(
      RecordId, ParentId, DateOfSpecCollection, LaboratoryCode, IsolateId, Specimen,
      MicroorganismCode, MicroorganismCodeLabel, MicroorganismCodeSystem, MicroorganismCodeSystemSpec,
      MicroorganismCodeSystemVersion
    ) %>%
    dplyr::distinct()
  
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
    dplyr::filter(SIR != "") %>% # remove empty records, where no sensitivity result was reported for that antibiotic
    dplyr::distinct()
  
  return(res)
}

.create_malta_ehrbsi_table <- function(recoded_data, reporting_year, episode_duration) {
  ehrbsi <- recoded_data %>%
    dplyr::mutate(
      AggregationLevel = "HOSP",
      ClinicalTerminology = "SNOMED-CT",
      ClinicalTerminologySpec = NA,
      DataSource = "MT-EHRBSI",
      DateUsedForStatistics = reporting_year,
      EpisodeDuration = episode_duration, # selected as 'usual', episode function should adapt to this
      ESurvBSI = NA, # level of automation? Full/semi/denom/manual/etc
      GeoLocation = NA, # Can we get NUT2?
      HospitalId = HospitalId,
      HospitalSize = NA, # how many beds?
      HospitalType = HospitalType,
      LaboratoryCode = NA, # will not be provided
      MicrobiologicalTerminology = "SNOMED-CT",
      MicrobiologicalTerminologySpec = NA,
      NumberOfBloodCultureSets = NA, # Andrea - provide data for prev year #'s blood culture sets
      NumberOfHOHABSIs = NA, # TO BE CALCULATED ONCE EPISODES CALC'D
      NumberOfHospitalDischarges = NA,  # Ask for denoms
      NumberOfHospitalPatientDays = NA, # Ask for denoms
      NumberOfImportedHABSIs = NA, # TO BE CALCULATED ONCE EPISODES CALC'D
      NumberOfTotalBSIs = NA, # TO BE CALCULATED ONCE EPISODES CALC'D
      ProportionPopulationCovered = NA, # Request coverage estimate
      RecordId = record_id_bsi,
      RecordType = "EHRBSI",
      RecordTypeVersion = NA,
      ReportingCountry = "MT",
      Status = "New/Update",
      Subject = "EHRBSI"
    ) %>%
    dplyr::select(
      AggregationLevel, ClinicalTerminology, ClinicalTerminologySpec, DataSource,
      DateUsedForStatistics, EpisodeDuration, ESurvBSI, GeoLocation, HospitalId,
      HospitalSize, HospitalType, LaboratoryCode, MicrobiologicalTerminology,
      MicrobiologicalTerminologySpec, NumberOfBloodCultureSets, NumberOfHOHABSIs,
      NumberOfHospitalDischarges, NumberOfHospitalPatientDays, NumberOfImportedHABSIs,
      NumberOfTotalBSIs, ProportionPopulationCovered, RecordId, RecordType,
      RecordTypeVersion, ReportingCountry, Status, Subject
    ) %>%
    dplyr::distinct()
  
  return(ehrbsi)
}

.write_malta_output_files <- function(result_list, output_path) {
  # Write RDS files to the specified path
  saveRDS(result_list$ehrbsi, file.path(output_path, "1.EHRBSI.rds"))
  saveRDS(result_list$patient, file.path(output_path, "2.EHRBSI$Patient.rds"))
  saveRDS(result_list$isolate, file.path(output_path, "3.EHRBSI$Patient$Isolate.rds"))
  saveRDS(result_list$res, file.path(output_path, "4.EHRBSI$Patient$Isolate$Res.rds"))
}

