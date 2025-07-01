#' Process Estonia BSI data from raw format to EHR-BSI format
#'
#' @param input_file Name of the input Excel file, defaults to "BSI_REPORT_2024_share.xlsx"
#' @param input_file_path Path to the input file, defaults to the working directory
#' @param dictionary_path Path to the data dictionary Excel file
#' @param value_maps_path Path to the value maps R script
#' @param metadata_path Path to the metadata Excel file for antibiotic recoding
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
#' result <- process_estonia_bsi(
#'   input_file_path = "Estonia/data/raw",
#'   dictionary_path = "Estonia/data/reference/dictionary_raw_BSI_Estonia.xlsx",
#'   write_to_file = TRUE,
#'   write_to_file_path = "Estonia/data/formatted"
#' )
#' }
process_estonia_bsi <- function(input_file = "BSI_REPORT_2024_share.xlsx",
                               input_file_path = NULL,
                               dictionary_path = NULL,
                               value_maps_path = "reference/raw_BSI_valueMaps.r",
                               metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx",
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
  
  # Load data
  raw_data <- readxl::read_xlsx(file.path(input_file_path, input_file))
  
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
  recoded_data <- .process_basic_cleaning(raw_data, reporting_year)
  
  # Create the four tables
  patient <- .create_patient_table(recoded_data)
  isolate <- .create_isolate_table(recoded_data)
  res <- .create_res_table(recoded_data, metadata_path)
  ehrbsi <- .create_ehrbsi_table(recoded_data, reporting_year, episode_duration)
  
  # Create output list
  result <- list(
    ehrbsi = ehrbsi,
    patient = patient,
    isolate = isolate,
    res = res
  )
  
  # Write files if requested
  if (write_to_file) {
    .write_output_files(result, write_to_file_path)
  }
  
  return(result)
}

# Internal helper functions (not exported)
.process_basic_cleaning <- function(raw_data, reporting_year) {
  # Validate required columns exist
  required_cols <- c("DateOfSpecCollection", "DateOfHospitalAdmission", 
                    "HospitalId", "PatientId", "IsolateId")
  missing_cols <- setdiff(required_cols, names(raw_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create date-time strings for IDs
  recoded_data <- raw_data %>%
    dplyr::mutate(
      DateOfSpecCollection = as.POSIXct(DateOfSpecCollection, format = "%d/%m/%Y %H:%M"),
      sample_date_time = paste0(
        format(DateOfSpecCollection, "%d%m%Y"), "_",
        format(DateOfSpecCollection, "%H_%M")
      ),
      DateOfHospitalAdmission = as.POSIXct(DateOfHospitalAdmission, format = "%d/%m/%Y %H:%M"),
      admit_date_time = paste0(
        format(DateOfHospitalAdmission, "%d%m%Y"), "_",
        format(DateOfHospitalAdmission, "%H_%M")
      )
    )
  
  # Create dataset IDs
  recoded_data <- recoded_data %>%
    dplyr::mutate(
      record_id_bsi = paste0(HospitalId, "-", admit_date_time),
      record_id_patient = paste0(PatientId, "-", admit_date_time)
    ) %>%
    dplyr::select(-admit_date_time, -sample_date_time)
  
  # Recode dates
  if (exists("getAnyDictionaryValue")) {
    DateVariables <- getAnyDictionaryValue(varname = "date", search = "type", value = "generic_name")
    recoded_data[, DateVariables] <- lapply(
      recoded_data[, DateVariables], 
      function(x) as.Date(x, format = "%d/%m/%Y %H:%M")
    )
  } else {
    # Fallback if dictionary function not available
    date_cols <- c("DateOfSpecCollection", "DateOfHospitalAdmission", "DateOfHospitalDischarge")
    available_date_cols <- intersect(date_cols, names(recoded_data))
    recoded_data[available_date_cols] <- lapply(
      recoded_data[available_date_cols],
      function(x) as.Date(x, format = "%d/%m/%Y %H:%M")
    )
  }
  
  return(recoded_data)
}

.create_patient_table <- function(recoded_data) {
  patient <- recoded_data %>%
    dplyr::mutate(
      RecordId = record_id_patient,
      ParentId = record_id_bsi,
      UnitId = paste0(HospitalId, "_", UnitSpecialtyShort),
      PatientSpecialty = NA_character_,
      patientType = "INPAT",
      OutcomeOfCase = NA_character_,
      HospitalisationCode = NA_character_,
      HospitalisationCodeLabel = NA_character_,
      HospitalisationAdmissionCodeSystem = "ICD-10",
      HospitalisationCodeSystemVersion = NA_character_,
      HospitalisationAdmissionCodeSystemSpec = NA_character_
    ) %>%
    dplyr::select(
      RecordId, ParentId, UnitId, UnitSpecialtyShort, PatientSpecialty, 
      DateOfAdmissionCurrentWard, PatientId, Age, Sex, patientType,
      DateOfHospitalAdmission, DateOfHospitalDischarge, OutcomeOfCase,
      HospitalisationCode, HospitalisationCodeLabel,
      HospitalisationAdmissionCodeSystem, HospitalisationCodeSystemVersion,
      HospitalisationAdmissionCodeSystemSpec, HospitalId
    ) %>%
    dplyr::distinct() %>%
    dplyr::arrange(PatientId, DateOfHospitalAdmission) %>%
    dplyr::group_by(PatientId) %>%
    dplyr::mutate(
      gap_days = as.numeric(
        difftime(DateOfHospitalAdmission, dplyr::lag(DateOfHospitalAdmission), units = "days")
      ),
      prev_HospitalId = dplyr::lag(HospitalId),
      PreviousAdmission = dplyr::case_when(
        (gap_days > 0 & gap_days <= 3) & (HospitalId == prev_HospitalId) ~ "CURR",
        (gap_days > 0 & gap_days <= 3) & (HospitalId != prev_HospitalId) ~ "OHOSP",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-gap_days, -prev_HospitalId, -HospitalId)
  
  return(patient)
}

.create_isolate_table <- function(recoded_data) {
  # Implement isolate table creation logic
}

.create_res_table <- function(recoded_data, metadata_path) {
  # Implement resistance table creation logic
  # Include the antibiotic recoding functionality
}

.create_ehrbsi_table <- function(recoded_data, reporting_year, episode_duration) {
  # Implement BSI table creation logic
}

.write_output_files <- function(result_list, output_path) {
  # Write RDS files to the specified path
  saveRDS(result_list$ehrbsi, file.path(output_path, "1.EHRBSI.rds"))
  saveRDS(result_list$patient, file.path(output_path, "2.EHRBSI$Patient.rds"))
  saveRDS(result_list$isolate, file.path(output_path, "3.EHRBSI$Patient$Isolate.rds"))
  saveRDS(result_list$res, file.path(output_path, "4.EHRBSI$Patient$Isolate$Res.rds"))
} 