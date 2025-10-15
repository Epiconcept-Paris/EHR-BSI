#' Process Estonia BSI data from raw format to EHR-BSI format
#'
#' @param raw_data Received from genericRecodeOrchestrator.R
#' @note The reporting year is automatically derived from the admission dates in the data
#'
#' @return Returns a list containing the four EHR-BSI data tables: ehrbsi, patient, isolate, res
#' @export
#'

# Internal helper functions (not exported) - simplified to use unified functions
.process_estonia_basic_cleaning <- function(raw_data) {
  # Load Estonia configuration
  config <- get_country_config("EE")
  
  # Use unified cleaning function
  # Date parsing is now handled by process_basic_cleaning based on config
  recoded_data <- process_basic_cleaning(raw_data, config, "EE")
  
  return(recoded_data)
}

.create_estonia_patient_table <- function(recoded_data) {
  # Load config
  config <- get_country_config("EE")
  
  # Create base patient table using shared function
  patient <- create_standard_patient_table(recoded_data, config = config)
  
  # Apply Estonia-specific PreviousAdmission logic
  # This is now handled by the field_transforms in config, but we ensure it's applied
  if ("PreviousAdmission" %in% names(config$field_transforms)) {
    patient <- config$field_transforms$PreviousAdmission(patient)
  }
  
  # Apply Estonia-specific UnitId logic
  if ("UnitId" %in% names(config$field_transforms)) {
    unit_id_result <- config$field_transforms$UnitId(patient)
    if (length(unit_id_result) == nrow(patient)) {
      patient$UnitId <- unit_id_result
    }
  }
  
  # Finalize table with standard column selection
  patient <- finalize_table(patient, get_standard_table_columns("patient"))
  
  return(patient)
}

.create_estonia_isolate_table <- function(recoded_data) {
  # Load config
  config <- get_country_config("EE")
  
  # Create base isolate table using shared function
  isolate <- create_standard_isolate_table(recoded_data, config = config)
  
  # Finalize table with standard column selection
  isolate <- finalize_table(isolate, get_standard_table_columns("isolate"))
  
  return(isolate)
}

.create_estonia_res_table <- function(recoded_data, metadata_path = NULL) {
  # Load config
  config <- get_country_config("EE")
  
  # Use unified resistance table creation function
  # This now handles all the Estonia-specific processing:
  # - Long format detection
  # - Test type classification
  # - Antibiotic name translation chain (EST2ENG -> ENG2HAI)
  # - Mechanism resistance mapping
  res <- create_standard_res_table(recoded_data, config, "EE", metadata_path)
  
  return(res)
}

.create_estonia_ehrbsi_table <- function(recoded_data, episode_duration) {
  # Load config
  config <- get_country_config("EE")
  
  # Create base EHRBSI table using shared function with config
  # This now handles:
  # - Terminology systems from config
  # - HospitalType and GeoLocation lookups
  # - Estonia-specific defaults (ESurvBSI = 2, ProportionPopulationCovered = 1)
  ehrbsi <- create_base_ehrbsi_table(recoded_data, "EE", episode_duration, 
                                     record_id_col = "record_id_bsi", config = config)
  
  # Finalize table with standard column selection
  ehrbsi <- finalize_table(ehrbsi, get_standard_table_columns("ehrbsi"))
  
  return(ehrbsi)
}
