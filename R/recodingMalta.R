#' Process Malta BSI data from raw format to EHR-BSI format
#'
#' @param raw_data Received from genericRecodeOrchestrator.R
#'
#' @return Returns a list containing the four EHR-BSI data tables: ehrbsi, patient, isolate, res
#' @export
#'


# Internal helper functions (not exported) - simplified to use unified functions
.process_malta_basic_cleaning <- function(raw_data) {
  # Load Malta configuration
  config <- get_country_config("MT")
  
  # Use unified cleaning function
  recoded_data <- process_basic_cleaning(raw_data, config, "MT")
  
  return(recoded_data)
}

.create_malta_patient_table <- function(recoded_data) {
  # Load config
  config <- get_country_config("MT")
  
  # Create base patient table using shared function with Malta config
  patient <- create_standard_patient_table(recoded_data, config = config)
  
  # Finalize table with standard column selection
  patient <- finalize_table(patient, get_standard_table_columns("patient"))
  
  return(patient)
}

.create_malta_isolate_table <- function(recoded_data) {
  # Load config
  config <- get_country_config("MT")
  
  # Create base isolate table using shared function
  isolate <- create_standard_isolate_table(recoded_data, config = config)
  
  # Note: Pathogen code mapping is now handled in process_basic_cleaning
  # via the PathogenCode lookup mapping in config
  
  # Finalize table with standard column selection
  isolate <- finalize_table(isolate, get_standard_table_columns("isolate"))
  
  return(isolate)
}

.create_malta_res_table <- function(recoded_data) {
  # Load config
  config <- get_country_config("MT")
  
  # Use unified resistance table creation function
  res <- create_standard_res_table(recoded_data, config, "MT", metadata_path = NULL)
  
  return(res)
}

.create_malta_ehrbsi_table <- function(recoded_data, episode_duration) {
  # Load config
  config <- get_country_config("MT")
  
  # Create base EHRBSI table using shared function with config
  ehrbsi <- create_base_ehrbsi_table(recoded_data, "MT", episode_duration, 
                                     record_id_col = "record_id_bsi", config = config)
  
  # Finalize table with standard column selection
  ehrbsi <- finalize_table(ehrbsi, get_standard_table_columns("ehrbsi"))
  
  return(ehrbsi)
}
