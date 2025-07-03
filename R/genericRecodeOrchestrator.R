#' Process BSI data from raw format to EHR-BSI format for multiple countries
#'
#' @param country Country code ("MT" for Malta, "EE" for Estonia)
#' @param input_file Name of the input file, defaults based on country
#' @param input_file_path Path to the input file, defaults to the working directory
#' @param dictionary_path Path to the data dictionary Excel file
#' @param value_maps_path Path to the value maps R script
#' @param metadata_path Path to the metadata Excel file for antibiotic recoding (Estonia only)
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
#' # Process Malta data
#' result_malta <- process_country_bsi(
#'   country = "MT",
#'   input_file_path = "Malta/data/raw",
#'   dictionary_path = "Malta/data/reference/dictionary_raw_BSI_Malta.xlsx",
#'   write_to_file = TRUE,
#'   write_to_file_path = "Malta/data/formatted"
#' )
#' 
#' # Process Estonia data
#' result_estonia <- process_country_bsi(
#'   country = "EE",
#'   input_file_path = "Estonia/data/raw",
#'   dictionary_path = "Estonia/data/reference/dictionary_raw_BSI_Estonia.xlsx",
#'   metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx",
#'   write_to_file = TRUE,
#'   write_to_file_path = "Estonia/data/formatted"
#' )
#' }

# 
# # The generic function automatically handles:
# # - Setting appropriate default input file names
# # - Loading CSV for Malta, Excel for Estonia
# # - Calling the correct internal helper functions
# # - Using the appropriate file writing functions

process_country_bsi <- function(country,
                               input_file = NULL,
                               input_file_path = NULL,
                               dictionary_path = NULL,
                               value_maps_path = "reference/Lookup_Tables.r",
                               metadata_path = NULL,
                               reporting_year = as.numeric(format(Sys.Date(), "%Y")),
                               episode_duration = 14,
                               write_to_file = FALSE,
                               write_to_file_path = NULL,
                               return_format = "list",
                               calculate_episodes = TRUE) {
  
  # Validate country parameter
  if (!country %in% c("MT", "EE")) {
    stop("Country must be either 'MT' (Malta) or 'EE' (Estonia)")
  }
  
  # Set country-specific defaults
  if (is.null(input_file)) {
    input_file <- switch(country,
      "MT" = "BSI_REPORT_Malta.csv",
      "EE" = "BSI_REPORT_2024_share.xlsx"
    )
  }
  
  # Set default dictionary path if not provided
  if (is.null(dictionary_path)) {
    dictionary_path <- switch(country,
      "MT" = "reference/dictionary_raw_BSI_Malta.xlsx",
      "EE" = "reference/dictionary_raw_BSI_Estonia.xlsx"
    )
  }
  
  # Set default metadata path for Estonia if not provided
  if (is.null(metadata_path)) {
    metadata_path <- "reference/MetaDataSet_57 (2025-03-13).xlsx"
  }
  
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
  
  if (!is.null(metadata_path) && !file.exists(metadata_path)) {
    stop("Metadata file not found: ", metadata_path)
  }
  
  # Load required libraries (should be in Imports)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readxl", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  if (country == "MT") {
    requireNamespace("tidyverse", quietly = TRUE)
    requireNamespace("epiuf", quietly = TRUE)
  }
  
  # Load data based on file extension or country
  file_ext <- tools::file_ext(input_file)
  if (file_ext %in% c("xlsx", "xls")) {
    raw_data <- readxl::read_xlsx(file.path(input_file_path, input_file))
  } else {
    raw_data <- read.csv(file.path(input_file_path, input_file))
  }
  
  # Load value maps if provided
  if (file.exists(value_maps_path)) {
    source(value_maps_path, local = TRUE)
  }
  
  # Apply dictionary if provided
  if (!is.null(dictionary_path)) {
    epiuf::openDictionary(dictionary_path)
    raw_data <- epiuf::applyDictionary(dictionary = NULL, raw_data)
  }
  
  # Process the data using country-specific internal helper functions
  if (country == "MT") {
    recoded_data <- .process_malta_basic_cleaning(raw_data, reporting_year)
    
    # Create the four tables
    patient <- .create_malta_patient_table(recoded_data)
    isolate <- .create_malta_isolate_table(recoded_data)
    res <- .create_malta_res_table(recoded_data)
    ehrbsi <- .create_malta_ehrbsi_table(recoded_data, reporting_year, episode_duration)
    
  } else if (country == "EE") {
    recoded_data <- .process_estonia_basic_cleaning(raw_data, reporting_year)
    
    # Create the four tables
    patient <- .create_estonia_patient_table(recoded_data)
    isolate <- .create_estonia_isolate_table(recoded_data)
    res <- .create_estonia_res_table(recoded_data, metadata_path)
    ehrbsi <- .create_estonia_ehrbsi_table(recoded_data, reporting_year, episode_duration)
  }
  
  if(calculate_episodes){
    # Create a dataset with distinct episodes, dates of onset, origin of case etc
    eps_df <- calculateEpisodes(patient, isolate, commensal_df, episodeDuration = 14)
    
    # Aggregate to ehrbsi level
    ehrbsi <- aggregateEpisodes(eps_df,ehrbsi)
  }
  
  
  # Create output list
  result <- list(
    ehrbsi = ehrbsi,
    patient = patient,
    isolate = isolate,
    res = res
  )
  
  
  # Write files if requested using country-specific function
  if (write_to_file) {
    saveReportingTemplate(result,country)
  }
  
  return(result)
} 