#' Process BSI data from raw format to EHR-BSI format for multiple countries
#'
#' @param country Country code ("MT" for Malta, "EE" for Estonia)
#' @param input_data Data frame containing the raw BSI data to be processed
#' @param dictionary_path Path to the data dictionary Excel file
#' @param value_maps_path Path to the value maps R script
#' @param reporting_year Year for the DateUsedForStatistics field, defaults to current year
#' @param episode_duration Duration for episode calculation in days, defaults to 14
#' @param write_to_file Whether to write output files to disk
#' @param write_to_file_path Path for output files, defaults to working directory
#' @param return_format Whether to return "list" (default) or "separate" objects
#' @param calculate_episodes Whether to calculate episodes and aggregate results
#'
#' @return Returns a list containing the four EHR-BSI data tables: ehrbsi, patient, isolate, res
#' @export
#'
#' @examples
#' \dontrun{
#' # Load Malta data
#' malta_data <- read.csv("Malta/data/raw/BSI_REPORT_Malta.csv")
#' 
#' # Process Malta data
#' result_malta <- process_country_bsi(
#'   country = "MT",
#'   input_data = malta_data,
#'   write_to_file = TRUE,
#'   write_to_file_path = "Malta/data/formatted"
#' )
#' 
#' # Load Estonia data
#' estonia_data <- readxl::read_xlsx("Estonia/data/raw/BSI_REPORT_2024_share.xlsx")
#' 
#' # Process Estonia data
#' result_estonia <- process_country_bsi(
#'   country = "EE",
#'   input_data = estonia_data,
#'   write_to_file = TRUE,
#'   write_to_file_path = "Estonia/data/formatted"
#' )
#' }

# 
# # The generic function automatically handles:
# # - Country-specific dictionary loading from reference/dictionaries/
# # - Fixed metadata and commensal file paths in reference/
# # - Loading CSV for Malta, Excel for Estonia
# # - Calling the correct internal helper functions
# # - Using the appropriate file writing functions

process_country_bsi <- function(country,
                               input_data,
                               dictionary_path = NULL,
                               value_maps_path = "reference/Lookup_Tables.R",
                               write_to_file = FALSE,
                               write_to_file_path = NULL,
                               return_format = "list",
                               episode_duration = 14,
                               calculate_episodes = TRUE) {
  
  # Validate country parameter using config system
  tryCatch({
    # Just validate that config exists; it will be loaded in country-specific functions
    get_country_config(country)
  }, error = function(e) {
    stop("Invalid country code: ", country, ". ", e$message, call. = FALSE)
  })
  
  # Validate input data
  if (is.null(input_data)) {
    stop("input_data must be a valid data frame or list of data frames", call. = FALSE)
  }
  
  # Set default dictionary path if not provided
  if (is.null(dictionary_path)) {
    dictionary_path <- file.path("reference", "dictionaries", paste0(country, ".xlsx"))
    
    # Check if country dictionary exists
    if (!file.exists(dictionary_path)) {
      warning("No country dictionary found at: ", dictionary_path, 
              ". Proceeding without dictionary.", call. = FALSE)
      dictionary_path <- NULL
    }
  }
  
  # Set fixed metadata and commensal paths
  metadata_path <- "reference/Metadata.xlsx"
  commensal_path <- "reference/CommonCommensals.csv"
  
  # Parameter validation for file paths
  if (is.null(write_to_file_path)) {
    write_to_file_path <- getwd()
  }
  
  # Validate reference file paths
  if (!is.null(dictionary_path) && !file.exists(dictionary_path)) {
    warning("Dictionary file not found: ", dictionary_path, call. = FALSE)
    dictionary_path <- NULL
  }
  
  if (!file.exists(metadata_path)) {
    warning("Metadata file not found: ", metadata_path, 
            ". HAI short codes will not be applied.", call. = FALSE)
    metadata_path <- NULL
  }
  
  if (!file.exists(commensal_path)) {
    warning("Commensals file not found: ", commensal_path, 
            ". Episode calculation may be affected.", call. = FALSE)
  }
  
  # Load required libraries (should be in Imports)
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("readxl", quietly = TRUE)
  requireNamespace("stringr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  
  # Load epiuf for dictionary support if needed
  if (!is.null(dictionary_path) && country == "MT") {
    requireNamespace("tidyverse", quietly = TRUE)
    requireNamespace("epiuf", quietly = TRUE)
  }
  
  # Use input data directly
  raw_data <- input_data
  
  # Load value maps if provided
  if (file.exists(value_maps_path)) {
    source(value_maps_path, local = TRUE)
  }
  
  # Apply dictionary if provided
  if (!is.null(dictionary_path) && requireNamespace("epiuf", quietly = TRUE)) {
    tryCatch({
      epiuf::openDictionary(dictionary_path)
      raw_data <- epiuf::applyDictionary(dictionary = NULL, raw_data)
    }, error = function(e) {
      warning("Dictionary application failed: ", e$message, 
              ". Proceeding with raw data.", call. = FALSE)
    })
  }
  
  # Process the data using country-specific internal helper functions
  # These are now simplified wrappers around unified functions
  if (country == "MT") {
    recoded_data <- .process_malta_basic_cleaning(raw_data)
    
    # Create the four tables
    patient <- .create_malta_patient_table(recoded_data)
    isolate <- .create_malta_isolate_table(recoded_data)
    res <- .create_malta_res_table(recoded_data)
    ehrbsi <- .create_malta_ehrbsi_table(recoded_data, episode_duration)
    
  } else if (country == "EE") {
    recoded_data <- .process_estonia_basic_cleaning(raw_data)
    
    # Create the four tables
    patient <- .create_estonia_patient_table(recoded_data)
    isolate <- .create_estonia_isolate_table(recoded_data)
    res <- .create_estonia_res_table(recoded_data, metadata_path)
    ehrbsi <- .create_estonia_ehrbsi_table(recoded_data, episode_duration)
  } else {
    # Future countries can be added here with minimal code
    stop("Country '", country, "' is not yet implemented. ",
         "Please add implementation for this country.", call. = FALSE)
  }
  
  if(calculate_episodes){
    
    # Load commensal data based on file extension
    file_ext <- tools::file_ext(commensal_path)
    if (file_ext %in% c("xlsx", "xls")) {
      commensal_df <- readxl::read_xlsx(commensal_path)
    } else {
      commensal_df <- read.csv(commensal_path)
    }
    
    # Create a dataset with distinct episodes, dates of onset, origin of case etc
    eps_df <- calculateEpisodes(patient, isolate, commensal_df, episode_duration)
    
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
