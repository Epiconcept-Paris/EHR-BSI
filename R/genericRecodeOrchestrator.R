#' Process BSI data from raw format to EHR-BSI format for multiple countries
#'
#' @param country Country code ("MT" for Malta, "EE" for Estonia)
#' @param input_data Data frame containing the raw BSI data to be processed
#' @param dictionary_path Path to the data dictionary Excel file
#' @param reporting_year Year for the DateUsedForStatistics field, defaults to current year
#' @param episode_duration Duration for episode calculation in days, defaults to 14
#' @param aggregation_level Level of aggregation for EHRBSI table. Options: "HOSP" (hospital), 
#'   "HOSP-YEAR" (hospital-year), "LAB" (laboratory), "LAB-YEAR" (laboratory-year). Defaults to "HOSP".
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
# # - Lookup tables loaded from Excel files
# # - Calling the correct internal helper functions
# # - Using the appropriate file writing functions

process_country_bsi <- function(country,
                               input_data,
                               dictionary_path = NULL,
                               write_to_file = FALSE,
                               write_to_file_path = NULL,
                               return_format = "list",
                               episode_duration = 14,
                               aggregation_level = "HOSP",
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
  
  # Validate aggregation_level
  valid_agg_levels <- c("HOSP", "HOSP-YEAR", "LAB", "LAB-YEAR")
  if (!aggregation_level %in% valid_agg_levels) {
    stop("Invalid aggregation_level: ", aggregation_level, 
         ". Must be one of: ", paste(valid_agg_levels, collapse = ", "), 
         call. = FALSE)
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
  
  # Use input data directly
  raw_data <- input_data
  
  # Apply dictionary column renaming if provided
  if (!is.null(dictionary_path)) {
    tryCatch({
      raw_data <- apply_dictionary_from_excel(raw_data, dictionary_path)
    }, error = function(e) {
      warning("Dictionary application failed: ", e$message, 
              ". Proceeding with raw data.", call. = FALSE)
    })
  }
  
  # Process the data using unified config-driven approach
  # This works for all countries with proper configuration
  result <- process_country_generic(raw_data, country, episode_duration, metadata_path, aggregation_level)
  patient <- result$patient
  isolate <- result$isolate
  res <- result$res
  ehrbsi <- result$ehrbsi
  
  if(calculate_episodes){
    
    # Load commensal data based on file extension
    file_ext <- tools::file_ext(commensal_path)
    if (file_ext %in% c("xlsx", "xls")) {
      commensal_df <- readxl::read_xlsx(commensal_path)
    } else {
      commensal_df <- read.csv(commensal_path)
    }
    
    # Create a dataset with distinct episodes, dates of onset, origin of case etc
    eps_result <- calculateEpisodes(patient, isolate, commensal_df, episode_duration)
    
    # Extract episodes data frame from the returned list
    eps_df <- if (is.list(eps_result) && "episodes" %in% names(eps_result)) {
      result$episode_summary <- eps_result$episode_summary  # Store episode_summary in result
      eps_result$episodes
    } else {
      # Backward compatibility: if it returns just a data frame
      eps_result
    }
    
    # Extract hospital-to-lab mapping BEFORE deduplication (if needed for LAB aggregation)
    hospital_lab_map <- NULL
    if (aggregation_level %in% c("LAB", "LAB-YEAR") && "LaboratoryCode" %in% names(ehrbsi)) {
      hospital_lab_map <- ehrbsi %>%
        dplyr::select(HospitalId, LaboratoryCode) %>%
        dplyr::distinct()
    }
    
    # Aggregate to ehrbsi level
    ehrbsi <- aggregateEpisodes(eps_df, ehrbsi, aggregation_level, hospital_lab_map)
    
    # Deduplicate ehrbsi AFTER aggregation
    # This is necessary when multiple hospitals map to the same lab
    if (nrow(ehrbsi) > 0) {
      ehrbsi <- ehrbsi %>%
        dplyr::group_by(RecordId) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    }
  } else {
    # If not calculating episodes, still need to deduplicate for LAB aggregation
    if (aggregation_level %in% c("LAB", "LAB-YEAR") && nrow(ehrbsi) > 0) {
      ehrbsi <- ehrbsi %>%
        dplyr::group_by(RecordId) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
    }
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
