#' Common functions for country-specific BSI data recoding
#'
#' This file contains shared functions used by both Estonia and Malta
#' recoding scripts to avoid code duplication and ensure consistency.

#' Validate that required columns exist in the data (Enhanced)
#'
#' @param data Data frame to validate
#' @param required_cols Character vector of required column names
#' @param context_name String describing the context for error messages
#'
#' @return Nothing if validation passes, throws error if columns missing
validate_required_columns <- function(data, required_cols, context_name = "dataset") {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    # Try to suggest corrections for common typos
    available_cols <- names(data)
    suggestions <- sapply(missing_cols, function(miss) {
      # Find closest match using simple string distance
      distances <- adist(tolower(miss), tolower(available_cols))
      closest <- available_cols[which.min(distances)]
      if (min(distances) <= 3) {  # Allow up to 3 character differences
        return(paste0(miss, " (did you mean '", closest, "'?)"))
      } else {
        return(miss)
      }
    })
    
    stop("Missing required columns in ", context_name, ": ", 
         paste(suggestions, collapse = ", "),
         call. = FALSE)
  }
  
  # Report data quality issues
  for (col in required_cols) {
    if (all(is.na(data[[col]]))) {
      warning("Column '", col, "' in ", context_name, " contains only NA values", 
              call. = FALSE)
    }
  }
}

#' Parse date columns with dictionary fallback (Enhanced)
#'
#' @param data Data frame containing date columns
#' @param fallback_cols Character vector of date column names to use if dictionary unavailable
#' @param date_format Format string for date parsing (can include time)
#' @param preserve_time Whether to preserve time component (returns POSIXct instead of Date)
#'
#' @return Data frame with parsed date columns
parse_dates_with_fallback <- function(data, fallback_cols, date_format = "%d/%m/%Y", 
                                     preserve_time = FALSE) {
  if (exists("getAnyDictionaryValue")) {
    # Use dictionary if available
    date_variables <- getAnyDictionaryValue(varname = "date", search = "type", value = "generic_name")
    available_date_cols <- intersect(date_variables, names(data))
  } else {
    # Fallback to specified columns
    available_date_cols <- intersect(fallback_cols, names(data))
  }
  
  if (length(available_date_cols) > 0) {
    for (col in available_date_cols) {
      tryCatch({
        if (preserve_time || grepl("%H|%M|%S", date_format)) {
          # Parse as POSIXct for datetime
          data[[col]] <- as.POSIXct(data[[col]], format = date_format)
        } else {
          # Parse as Date for date-only
          data[[col]] <- as.Date(data[[col]], format = date_format)
        }
      }, error = function(e) {
        warning("Failed to parse date column '", col, "': ", e$message, call. = FALSE)
      })
    }
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
  
  # Use data.table::copy() to ensure we don't modify package data
  lookup_copy <- data.table::copy(lookup_table)
  return(setNames(lookup_copy[[value_col]], lookup_copy[[key_col]]))
}

#' Create base EHRBSI table with common fields (Enhanced)
#'
#' @param data Source data frame
#' @param country_code Two-letter country code (e.g., "EE", "MT")
#' @param episode_duration Episode duration in days
#' @param record_id_col Name of column containing record IDs
#' @param config Country configuration object (optional)
#'
#' @return Data frame with base EHRBSI structure
create_base_ehrbsi_table <- function(data, country_code, episode_duration, 
                                    record_id_col = "record_id_bsi", config = NULL) {
  # Get config if not provided
  if (is.null(config)) {
    config <- get_country_config(country_code)
  }
  
  term <- config$terminology
  
  base_ehrbsi <- data %>%
    dplyr::mutate(
      AggregationLevel = "HOSP",
      DataSource = paste0(country_code, "-EHRBSI"),
      DateUsedForStatistics = format(DateOfSpecCollection, "%Y"),
      EpisodeDuration = episode_duration,
      HospitalId = HospitalId,
      LaboratoryCode = NA_character_,
      MicrobiologicalTerminology = term$microbiological,
      MicrobiologicalTerminologySpec = term$microbiological_spec,
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
      Subject = "EHRBSI",
      ClinicalTerminology = term$clinical,
      ClinicalTerminologySpec = term$clinical_spec
    )
  
  # Apply country-specific defaults
  if (!is.null(config$defaults$ehrbsi)) {
    for (field in names(config$defaults$ehrbsi)) {
      if (field %in% names(base_ehrbsi)) {
        base_ehrbsi[[field]] <- config$defaults$ehrbsi[[field]]
      } else {
        base_ehrbsi <- base_ehrbsi %>%
          dplyr::mutate(!!field := config$defaults$ehrbsi[[field]])
      }
    }
  }
  
  # Apply lookup-based fields (like HospitalType, GeoLocation)
  lookups <- get_country_lookups(country_code)
  for (lookup_name in names(config$lookup_mappings)) {
    mapping_config <- config$lookup_mappings[[lookup_name]]
    if (mapping_config$output_column %in% c("HospitalType", "GeoLocation") && 
        lookup_name %in% names(lookups)) {
      lookup_table <- lookups[[lookup_name]]
      lookup_vec <- create_lookup_vector(lookup_table, mapping_config$to, mapping_config$from)
      
      if (mapping_config$column %in% names(base_ehrbsi)) {
        base_ehrbsi[[mapping_config$output_column]] <- dplyr::recode(
          base_ehrbsi[[mapping_config$column]], 
          !!!lookup_vec,
          .default = if(!is.null(mapping_config$fallback)) mapping_config$fallback else base_ehrbsi[[mapping_config$column]]
        )
      }
    }
  }
  
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

#' Create standard patient table structure (Enhanced)
#'
#' @param data Source data frame
#' @param record_id_col Name of patient record ID column
#' @param parent_id_col Name of parent (BSI) record ID column
#' @param country_defaults List of country-specific default values
#' @param config Country configuration object (optional)
#'
#' @return Data frame with standard patient table structure
create_standard_patient_table <- function(data, record_id_col = "record_id_patient",
                                         parent_id_col = "record_id_bsi",
                                         country_defaults = list(), config = NULL) {
  
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
  
  # Override with config defaults if provided
  if (!is.null(config) && !is.null(config$defaults$patient)) {
    defaults <- modifyList(defaults, config$defaults$patient)
  }
  
  # Override with explicit country-specific defaults
  defaults <- modifyList(defaults, country_defaults)
  
  # Create base patient table
  patient <- data %>%
    dplyr::mutate(
      RecordId = if(record_id_col %in% names(.)) .data[[record_id_col]] else NA_character_,
      ParentId = if(parent_id_col %in% names(.)) .data[[parent_id_col]] else NA_character_,
      PatientSpecialty = if("PatientSpecialty" %in% names(.)) PatientSpecialty else defaults$PatientSpecialty,
      patientType = if("patientType" %in% names(.)) patientType else defaults$patientType,
      OutcomeOfCase = if("OutcomeOfCase" %in% names(.)) OutcomeOfCase else defaults$OutcomeOfCase,
      HospitalisationCode = defaults$HospitalisationCode,
      HospitalisationCodeLabel = defaults$HospitalisationCodeLabel,
      HospitalisationAdmissionCodeSystem = defaults$HospitalisationAdmissionCodeSystem,
      HospitalisationCodeSystemVersion = defaults$HospitalisationCodeSystemVersion,
      HospitalisationAdmissionCodeSystemSpec = defaults$HospitalisationAdmissionCodeSystemSpec
    )
  
  # Apply additional country-specific defaults from config
  if (!is.null(config) && !is.null(config$defaults$patient)) {
    for (field in names(config$defaults$patient)) {
      if (!(field %in% names(defaults)) && !(field %in% names(patient))) {
        patient[[field]] <- config$defaults$patient[[field]]
      } else if (!(field %in% names(defaults)) && field %in% names(patient)) {
        # Field exists but not in defaults, set if currently NA
        patient[[field]] <- ifelse(is.na(patient[[field]]), 
                                   config$defaults$patient[[field]], 
                                   patient[[field]])
      }
    }
  }
  
  return(patient)
}

#' Create standard isolate table structure (Enhanced)
#'
#' @param data Source data frame
#' @param record_id_col Name of isolate record ID column
#' @param parent_id_col Name of parent (patient) record ID column
#' @param country_defaults List of country-specific default values
#' @param config Country configuration object (optional)
#'
#' @return Data frame with standard isolate table structure
create_standard_isolate_table <- function(data, record_id_col = "record_id_isolate",
                                         parent_id_col = "PatientId",
                                         country_defaults = list(), config = NULL) {
  
  # Set default values
  defaults <- list(
    LaboratoryCode = NA_character_,
    Specimen = NA_character_,
    MicroorganismCodeSystem = "SNOMED-CT",
    MicroorganismCodeSystemSpec = NA_character_,
    MicroorganismCodeSystemVersion = NA_character_
  )
  
  # Override with config defaults if provided
  if (!is.null(config) && !is.null(config$defaults$isolate)) {
    defaults <- modifyList(defaults, config$defaults$isolate)
  }
  
  # Override with explicit country-specific defaults
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

#' Apply a chain of lookups to a column
#'
#' @param data Data frame containing the column to recode
#' @param column_name Name of column to recode
#' @param lookup_chain List of lookup vectors to apply in sequence
#' @param track_unmapped Whether to track and report unmapped values
#' @param fallback_value Value or function to use for unmapped items
#'
#' @return Data frame with recoded column
#' @export
apply_lookup_chain <- function(data, column_name, lookup_chain, 
                               track_unmapped = TRUE, fallback_value = NULL) {
  if (!column_name %in% names(data)) {
    warning("Column '", column_name, "' not found in data", call. = FALSE)
    return(data)
  }
  
  unmapped_values <- c()
  
  for (i in seq_along(lookup_chain)) {
    lookup <- lookup_chain[[i]]
    if (length(lookup) == 0) next
    
    # Track values before recoding
    if (track_unmapped) {
      current_values <- unique(data[[column_name]])
      current_values <- current_values[!is.na(current_values)]
    }
    
    # Apply lookup
    if (is.null(fallback_value)) {
      data[[column_name]] <- dplyr::recode(data[[column_name]], !!!lookup, 
                                          .default = data[[column_name]])
    } else if (is.function(fallback_value)) {
      data[[column_name]] <- dplyr::recode(data[[column_name]], !!!lookup, 
                                          .default = fallback_value(data[[column_name]]))
    } else {
      data[[column_name]] <- dplyr::recode(data[[column_name]], !!!lookup, 
                                          .default = fallback_value)
    }
    
    # Track unmapped values
    if (track_unmapped) {
      unmapped <- setdiff(current_values, names(lookup))
      if (length(unmapped) > 0) {
        unmapped_values <- c(unmapped_values, unmapped)
      }
    }
  }
  
  # Report unmapped values
  if (track_unmapped && length(unmapped_values) > 0) {
    unmapped_values <- unique(unmapped_values)
    if (length(unmapped_values) <= 10) {
      warning("Unmapped values in '", column_name, "': ", 
              paste(unmapped_values, collapse = ", "), call. = FALSE)
    } else {
      warning("Unmapped values in '", column_name, "': ", 
              length(unmapped_values), " unique values (showing first 10): ",
              paste(head(unmapped_values, 10), collapse = ", "), call. = FALSE)
    }
  }
  
  return(data)
}

#' Create flexible record IDs based on templates
#'
#' @param data Data frame
#' @param id_templates Named list of ID templates (bsi, patient, isolate)
#' @param config Country configuration object
#'
#' @return Data frame with record ID columns added
#' @export
create_flexible_record_ids <- function(data, id_templates, config) {
  result <- data
  
  # Helper function to format dates
  format_date_for_id <- function(date_col, format_type = "date") {
    if (format_type == "datetime") {
      paste0(
        format(date_col, "%d%m%Y"), "_",
        format(date_col, "%H_%M")
      )
    } else if (format_type == "year") {
      format(date_col, "%Y")
    } else {
      format(date_col, "%d%m%Y")
    }
  }
  
  # Create BSI-level record ID
  if ("bsi" %in% names(id_templates)) {
    template <- id_templates$bsi
    if (!is.null(template) && length(template) > 0 && 
        grepl("\\{year\\}", template) && "DateOfSpecCollection" %in% names(data)) {
      result$sample_date_year <- format_date_for_id(result$DateOfSpecCollection, "year")
      # Vectorized replacement
      result$record_id_bsi <- mapply(
        function(year, hosp) {
          temp <- gsub("\\{year\\}", year, template)
          gsub("\\{HospitalId\\}", hosp, temp)
        },
        result$sample_date_year,
        result$HospitalId,
        USE.NAMES = FALSE
      )
    }
  }
  
  # Create patient-level record ID
  if ("patient" %in% names(id_templates)) {
    template <- id_templates$patient
    if (!is.null(template) && length(template) > 0 &&
        "DateOfHospitalAdmission" %in% names(data) && "PatientId" %in% names(data)) {
      # Safe check for has_time with default to FALSE
      has_time <- isTRUE(config$has_time)
      if (has_time) {
        result$admit_date_formatted <- format_date_for_id(result$DateOfHospitalAdmission, "datetime")
      } else {
        result$admit_date_formatted <- format_date_for_id(result$DateOfHospitalAdmission, "date")
      }
      # Vectorized replacement
      result$record_id_patient <- mapply(
        function(admit_dt, pat_id) {
          temp <- gsub("\\{admit_date\\}", admit_dt, template)
          temp <- gsub("\\{admit_datetime\\}", admit_dt, temp)
          gsub("\\{PatientId\\}", pat_id, temp)
        },
        result$admit_date_formatted,
        result$PatientId,
        USE.NAMES = FALSE
      )
      result <- result %>% dplyr::select(-admit_date_formatted)
    }
  }
  
  # Create isolate-level record ID
  if ("isolate" %in% names(id_templates)) {
    template <- id_templates$isolate
    
    # Check if template exists and is valid
    if (!is.null(template) && length(template) > 0) {
      # Check if template uses predefined isolate ID
      if (grepl("\\{IsolateId\\}", template) && "IsolateId" %in% names(data) && "MicroorganismCode" %in% names(data)) {
        result$record_id_isolate <- mapply(
          function(iso_id, org_code) {
            temp <- gsub("\\{IsolateId\\}", iso_id, template)
            gsub("\\{MicroorganismCode\\}", org_code, temp)
          },
          result$IsolateId,
          result$MicroorganismCode,
          USE.NAMES = FALSE
        )
      } else if ("DateOfSpecCollection" %in% names(data) && "PatientId" %in% names(data)) {
        # Use date-based ID
        result$specimen_date_formatted <- format_date_for_id(result$DateOfSpecCollection, "date")
        result$record_id_isolate <- mapply(
          function(spec_dt, pat_id) {
            temp <- gsub("\\{specimen_date\\}", spec_dt, template)
            gsub("\\{PatientId\\}", pat_id, temp)
          },
          result$specimen_date_formatted,
          result$PatientId,
          USE.NAMES = FALSE
        )
        result <- result %>% dplyr::select(-specimen_date_formatted)
      }
    }
  }
  
  return(result)
}

#' Unified basic cleaning function
#'
#' @param raw_data Raw data frame from country
#' @param config Country configuration object
#' @param country_code Two-letter country code
#'
#' @return Cleaned data frame with standard structure
#' @export
process_basic_cleaning <- function(raw_data, config, country_code) {
  # Validate required columns based on config
  base_required <- c("HospitalId", "PatientId", "DateOfHospitalAdmission")
  if (country_code == "EE") {
    base_required <- c(base_required, "DateOfSpecCollection", "IsolateId")
  }
  validate_required_columns(raw_data, base_required, paste0(country_code, " BSI data"))
  
  # Handle special field replacements
  recoded_data <- raw_data
  for (field in names(config$special_fields)) {
    source_field <- config$special_fields[[field]]
    if (source_field %in% names(recoded_data) && !is.na(recoded_data[[source_field]][1])) {
      recoded_data[[field]] <- recoded_data[[source_field]]
    }
  }
  
  # Parse dates
  recoded_data <- parse_dates_with_fallback(
    recoded_data, 
    config$date_columns, 
    config$date_format,
    preserve_time = config$has_time
  )
  
  # Apply field transformations
  if (!is.null(config$field_transforms)) {
    for (field_name in names(config$field_transforms)) {
      transform_func <- config$field_transforms[[field_name]]
      tryCatch({
        result_val <- transform_func(recoded_data)
        if (length(result_val) == nrow(recoded_data) || is.data.frame(result_val)) {
          if (is.data.frame(result_val)) {
            recoded_data <- result_val
          } else {
            recoded_data[[field_name]] <- result_val
          }
        }
      }, error = function(e) {
        warning("Field transformation for '", field_name, "' failed: ", e$message, call. = FALSE)
      })
    }
  }
  
  # Apply lookup mappings
  lookups <- get_country_lookups(country_code)
  for (lookup_name in names(config$lookup_mappings)) {
    mapping_config <- config$lookup_mappings[[lookup_name]]
    
    if (mapping_config$column %in% names(recoded_data) && lookup_name %in% names(lookups)) {
      lookup_table <- lookups[[lookup_name]]
      lookup_vec <- create_lookup_vector(lookup_table, mapping_config$to, mapping_config$from)
      
      # Apply lookup with fallback handling
      if (!is.null(mapping_config$fallback)) {
        # Safe check for fallback_prefix
        if ("fallback_prefix" %in% names(mapping_config) && !is.null(mapping_config$fallback_prefix)) {
          # Prefix unmapped values
          recoded_data <- recoded_data %>%
            dplyr::mutate(
              !!mapping_config$output_column := dplyr::case_when(
                .data[[mapping_config$column]] %in% names(lookup_vec) ~ 
                  lookup_vec[.data[[mapping_config$column]]],
                !is.na(.data[[mapping_config$column]]) ~ 
                  paste0(mapping_config$fallback_prefix, .data[[mapping_config$column]]),
                TRUE ~ NA_character_
              )
            )
        } else {
          # Use fallback value
          recoded_data[[mapping_config$output_column]] <- dplyr::recode(
            recoded_data[[mapping_config$column]], 
            !!!lookup_vec, 
            .default = mapping_config$fallback
          )
        }
      } else {
        recoded_data <- recode_with_lookup(
          recoded_data, 
          mapping_config$column, 
          lookup_vec
        )
        if (mapping_config$column != mapping_config$output_column) {
          recoded_data[[mapping_config$output_column]] <- recoded_data[[mapping_config$column]]
        }
      }
    }
  }
  
  # Create record IDs
  recoded_data <- create_flexible_record_ids(recoded_data, config$record_ids, config)
  
  # Clean up non-CDM columns
  noncdm_cols_to_remove <- intersect(config$noncdm_cleanup, names(recoded_data))
  if (length(noncdm_cols_to_remove) > 0) {
    recoded_data <- recoded_data %>% dplyr::select(-dplyr::all_of(noncdm_cols_to_remove))
  }
  
  return(recoded_data)
}

#' Create standard resistance table (unified for both formats)
#'
#' @param recoded_data Cleaned data frame
#' @param config Country configuration object
#' @param country_code Two-letter country code
#' @param metadata_path Path to metadata file (for HAI short codes)
#'
#' @return Standardized resistance table
#' @export
create_standard_res_table <- function(recoded_data, config, country_code, metadata_path = NULL) {
  ab_config <- config$antibiotic
  
  if (ab_config$format == "wide") {
    # Malta-style: wide format with ab_ prefix
    res <- recoded_data %>%
      tidyr::pivot_longer(
        cols = dplyr::starts_with(ab_config$prefix),
        names_to = "Antibiotic",
        values_to = "SIR",
        values_drop_na = TRUE
      ) %>%
      dplyr::mutate(
        Antibiotic = Antibiotic %>%
          stringr::str_remove(paste0("^", ab_config$prefix)) %>%
          stringr::str_remove_all("\\d+")
      ) %>%
      dplyr::mutate(
        RecordId = paste0(record_id_isolate, "_", Antibiotic),
        ParentId = record_id_isolate,
        ResultPCRmec = if ("ResultPCRmec" %in% names(.)) ResultPCRmec else NA_character_,
        ResultPbp2aAggl = if ("ResultPbp2aAggl" %in% names(.)) ResultPbp2aAggl else NA_character_,
        ResultESBL = if ("ResultESBL" %in% names(.)) ResultESBL else NA_character_,
        ResultCarbapenemase = if ("ResultCarbapenemase" %in% names(.)) ResultCarbapenemase else NA_character_,
        ZoneValue = NA_real_,
        ZoneSIR = NA_character_,
        ZoneSusceptibilitySign = NA_character_,
        MICSusceptibilitySign = NA_character_,
        MICValue = NA_real_,
        MICSIR = NA_character_,
        GradSusceptibilitySign = NA_character_,
        GradValue = NA_real_,
        GradSIR = NA_character_,
        ZoneTestDiskLoad = NA_character_,
        ReferenceGuidelinesSIR = NA_character_
      ) %>%
      dplyr::filter(SIR != "")
    
  } else {
    # Estonia-style: long format with test classification
    res <- recoded_data %>%
      dplyr::filter(!is.na(.data[[ab_config$test_column]]) & .data[[ab_config$test_column]] != "") %>%
      dplyr::mutate(
        RecordId = paste0(record_id_isolate, "_", .data[[ab_config$test_column]]),
        ParentId = record_id_isolate
      )
    
    # Classify test types
    # Extract test type patterns from config for easier reference
    grad_pattern <- ab_config$test_types$grad
    mic_pattern <- ab_config$test_types$mic
    zone_pattern <- ab_config$test_types$zone
    
    res <- res %>%
      dplyr::mutate(
        test_tag = dplyr::case_when(
          stringr::str_detect(.data[[ab_config$test_column]], grad_pattern) ~ "Grad",
          stringr::str_detect(.data[[ab_config$test_column]], mic_pattern) ~ "MIC",
          stringr::str_detect(.data[[ab_config$test_column]], zone_pattern) ~ "Zone",
          TRUE ~ "Mechanism"
        ),
        Antibiotic = dplyr::case_when(
          test_tag == "Grad" ~ stringr::str_trim(stringr::str_remove(.data[[ab_config$test_column]], grad_pattern)),
          test_tag == "MIC" ~ stringr::str_trim(stringr::str_remove(.data[[ab_config$test_column]], mic_pattern)),
          test_tag == "Zone" ~ stringr::str_trim(stringr::str_remove(.data[[ab_config$test_column]], zone_pattern)),
          TRUE ~ NA_character_
        )
      )
    
    # Process sensitivity results
    sens_results <- res %>%
      dplyr::filter(!is.na(Antibiotic)) %>%
      dplyr::mutate(
        RecordIdAb = paste0(RecordId, "_", Antibiotic),
        SusceptibilitySign = stringr::str_extract(.data[[ab_config$value_column]], "^<=|^<|^>=|^>|^="),
        Value = stringr::str_remove(.data[[ab_config$value_column]], "^<=|^<|^>=|^>|^="),
        Value = as.numeric(Value),
        SIR = .data[[ab_config$result_column]]
      ) %>%
      dplyr::select(ParentId, RecordId, RecordIdAb, Antibiotic, test_tag, SIR, SusceptibilitySign, Value) %>%
      tidyr::pivot_wider(
        id_cols = c(ParentId, RecordId, RecordIdAb, Antibiotic),
        names_from = test_tag,
        values_from = c(SIR, SusceptibilitySign, Value),
        names_glue = "{test_tag}{.value}",
        values_fn = dplyr::first
      )
    
    # Process mechanism results
    mech_results <- res %>%
      dplyr::filter(is.na(Antibiotic)) %>%
      dplyr::select(RecordId, test_tag, .data[[ab_config$result_column]]) %>%
      tidyr::pivot_wider(
        id_cols = RecordId,
        names_from = test_tag,
        values_from = ab_config$result_column,
        values_fn = dplyr::first
      )
    
    # Apply resistance recoding if available
    lookups <- get_country_lookups(country_code)
    if ("ResRecode" %in% names(lookups)) {
      recode_lookup <- create_lookup_vector(lookups$ResRecode, "generic_result", "estonia_result")
      result_cols <- setdiff(names(mech_results), "RecordId")
      for (col in result_cols) {
        mech_results <- recode_with_lookup(mech_results, col, recode_lookup)
      }
    }
    
    # Combine results
    res <- sens_results %>%
      dplyr::left_join(mech_results, by = "RecordId") %>%
      dplyr::mutate(RecordId = RecordIdAb) %>%
      dplyr::select(-RecordIdAb)
    
    # Apply antibiotic name translation chain
    if (!is.null(ab_config$translation_chain)) {
      for (trans in ab_config$translation_chain) {
        lookup_name <- paste0("Ab_", trans)
        if (lookup_name %in% names(lookups)) {
          lookup_table <- lookups[[lookup_name]]
          if (trans == "EST2ENG") {
            lookup_vec <- create_lookup_vector(lookup_table, "english_name", "estonia_name")
          } else {
            lookup_vec <- create_lookup_vector(lookup_table, "generic_name", "english_name")
          }
          res <- recode_with_lookup(res, "Antibiotic", lookup_vec)
        }
      }
    }
    
    # Apply HAI short codes if metadata available
    if (!is.null(metadata_path)) {
      res <- recode_to_HAI_short(res, metadata_path, "Antibiotic", "Antibiotic")
    }
  }
  
  # Standardize final structure
  res <- finalize_table(res, get_standard_table_columns("res"))
  
  return(res)
} 