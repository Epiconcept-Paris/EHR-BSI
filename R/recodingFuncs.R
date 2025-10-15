#' Common functions for country-specific BSI data recoding
#'
#' This file contains shared functions used by both Estonia and Malta
#' recoding scripts to avoid code duplication and ensure consistency.

#' Load country configuration from Excel file
#'
#' @param country_code Two-letter country code
#' @param dictionary_path Optional path to country Excel file
#'
#' @return List containing configuration from Config tab
#' @export
load_country_config_from_excel <- function(country_code, dictionary_path = NULL) {
  if (is.null(dictionary_path)) {
    dictionary_path <- file.path("reference", "dictionaries", paste0(country_code, ".xlsx"))
  }
  
  if (!file.exists(dictionary_path)) {
    stop("Dictionary file not found: ", dictionary_path, call. = FALSE)
  }
  
  # Try to read Config tab
  config_raw <- tryCatch({
    readxl::read_xlsx(dictionary_path, sheet = "Config")
  }, error = function(e) {
    return(NULL)  # Config tab optional for backward compatibility
  })
  
  if (is.null(config_raw)) {
    return(list())  # Return empty config if no Config tab
  }
  
  # Validate structure
  required_cols <- c("config_key", "config_value")
  if (!all(required_cols %in% names(config_raw))) {
    warning("Config tab missing required columns (config_key, config_value). Using empty config.", 
            call. = FALSE)
    return(list())
  }
  
  # Parse config into list
  config <- list()
  for (i in seq_len(nrow(config_raw))) {
    key <- config_raw$config_key[i]
    value <- config_raw$config_value[i]
    
    # Skip NA keys
    if (is.na(key)) next
    
    type <- if ("config_type" %in% names(config_raw) && !is.na(config_raw$config_type[i])) {
      config_raw$config_type[i]
    } else {
      "string"
    }
    
    # Type conversion
    parsed_value <- switch(type,
      "boolean" = as.logical(value),
      "numeric" = as.numeric(value),
      "list" = if (!is.na(value)) trimws(strsplit(as.character(value), ",")[[1]]) else character(0),
      as.character(value)  # string default
    )
    
    config[[key]] <- parsed_value
  }
  
  return(config)
}

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

#' Parse date columns with specified format
#'
#' @param data Data frame containing date columns
#' @param fallback_cols Character vector of date column names to parse
#' @param date_format Format string for date parsing (can include time)
#' @param preserve_time Whether to preserve time component (returns POSIXct instead of Date)
#'
#' @return Data frame with parsed date columns
parse_dates_with_fallback <- function(data, fallback_cols, date_format = "%d/%m/%Y", 
                                     preserve_time = FALSE) {
  # Parse specified date columns
  available_date_cols <- intersect(fallback_cols, names(data))
  # Normalize factor/character to character before parsing; guard against non-atomic types
  coerce_to_char <- function(x) {
    if (is.factor(x)) return(as.character(x))
    if (inherits(x, "POSIXct") || inherits(x, "Date")) return(x)
    if (!is.atomic(x)) return(as.character(x))
    x
  }
  
  if (length(available_date_cols) > 0) {
    for (col in available_date_cols) {
      tryCatch({
        if (preserve_time || grepl("%H|%M|%S", date_format)) {
          # Parse as POSIXct for datetime
          data[[col]] <- as.POSIXct(coerce_to_char(data[[col]]), format = date_format)
        } else {
          # Parse as Date for date-only
          data[[col]] <- as.Date(coerce_to_char(data[[col]]), format = date_format)
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
  
  return(setNames(lookup_table[[value_col]], lookup_table[[key_col]]))
}

#' Check if a column exists in data frame
#'
#' @param data Data frame to check
#' @param col Column name to check for
#'
#' @return Logical indicating if column exists
has_column <- function(data, col) {
  col %in% names(data)
}

#' Get column from data frame or return default value
#'
#' @param data Data frame to extract from
#' @param col Column name to extract
#' @param default Default value if column doesn't exist
#'
#' @return Column values or default value
get_column_or_default <- function(data, col, default) {
  if (has_column(data, col)) {
    return(data[[col]])
  } else {
    return(default)
  }
}

#' Format date column for use in record IDs
#'
#' @param date_col Date vector to format
#' @param format_type Type of formatting: "date", "datetime", or "year"
#'
#' @return Character vector of formatted dates
format_date_for_id <- function(date_col, format_type = "date") {
  # Ensure date_col is a Date or POSIXt object
  if (!inherits(date_col, c("Date", "POSIXct", "POSIXlt"))) {
    # Try to convert to Date if it's character or numeric
    date_col <- tryCatch({
      as.Date(date_col)
    }, error = function(e) {
      warning("Could not convert date column to Date object: ", e$message)
      return(rep(NA, length(date_col)))
    })
  }
  
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

#' Apply template substitution to create IDs
#'
#' @param template Character template with placeholders in {brackets}
#' @param data Data frame containing columns for substitution
#' @param substitution_map Named list mapping placeholder names to column names or formatted values
#'
#' @return Character vector of IDs with substitutions applied
apply_template_substitution <- function(template, data, substitution_map) {
  if (is.null(template) || length(template) == 0 || nrow(data) == 0) {
    return(character(nrow(data)))
  }
  
  # Initialize result with template repeated for each row
  result <- rep(template, nrow(data))
  
  # Apply each substitution
  for (placeholder in names(substitution_map)) {
    pattern <- paste0("\\{", placeholder, "\\}")
    replacement_col <- substitution_map[[placeholder]]
    
    # Handle both column names and pre-computed values
    if (is.character(replacement_col) && length(replacement_col) == 1 && has_column(data, replacement_col)) {
      replacement_values <- data[[replacement_col]]
    } else if (length(replacement_col) == nrow(data)) {
      replacement_values <- replacement_col
    } else {
      next  # Skip if neither a valid column nor matching length vector
    }
    
    # Vectorized replacement
    result <- mapply(
      function(template_str, value) {
        gsub(pattern, value, template_str)
      },
      result,
      replacement_values,
      USE.NAMES = FALSE
    )
  }
  
  return(result)
}

#' Find tolerant column name match
#'
#' @param raw_names Character vector of raw column names to find
#' @param data_names Character vector of available column names in data
#' @param norm_func Function to normalize names for comparison
#'
#' @return Named list with matches (name = raw_name, value = matched_data_name)
find_tolerant_column_match <- function(raw_names, data_names, norm_func) {
  matches <- list()
  
  norm_data <- setNames(vapply(data_names, norm_func, character(1)), data_names)
  norm_raw <- setNames(vapply(raw_names, norm_func, character(1)), raw_names)
  
  for (rn in names(norm_raw)) {
    candidates <- names(norm_data)[norm_data == norm_raw[[rn]]]
    if (length(candidates) == 1L) {
      matches[[rn]] <- candidates[[1]]
    } else if (length(candidates) > 1L) {
      warning("Ambiguous dictionary match for '", rn, "' -> candidates: ", 
              paste(candidates, collapse = ", "), call. = FALSE)
    }
  }
  
  return(matches)
}

#' Apply a single lookup mapping to data
#'
#' @param data Data frame to modify
#' @param mapping_config Mapping configuration with column, from, to, output_column, fallback settings
#' @param lookup_table Lookup table data frame
#'
#' @return Modified data frame with lookup applied
apply_single_lookup_mapping <- function(data, mapping_config, lookup_table) {
  # Validate inputs
  if (!has_column(data, mapping_config$column)) {
    return(data)
  }
  
  # Create lookup vector
  lookup_vec <- create_lookup_vector(lookup_table, mapping_config$to, mapping_config$from)
  
  if (length(lookup_vec) == 0) {
    return(data)
  }
  
  # Determine output column (default to input column if not specified)
  output_col <- mapping_config$output_column %||% mapping_config$column
  
  # Apply lookup based on fallback configuration
  if (!is.null(mapping_config$fallback_prefix)) {
    # Prefix unmapped values
    data <- data %>%
      dplyr::mutate(
        !!output_col := dplyr::case_when(
          .data[[mapping_config$column]] %in% names(lookup_vec) ~ 
            lookup_vec[.data[[mapping_config$column]]],
          !is.na(.data[[mapping_config$column]]) ~ 
            paste0(mapping_config$fallback_prefix, .data[[mapping_config$column]]),
          TRUE ~ NA_character_
        )
      )
  } else if (!is.null(mapping_config$fallback)) {
    # Use fallback value
    data[[output_col]] <- dplyr::recode(
      data[[mapping_config$column]], 
      !!!lookup_vec, 
      .default = mapping_config$fallback
    )
  } else {
    # No fallback - keep original or map to new column
    if (mapping_config$column != output_col) {
      # Create new column with lookup applied, don't modify source
      data[[output_col]] <- dplyr::recode(
        data[[mapping_config$column]], 
        !!!lookup_vec,
        .default = data[[mapping_config$column]]
      )
    } else {
      # Only modify in-place if output is same as input
      data <- recode_with_lookup(data, mapping_config$column, lookup_vec)
    }
  }
  
  return(data)
}

#' Initialize standard resistance table columns
#'
#' @param res_data Base resistance data frame
#' @param mechanism_cols Optional vector of mechanism result column names
#'
#' @return Data frame with all standard resistance columns initialized
initialize_resistance_columns <- function(res_data, mechanism_cols = c()) {
  # Define standard columns that should exist
  standard_cols <- list(
    ResultPCRmec = NA_character_,
    ResultPbp2aAggl = NA_character_,
    ResultESBL = NA_character_,
    ResultCarbapenemase = NA_character_,
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
  )
  
  # Add standard columns if they don't exist
  for (col in names(standard_cols)) {
    if (!has_column(res_data, col)) {
      res_data[[col]] <- standard_cols[[col]]
    }
  }
  
  # Ensure mechanism columns exist if specified
  for (col in mechanism_cols) {
    if (!has_column(res_data, col)) {
      res_data[[col]] <- NA_character_
    }
  }
  
  return(res_data)
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
      DateUsedForStatistics = format(as.Date(DateOfSpecCollection), "%Y"),
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
        admit_date_formatted = format(as.Date(.data[[admission_date_col]]), "%d%m%Y"),
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
        specimen_date_formatted = format(as.Date(.data[[specimen_date_col]]), "%d%m%Y"),
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
      RecordId = get_column_or_default(., record_id_col, NA_character_),
      ParentId = get_column_or_default(., parent_id_col, NA_character_),
      PatientSpecialty = get_column_or_default(., "PatientSpecialty", defaults$PatientSpecialty),
      patientType = get_column_or_default(., "patientType", defaults$patientType),
      OutcomeOfCase = get_column_or_default(., "OutcomeOfCase", defaults$OutcomeOfCase),
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
      RecordId = get_column_or_default(., record_id_col, NA_character_),
      ParentId = get_column_or_default(., parent_id_col, NA_character_),
      LaboratoryCode = defaults$LaboratoryCode,
      Specimen = get_column_or_default(., "Specimen", defaults$Specimen),
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

#' Load country lookups from Excel file
#'
#' @param country_code Two-letter country code (e.g., "MT", "EE")
#' @param dictionary_path Path to the country-specific Excel file (optional)
#'
#' @return List of lookup data.tables keyed by lookup name
#' @export
load_country_lookups_from_excel <- function(country_code, dictionary_path = NULL) {
  # Set default path if not provided
  if (is.null(dictionary_path)) {
    dictionary_path <- file.path("reference", "dictionaries", paste0(country_code, ".xlsx"))
  }
  
  # Check if file exists
  if (!file.exists(dictionary_path)) {
    stop("Dictionary file not found: ", dictionary_path, call. = FALSE)
  }
  
  # Read the Lookups tab
  tryCatch({
    lookups_long <- readxl::read_xlsx(dictionary_path, sheet = "Lookups")
  }, error = function(e) {
    stop("Failed to read Lookups tab from ", dictionary_path, ": ", e$message, call. = FALSE)
  })
  
  # Validate structure
  required_cols <- c("lookup_name", "from_value", "to_value")
  missing_cols <- setdiff(required_cols, names(lookups_long))
  if (length(missing_cols) > 0) {
    stop("Lookups tab missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  
  # Check if new data-driven columns exist
  has_column_metadata <- all(c("from_column", "to_column") %in% names(lookups_long))
  
  if (!has_column_metadata) {
    # Issue deprecation warning
    warning("Lookups tab missing 'from_column' and 'to_column' columns. ",
            "Using legacy hardcoded column names. ",
            "Run update_excel_lookups.R to update your Excel files to the new format.",
            call. = FALSE)
  }
  
  # Convert to list of data.tables grouped by lookup_name
  lookup_list <- list()
  unique_lookup_names <- unique(lookups_long$lookup_name)
  
  for (lookup_name in unique_lookup_names) {
    # Filter rows for this lookup
    lookup_subset <- lookups_long[lookups_long$lookup_name == lookup_name, ]
    
    # Determine column names: use data-driven if available, otherwise use legacy patterns
    if (has_column_metadata) {
      # New data-driven approach: read column names from Excel
      from_col <- unique(lookup_subset$from_column)[1]
      to_col <- unique(lookup_subset$to_column)[1]
      
      if (is.na(from_col) || is.na(to_col)) {
        warning("Missing column metadata for lookup '", lookup_name, "'. Using generic 'from/to' columns.",
                call. = FALSE)
        from_col <- "from"
        to_col <- "to"
      }}
    
    # Create data.table with determined column names
    lookup_dt <- data.table::data.table(
      from_value = lookup_subset$from_value,
      to_value = lookup_subset$to_value
    )
    names(lookup_dt) <- c(from_col, to_col)
    
    # Remove country prefix from lookup name for consistency with existing code
    clean_name <- sub("^(Malta|Estonia)_", "", lookup_name)
    if (clean_name == "Name_Lookup" || clean_name == "Specialty_Lookup") {
      clean_name <- lookup_name  # Keep full name for shared lookups
    }
    
    lookup_list[[clean_name]] <- lookup_dt
  }
  
  return(lookup_list)
}

#' Apply dictionary column renaming from Excel
#'
#' @param data Data frame with raw column names
#' @param dictionary_path Path to the country-specific Excel file
#'
#' @return Data frame with standardized column names
#' @export
apply_dictionary_from_excel <- function(data, dictionary_path) {
  # Check if file exists
  if (!file.exists(dictionary_path)) {
    warning("Dictionary file not found: ", dictionary_path, ". Proceeding without renaming.", call. = FALSE)
    return(data)
  }
  
  # Read the Dictionary tab
  tryCatch({
    dictionary <- readxl::read_xlsx(dictionary_path, sheet = "Dictionary")
  }, error = function(e) {
    warning("Failed to read Dictionary tab from ", dictionary_path, ": ", e$message, 
            ". Proceeding without renaming.", call. = FALSE)
    return(data)
  })
  
  # Validate structure
  required_cols <- c("raw_column_name", "standard_column_name")
  missing_cols <- setdiff(required_cols, names(dictionary))
  if (length(missing_cols) > 0) {
    warning("Dictionary tab missing required columns: ", paste(missing_cols, collapse = ", "),
            ". Proceeding without renaming.", call. = FALSE)
    return(data)
  }
  
  # Create a named vector for renaming (exact match)
  rename_map <- setNames(dictionary$standard_column_name, dictionary$raw_column_name)
  
  # Phase 1: exact matches only
  cols_to_rename <- intersect(names(data), names(rename_map))
  renamed_count <- 0L
  if (length(cols_to_rename) > 0) {
    for (old_name in cols_to_rename) {
      new_name <- rename_map[[old_name]]
      names(data)[names(data) == old_name] <- new_name
    }
    renamed_count <- length(cols_to_rename)
  }
  
  # Phase 2: tolerant matches (case/space/punctuation-insensitive)
  # This helps when Excel headers have subtle spacing differences like "L3_RSLT_CODE_NAME (SIR)"
  norm <- function(x) tolower(gsub("[^a-z0-9]", "", trimws(as.character(x))))
  raw_names <- names(rename_map)
  still_unmatched <- setdiff(raw_names, cols_to_rename)
  if (length(still_unmatched) > 0) {
    tolerant_matches <- find_tolerant_column_match(still_unmatched, names(data), norm)
    if (length(tolerant_matches) > 0) {
      for (rn in names(tolerant_matches)) {
        old <- tolerant_matches[[rn]]
        new <- rename_map[[rn]]
        names(data)[names(data) == old] <- new
      }
      renamed_count <- renamed_count + length(tolerant_matches)
    }
  }
  
  if (renamed_count > 0) {
    message("Renamed ", renamed_count, " columns using dictionary")
  } else {
    message("No columns matched dictionary entries. Data may already be standardized.")
  }
  
  return(data)
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
  
  # Create BSI-level record ID
  if ("bsi" %in% names(id_templates) && has_column(data, "DateOfSpecCollection") && has_column(data, "HospitalId")) {
    template <- id_templates$bsi
    if (!is.null(template) && length(template) > 0) {
      substitutions <- list(
        year = format_date_for_id(result$DateOfSpecCollection, "year"),
        HospitalId = "HospitalId"
      )
      result$record_id_bsi <- apply_template_substitution(template, result, substitutions)
    }
  }
  
  # Create patient-level record ID
  if ("patient" %in% names(id_templates) && has_column(data, "DateOfHospitalAdmission") && has_column(data, "PatientId")) {
    template <- id_templates$patient
    if (!is.null(template) && length(template) > 0) {
      # Safe check for has_time with default to FALSE
      has_time <- isTRUE(config$has_time)
      format_type <- if (has_time) "datetime" else "date"
      admit_formatted <- format_date_for_id(result$DateOfHospitalAdmission, format_type)
      
      substitutions <- list(
        admit_date = admit_formatted,
        admit_datetime = admit_formatted,
        PatientId = "PatientId"
      )
      result$record_id_patient <- apply_template_substitution(template, result, substitutions)
    }
  }
  
  # Create isolate-level record ID
  if ("isolate" %in% names(id_templates)) {
    template <- id_templates$isolate
    if (!is.null(template) && length(template) > 0) {
      # Check if template uses predefined isolate ID
      if (grepl("\\{IsolateId\\}", template) && has_column(data, "IsolateId") && has_column(data, "MicroorganismCode")) {
        substitutions <- list(
          IsolateId = "IsolateId",
          MicroorganismCode = "MicroorganismCode"
        )
        result$record_id_isolate <- apply_template_substitution(template, result, substitutions)
      } else if (has_column(data, "DateOfSpecCollection") && has_column(data, "PatientId")) {
        # Use date-based ID
        substitutions <- list(
          specimen_date = format_date_for_id(result$DateOfSpecCollection, "date"),
          PatientId = "PatientId"
        )
        result$record_id_isolate <- apply_template_substitution(template, result, substitutions)
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
    
    if (lookup_name %in% names(lookups)) {
      lookup_table <- lookups[[lookup_name]]
      recoded_data <- apply_single_lookup_mapping(recoded_data, mapping_config, lookup_table)
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
          stringr::str_remove_all("\\d+"),
        RecordId = paste0(record_id_isolate, "_", Antibiotic),
        ParentId = record_id_isolate
      ) %>%
      dplyr::filter(SIR != "")
    
    # Initialize standard resistance columns
    mechanism_cols <- c("ResultPCRmec", "ResultPbp2aAggl", "ResultESBL", "ResultCarbapenemase")
    res <- initialize_resistance_columns(res, mechanism_cols)
    
  } else {
    # Estonia-style: long format with test classification
    # Defensive: check required columns exist
    required_long <- c(ab_config$test_column, ab_config$result_column, ab_config$value_column)
    miss_long <- setdiff(required_long, names(recoded_data))
    if (length(miss_long) > 0) {
      # Try tolerant matches to suggest possible fixes
      norm <- function(x) tolower(gsub("[^a-z0-9]", "", x))
      nn <- setNames(vapply(names(recoded_data), norm, character(1)), names(recoded_data))
      suggestions <- lapply(miss_long, function(m) {
        mm <- norm(m)
        close <- names(nn)[nn == mm]
        if (length(close) == 0) close <- names(recoded_data)[agrepl(m, names(recoded_data), ignore.case = TRUE)]
        paste0("'", m, "' -> ", ifelse(length(close) > 0, paste(close, collapse = ", "), "no close match"))
      })
      stop("Missing long-format antibiotic column(s): ", paste(miss_long, collapse = ", "),
           ". Suggestions: ", paste(unlist(suggestions), collapse = "; "))
    }
    res <- recoded_data %>%
      dplyr::filter(!is.na(.data[[ab_config$test_column]]) & .data[[ab_config$test_column]] != "") %>%
      dplyr::mutate(
        RecordId = paste0(record_id_isolate, "_", .data[[ab_config$test_column]]),
        ParentId = record_id_isolate
      )
    
    # Classify test types (provide defensible defaults if missing)
    grad_pattern <- ab_config$test_types$grad %||% " Grad$"
    mic_pattern <- ab_config$test_types$mic %||% " MIK$"
    zone_pattern <- ab_config$test_types$zone %||% " Disk$"
    
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
    
    # Get mechanism column names from mech_results (exclude RecordId)
    mechanism_cols <- setdiff(names(mech_results), "RecordId")
    
    # Initialize standard resistance columns
    res <- initialize_resistance_columns(res, mechanism_cols)
    
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

#' Process country BSI data using config-driven approach
#'
#' This generic processor works for any country with a valid configuration.
#' It replaces the need for country-specific recoding files.
#'
#' @param raw_data Raw data frame
#' @param country_code Two-letter country code
#' @param episode_duration Episode duration in days
#' @param metadata_path Path to metadata file
#'
#' @return List with ehrbsi, patient, isolate, res tables
#' @export
process_country_generic <- function(raw_data, country_code, episode_duration, metadata_path = NULL) {
  # Get config (from Excel + R transforms)
  config <- get_country_config(country_code)
  
  # Step 1: Basic cleaning
  recoded_data <- process_basic_cleaning(raw_data, config, country_code)
  
  # Step 2: Create tables
  patient <- create_standard_patient_table(recoded_data, config = config)
  patient <- finalize_table(patient, get_standard_table_columns("patient"))
  
  isolate <- create_standard_isolate_table(recoded_data, config = config)
  isolate <- finalize_table(isolate, get_standard_table_columns("isolate"))
  
  res <- create_standard_res_table(recoded_data, config, country_code, metadata_path)
  
  ehrbsi <- create_base_ehrbsi_table(recoded_data, country_code, episode_duration, 
                                     record_id_col = "record_id_bsi", config = config)
  ehrbsi <- finalize_table(ehrbsi, get_standard_table_columns("ehrbsi"))
  
  return(list(
    ehrbsi = ehrbsi,
    patient = patient,
    isolate = isolate,
    res = res
  ))
} 