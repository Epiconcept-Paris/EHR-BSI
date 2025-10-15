#' Country-Specific Configuration for EHR-BSI Data Processing
#'
#' This file defines configuration objects for each country, enabling a unified
#' data processing pipeline while accommodating country-specific requirements.
#'
#' @keywords internal

#' Build complete country configuration from Excel and R sources
#'
#' @param country_code Two-letter country code (e.g., "MT", "EE")
#' @param dictionary_path Optional path to Excel file
#'
#' @return Complete configuration list
#' @export
build_country_config <- function(country_code, dictionary_path = NULL) {
  # Load Excel config
  excel_config <- load_country_config_from_excel(country_code, dictionary_path)
  
  # Get R-only config (field_transforms mainly) - fallback to old system if new not found
  r_config <- if (exists("COUNTRY_R_TRANSFORMS") && country_code %in% names(COUNTRY_R_TRANSFORMS)) {
    COUNTRY_R_TRANSFORMS[[country_code]]
  } else if (country_code %in% names(COUNTRY_CONFIGS)) {
    # Backward compatibility: use old COUNTRY_CONFIGS
    return(COUNTRY_CONFIGS[[country_code]])
  } else {
    list()
  }
  
  # Parse Excel config into structured format
  structured_config <- parse_excel_config(excel_config, country_code)
  
  # Merge: R-only configs take precedence for field_transforms
  config <- modifyList(structured_config, r_config)
  
  return(config)
}

#' Get country configuration
#'
#' @param country_code Two-letter country code (e.g., "MT", "EE")
#' @param dictionary_path Optional path to Excel file
#'
#' @return List containing country-specific configuration
#' @export
get_country_config <- function(country_code, dictionary_path = NULL) {
  # Check if we have either old or new config
  has_old_config <- exists("COUNTRY_CONFIGS") && country_code %in% names(COUNTRY_CONFIGS)
  has_new_config <- exists("COUNTRY_R_TRANSFORMS") && country_code %in% names(COUNTRY_R_TRANSFORMS)
  
  # Check if Excel file exists
  if (is.null(dictionary_path)) {
    dictionary_path <- file.path("reference", "dictionaries", paste0(country_code, ".xlsx"))
  }
  has_excel <- file.exists(dictionary_path)
  
  if (!has_old_config && !has_new_config && !has_excel) {
    available <- character(0)
    if (exists("COUNTRY_CONFIGS")) {
      available <- c(available, names(COUNTRY_CONFIGS))
    }
    if (exists("COUNTRY_R_TRANSFORMS")) {
      available <- c(available, names(COUNTRY_R_TRANSFORMS))
    }
    available <- unique(available)
    
    stop("Unknown country code: ", country_code, 
         if (length(available) > 0) {
           paste0(". Supported countries: ", paste(available, collapse = ", "))
         } else {
           ". No countries configured."
         },
         call. = FALSE)
  }
  
  return(build_country_config(country_code, dictionary_path))
}

#' Get lookup tables for a country
#'
#' @param country_code Two-letter country code
#' @param dictionary_path Optional path to country Excel file (defaults to reference/dictionaries/{country_code}.xlsx)
#' @return List of lookup vectors for the specified country
#' @export
get_country_lookups <- function(country_code, dictionary_path = NULL) {
  config <- get_country_config(country_code)
  
  # Load all lookups from Excel
  all_lookups <- load_country_lookups_from_excel(country_code, dictionary_path)
  
  # Filter to only the lookups specified in the config
  lookups <- list()
  for (lookup_name in config$lookups) {
    if (lookup_name %in% names(all_lookups)) {
      lookups[[lookup_name]] <- all_lookups[[lookup_name]]
    } else {
      warning("Lookup table not found in Excel: ", lookup_name, call. = FALSE)
    }
  }
  
  return(lookups)
}

#' Null coalescing operator
#' @keywords internal
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Parse flat Excel config into structured config list
#'
#' @param excel_config Flat list from Excel
#' @param country_code Two-letter country code
#'
#' @return Structured config matching COUNTRY_CONFIGS format
#' @keywords internal
parse_excel_config <- function(excel_config, country_code) {
  
  config <- list(
    date_format = excel_config$date_format %||% "%d/%m/%Y",
    has_time = excel_config$has_time %||% FALSE,
    date_columns = excel_config$date_columns %||% c("DateOfSpecCollection", "DateOfHospitalAdmission"),
    
    record_ids = list(
      bsi = excel_config$record_id_bsi %||% "{HospitalId}-{year}",
      patient = excel_config$record_id_patient %||% "{PatientId}-{admit_date}",
      isolate = excel_config$record_id_isolate %||% "{IsolateId}_{MicroorganismCode}"
    ),
    
    antibiotic = build_antibiotic_config(excel_config),
    
    terminology = list(
      clinical = excel_config$terminology_clinical %||% "ICD-10",
      clinical_spec = excel_config$terminology_clinical_spec %||% NA_character_,
      microbiological = excel_config$terminology_microbiological %||% "SNOMED-CT",
      microbiological_spec = excel_config$terminology_microbiological_spec %||% NA_character_,
      hospitalisation = excel_config$terminology_hospitalisation %||% "ICD-10"
    ),
    
    lookups = excel_config$lookups %||% auto_detect_lookups(country_code),
    
    lookup_mappings = build_lookup_mappings_from_excel(excel_config),
    
    field_transforms = list(),  # Populated from R code
    
    defaults = list(
      patient = build_defaults_from_excel(excel_config, "patient"),
      isolate = build_defaults_from_excel(excel_config, "isolate"),
      ehrbsi = build_defaults_from_excel(excel_config, "ehrbsi")
    ),
    
    noncdm_cleanup = excel_config$noncdm_cleanup %||% c(),
    
    special_fields = build_special_fields_from_excel(excel_config)
  )
  
  return(config)
}

#' Build antibiotic configuration from Excel config
#' @keywords internal
build_antibiotic_config <- function(excel_config) {
  format <- excel_config$antibiotic_format %||% "wide"
  
  config <- list(format = format)
  
  if (format == "wide") {
    config$prefix <- excel_config$antibiotic_prefix %||% "ab_"
    config$test_types <- NULL
  } else if (format == "long") {
    config$test_column <- excel_config$antibiotic_test_column
    config$result_column <- excel_config$antibiotic_result_column
    config$value_column <- excel_config$antibiotic_value_column
    config$unit_column <- excel_config$antibiotic_unit_column
    
    # Parse test types if provided
    config$test_types <- parse_test_types(excel_config)
    
    # Parse translation chain
    if (!is.null(excel_config$antibiotic_translation_chain)) {
      config$translation_chain <- excel_config$antibiotic_translation_chain
    }
  }
  
  return(config)
}

#' Parse test types from Excel config
#' @keywords internal
parse_test_types <- function(excel_config) {
  test_types <- list()
  
  # Look for test type definitions
  if (!is.null(excel_config$test_type_grad)) {
    test_types$grad <- excel_config$test_type_grad
  }
  if (!is.null(excel_config$test_type_mic)) {
    test_types$mic <- excel_config$test_type_mic
  }
  if (!is.null(excel_config$test_type_zone)) {
    test_types$zone <- excel_config$test_type_zone
  }
  if (!is.null(excel_config$test_type_mechanism)) {
    test_types$mechanism <- excel_config$test_type_mechanism
  }
  
  if (length(test_types) == 0) return(NULL)
  return(test_types)
}

#' Auto-detect lookups from Lookups tab
#' @keywords internal
auto_detect_lookups <- function(country_code) {
  tryCatch({
    dictionary_path <- file.path("reference", "dictionaries", paste0(country_code, ".xlsx"))
    if (!file.exists(dictionary_path)) return(c())
    
    lookups_long <- readxl::read_xlsx(dictionary_path, sheet = "Lookups")
    unique_names <- unique(lookups_long$lookup_name)
    # Remove country prefix
    clean_names <- sub(paste0("^", country_code, "_"), "", unique_names)
    return(clean_names)
  }, error = function(e) {
    return(c())
  })
}

#' Build lookup mappings from Excel config
#' @keywords internal
build_lookup_mappings_from_excel <- function(excel_config) {
  mappings <- list()
  
  # Pattern: lookup_mapping_{name}_{property}
  mapping_keys <- grep("^lookup_mapping_", names(excel_config), value = TRUE)
  
  if (length(mapping_keys) == 0) return(mappings)
  
  # Extract unique mapping names
  mapping_names <- unique(sub("^lookup_mapping_([^_]+)_.*", "\\1", mapping_keys))
  
  for (name in mapping_names) {
    mapping <- list()
    prefix <- paste0("lookup_mapping_", name, "_")
    
    if (!is.null(excel_config[[paste0(prefix, "column")]])) {
      mapping$column <- excel_config[[paste0(prefix, "column")]]
    }
    if (!is.null(excel_config[[paste0(prefix, "from")]])) {
      mapping$from <- excel_config[[paste0(prefix, "from")]]
    }
    if (!is.null(excel_config[[paste0(prefix, "to")]])) {
      mapping$to <- excel_config[[paste0(prefix, "to")]]
    }
    if (!is.null(excel_config[[paste0(prefix, "output_column")]])) {
      mapping$output_column <- excel_config[[paste0(prefix, "output_column")]]
    }
    if (!is.null(excel_config[[paste0(prefix, "fallback")]])) {
      mapping$fallback <- excel_config[[paste0(prefix, "fallback")]]
    }
    if (!is.null(excel_config[[paste0(prefix, "fallback_prefix")]])) {
      mapping$fallback_prefix <- excel_config[[paste0(prefix, "fallback_prefix")]]
    }
    
    if (length(mapping) > 0) {
      mappings[[name]] <- mapping
    }
  }
  
  return(mappings)
}

#' Build table defaults from Excel config
#' @keywords internal
build_defaults_from_excel <- function(excel_config, table_name) {
  defaults <- list()
  
  # Pattern: default_{table}_{field}
  prefix <- paste0("default_", table_name, "_")
  default_keys <- grep(paste0("^", prefix), names(excel_config), value = TRUE)
  
  for (key in default_keys) {
    field_name <- sub(prefix, "", key)
    defaults[[field_name]] <- excel_config[[key]]
  }
  
  return(defaults)
}

#' Build special fields from Excel config
#' @keywords internal
build_special_fields_from_excel <- function(excel_config) {
  special <- list()
  
  # Pattern: special_field_{target}
  special_keys <- grep("^special_field_", names(excel_config), value = TRUE)
  
  for (key in special_keys) {
    field_name <- sub("^special_field_", "", key)
    special[[field_name]] <- excel_config[[key]]
  }
  
  return(special)
}

#' Country Configuration Objects (Legacy)
#'
#' @format List of configuration objects, one per country
#' @note This is maintained for backward compatibility. New countries should use
#'       Excel-based configuration with COUNTRY_R_TRANSFORMS for R-only logic.
COUNTRY_CONFIGS <- list(
  MT = list(
    # Date format specifications
    date_format = "%d/%m/%Y",
    has_time = FALSE,
    date_columns = c("DateOfSpecCollection", "DateOfHospitalAdmission", 
                     "DateOfHospitalDischarge", "EpisodeStartDate_noncdm"),
    
    # Record ID templates
    record_ids = list(
      bsi = "{HospitalId}-{year}",
      patient = "{PatientId}-{admit_date}",
      isolate = "{PatientId}-{specimen_date}"
    ),
    
    # Antibiotic data format
    antibiotic = list(
      format = "wide",
      prefix = "ab_",
      test_types = NULL  # Malta has single SIR column
    ),
    
    # Terminology systems
    terminology = list(
      clinical = "SNOMED-CT",
      clinical_spec = NA_character_,
      microbiological = "SNOMED-CT",
      microbiological_spec = NA_character_,
      hospitalisation = "SNOMED-CT"
    ),
    
    # Lookup tables to load
    lookups = c("UnitSpecialty", "Outcome", "HospType", "PathogenCode"),
    
    # Lookup mappings configuration
    lookup_mappings = list(
      UnitSpecialty = list(
        column = "UnitSpecialtyShort_noncdm",
        from = "malta_code",
        to = "generic_code",
        output_column = "UnitSpecialtyShort"
      ),
      Outcome = list(
        column = "OutcomeOfCase_noncdm",
        from = "malta_code",
        to = "generic_code",
        output_column = "OutcomeOfCase",
        fallback = "A"  # Default to "ALIVE"
      ),
      HospType = list(
        column = "HospitalId",
        from = "malta_hosptype",
        to = "hosptype_code",
        output_column = "HospitalType",
        fallback = "NOT CODED"
      ),
      PathogenCode = list(
        column = "MicroorganismCodeLabel",
        from = "malta_pathogen_name",
        to = "microorganism_code",
        output_column = "MicroorganismCode",
        fallback_prefix = "UNMAPPED: "
      )
    ),
    
    # Field transformations
    field_transforms = list(
      patientType = function(data) {
        if ("patientType_noncdm" %in% names(data)) {
          dplyr::case_when(
            data$patientType_noncdm == "TRUE" ~ "INPAT",
            data$patientType_noncdm == "FALSE" & 
              (grepl("OP", data$sourceLocation_noncdm) | 
               grepl("outpatients", tolower(data$sourceLocation_noncdm))) ~ "OUTPAT",
            data$patientType_noncdm == "FALSE" ~ "OTH",
            TRUE ~ NA_character_
          )
        } else {
          "INPAT"
        }
      },
      PreviousAdmission = function(data) {
        if ("PreviousAdmission_noncdm" %in% names(data)) {
          dplyr::case_when(
            data$PreviousAdmission_noncdm == TRUE ~ "OTH",
            data$PreviousAdmission_noncdm == FALSE ~ "NO",
            TRUE ~ NA_character_
          )
        } else {
          NA_character_
        }
      },
      HospitalSize = function(data) {
        if ("HospitalId" %in% names(data)) {
          dplyr::case_when(
            data$HospitalId == "GO" ~ 320,
            data$HospitalId == "MDH" ~ 1200,
            data$HospitalId == "OC" ~ 70,
            TRUE ~ NA_real_
          )
        } else {
          NA_real_
        }
      },
      GeoLocation = function(data) {
        if ("HospitalId" %in% names(data)) {
          dplyr::case_when(
            data$HospitalId == "GO" ~ "Gozo",
            data$HospitalId %in% c("MDH", "OC") ~ "Malta",
            TRUE ~ NA_character_
          )
        } else {
          NA_character_
        }
      }
    ),
    
    # Table-specific defaults
    defaults = list(
      patient = list(
        HospitalisationAdmissionCodeSystem = "SNOMED-CT",
        DateOfAdmissionCurrentWard = NA_character_,
        LaboratoryCode = "MT001"
      ),
      isolate = list(
        LaboratoryCode = "MT001",
        Specimen = "BLOOD"
      ),
      ehrbsi = list(
        ESurvBSI = "Automated (except denominators)",
        ProportionPopulationCovered = 0.95,
        LaboratoryCode = "MT001"
      )
    ),
    
    # Columns to remove after processing
    noncdm_cleanup = c("UnitSpecialtyShort_noncdm", "sourceLocation_noncdm", 
                       "OutcomeOfCase_noncdm", "HospitalType_noncdm", 
                       "patientType_noncdm", "PreviousAdmission_noncdm", 
                       "EpisodeStartDate_noncdm"),
    
    # Special field handling
    special_fields = list(
      DateOfSpecCollection = "EpisodeStartDate_noncdm"  # Use this if available
    )
  ),
  
  EE = list(
    # Date format specifications
    date_format = "%d/%m/%Y %H:%M",
    has_time = TRUE,
    date_columns = c("DateOfSpecCollection", "DateOfHospitalAdmission", 
                     "DateOfHospitalDischarge"),
    
    # Record ID templates
    record_ids = list(
      bsi = "{HospitalId}-{year}",
      patient = "{PatientId}-{admit_datetime}",
      isolate = "{IsolateId}_{MicroorganismCode}"
    ),
    
    # Antibiotic data format
    antibiotic = list(
      format = "long",
      test_column = "sensitivityTest_noncdm",
      result_column = "sensitivityResult_noncdm",
      value_column = "sensitivityValue_noncdm",
      unit_column = "sensitivityUnit_noncdm",
      test_types = list(
        mechanism = c("Karbapeneemide resistentsus", "Laia spektriga beetalaktamaasid",
                      "Metitsilliin-resistentsus", "Staphylococcus aureus DNA"),
        other = c("Mikroobide hulk külvis", "Mikroobi resistentsus- või virulentsusmehhanism"),
        grad = " Grad$",
        mic = " MIK$",
        zone = " Disk$"
      ),
      translation_chain = c("EST2ENG", "ENG2HAI")  # Apply in this order
    ),
    
    # Terminology systems
    terminology = list(
      clinical = "ICD-10",
      clinical_spec = NA_character_,
      microbiological = "SNOMED-CT",
      microbiological_spec = NA_character_,
      hospitalisation = "ICD-10"
    ),
    
    # Lookup tables to load
    lookups = c("MecRes", "ResRecode", "Ab_EST2ENG", "Ab_ENG2HAI", 
                "HospType", "HospGeog"),
    
    # Lookup mappings configuration
    lookup_mappings = list(
      HospType = list(
        column = "HospitalId",
        from = "estonia_hosptype",
        to = "hosptype_code",
        output_column = "HospitalType"
      ),
      HospGeog = list(
        column = "HospitalId",
        from = "estonia_hosptype",
        to = "nuts3_code",
        output_column = "GeoLocation"
      )
    ),
    
    # Field transformations
    field_transforms = list(
      PreviousAdmission = function(data) {
        # Estonia-specific gap analysis
        data %>%
          dplyr::arrange(PatientId, DateOfHospitalAdmission) %>%
          dplyr::group_by(PatientId) %>%
          dplyr::mutate(
            gap_days = as.numeric(
              difftime(DateOfHospitalAdmission, dplyr::lag(DateOfHospitalAdmission), 
                      units = "days")
            ),
            prev_HospitalId = dplyr::lag(HospitalId),
            PreviousAdmission = dplyr::case_when(
              (gap_days > 0 & gap_days <= 3) & (HospitalId == prev_HospitalId) ~ "CURR",
              (gap_days > 0 & gap_days <= 3) & (HospitalId != prev_HospitalId) ~ "OHOSP",
              TRUE ~ NA_character_
            )
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(-gap_days, -prev_HospitalId)
      },
      UnitId = function(data) {
        if (all(c("HospitalId", "UnitSpecialtyShort") %in% names(data))) {
          paste0(data$HospitalId, "_", data$UnitSpecialtyShort)
        } else {
          NA_character_
        }
      }
    ),
    
    # Table-specific defaults
    defaults = list(
      patient = list(),
      isolate = list(),
      ehrbsi = list(
        ESurvBSI = 2,
        HospitalSize = NA_real_,
        ProportionPopulationCovered = 1
      )
    ),
    
    # Columns to remove after processing
    noncdm_cleanup = c(),
    
    # Special field handling
    special_fields = list()
  )
)

#' Country R-Only Transformations
#'
#' @format List of R-only transformations that cannot be defined in Excel
#' @note This contains field_transforms functions that require R code execution.
#'       All other configuration should be in the Excel Config tab.
COUNTRY_R_TRANSFORMS <- list(
  MT = list(
    field_transforms = list(
      patientType = function(data) {
        if ("patientType_noncdm" %in% names(data)) {
          dplyr::case_when(
            data$patientType_noncdm == "TRUE" ~ "INPAT",
            data$patientType_noncdm == "FALSE" & 
              (grepl("OP", data$sourceLocation_noncdm) | 
               grepl("outpatients", tolower(data$sourceLocation_noncdm))) ~ "OUTPAT",
            data$patientType_noncdm == "FALSE" ~ "OTH",
            TRUE ~ NA_character_
          )
        } else {
          "INPAT"
        }
      },
      PreviousAdmission = function(data) {
        if ("PreviousAdmission_noncdm" %in% names(data)) {
          dplyr::case_when(
            data$PreviousAdmission_noncdm == TRUE ~ "OTH",
            data$PreviousAdmission_noncdm == FALSE ~ "NO",
            TRUE ~ NA_character_
          )
        } else {
          NA_character_
        }
      },
      HospitalSize = function(data) {
        if ("HospitalId" %in% names(data)) {
          dplyr::case_when(
            data$HospitalId == "GO" ~ 320,
            data$HospitalId == "MDH" ~ 1200,
            data$HospitalId == "OC" ~ 70,
            TRUE ~ NA_real_
          )
        } else {
          NA_real_
        }
      },
      GeoLocation = function(data) {
        if ("HospitalId" %in% names(data)) {
          dplyr::case_when(
            data$HospitalId == "GO" ~ "Gozo",
            data$HospitalId %in% c("MDH", "OC") ~ "Malta",
            TRUE ~ NA_character_
          )
        } else {
          NA_character_
        }
      }
    )
  ),
  
  EE = list(
    field_transforms = list(
      PreviousAdmission = function(data) {
        # Estonia-specific gap analysis
        data %>%
          dplyr::arrange(PatientId, DateOfHospitalAdmission) %>%
          dplyr::group_by(PatientId) %>%
          dplyr::mutate(
            gap_days = as.numeric(
              difftime(DateOfHospitalAdmission, dplyr::lag(DateOfHospitalAdmission), 
                      units = "days")
            ),
            prev_HospitalId = dplyr::lag(HospitalId),
            PreviousAdmission = dplyr::case_when(
              (gap_days > 0 & gap_days <= 3) & (HospitalId == prev_HospitalId) ~ "CURR",
              (gap_days > 0 & gap_days <= 3) & (HospitalId != prev_HospitalId) ~ "OHOSP",
              TRUE ~ NA_character_
            )
          ) %>%
          dplyr::ungroup() %>%
          dplyr::select(-gap_days, -prev_HospitalId)
      },
      UnitId = function(data) {
        if (all(c("HospitalId", "UnitSpecialtyShort") %in% names(data))) {
          paste0(data$HospitalId, "_", data$UnitSpecialtyShort)
        } else {
          NA_character_
        }
      }
    )
  )
)

