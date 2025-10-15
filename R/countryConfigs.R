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
  
  # Parse Excel config (schema v1 or v2) into structured format
  schema_version <- tryCatch({
    if (!is.null(excel_config$schema_version)) suppressWarnings(as.integer(excel_config$schema_version)) else NA_integer_
  }, error = function(e) NA_integer_)
  is_v2_like <- any(grepl("^lookup_mappings\\.", names(excel_config))) ||
    any(grepl("^record_ids\\.", names(excel_config))) ||
    any(grepl("^default\\.", names(excel_config))) ||
    any(grepl("^date\\.", names(excel_config))) ||
    (!is.na(schema_version) && schema_version >= 2)
  structured_config <- if (isTRUE(is_v2_like)) {
    parse_excel_config_v2(excel_config, country_code)
  } else {
    parse_excel_config(excel_config, country_code)
  }
  
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
  
  # Load lookups: country-specific from Excel and shared from package/resources
  all_lookups <- load_country_lookups_from_excel(country_code, dictionary_path)
  shared <- load_shared_lookups()
  merged <- all_lookups
  if (length(shared) > 0) {
    for (nm in names(shared)) {
      if (is.null(merged[[nm]])) merged[[nm]] <- shared[[nm]]
    }
  }
  
  # Filter to only the lookups specified in the config
  lookups <- list()
  for (lookup_name in config$lookups) {
    if (lookup_name %in% names(merged)) {
      lookups[[lookup_name]] <- merged[[lookup_name]]
    } else {
      warning("Lookup table not found: ", lookup_name, call. = FALSE)
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

#' Parse flat Excel config into structured config list (schema v2)
#'
#' @param excel_config Flat list from Excel (dot-notation keys supported)
#' @param country_code Two-letter country code
#'
#' @return Structured config matching COUNTRY_CONFIGS format
#' @keywords internal
parse_excel_config_v2 <- function(excel_config, country_code) {
  # Helper: fetch a value supporting multiple candidate keys
  getv <- function(...) {
    keys <- unlist(list(...))
    for (k in keys) {
      if (!is.null(excel_config[[k]])) return(excel_config[[k]])
    }
    NULL
  }
  # Dates
  date_format_val <- getv("date.format", "date_format")
  if (isTRUE(tolower(as.character(date_format_val)) == "auto")) {
    date_format_val <- "%d/%m/%Y"
  }
  has_time_val <- getv("has_time")
  # coerce has_time to logical when provided as string
  if (!is.null(has_time_val)) {
    if (is.character(has_time_val)) {
      hv <- tolower(trimws(has_time_val))
      if (hv %in% c("true", "t", "1", "yes", "y")) has_time_val <- TRUE
      else if (hv %in% c("false", "f", "0", "no", "n")) has_time_val <- FALSE
    }
  }
  if (isTRUE(tolower(as.character(has_time_val)) == "auto")) {
    has_time_val <- FALSE
  }
  # If date format includes time tokens, default has_time TRUE
  if (is.character(date_format_val) && grepl("%H|%M|%S", date_format_val)) {
    has_time_val <- TRUE
  }
  # Record IDs
  record_bsi <- getv("record_ids.bsi", "record_id_bsi") %||% "{HospitalId}-{year}"
  record_patient <- getv("record_ids.patient", "record_id_patient") %||% "{PatientId}-{admit_date}"
  record_isolate <- getv("record_ids.isolate", "record_id_isolate") %||% "{IsolateId}_{MicroorganismCode}"
  # Antibiotic profiles → derive v1-compatible fields for existing builder
  ab_profile <- getv("antibiotic.profile")
  if (!is.null(ab_profile)) ab_profile <- tolower(as.character(ab_profile))
  v1_ab <- list()
  if (identical(ab_profile, "malta_wide")) {
    v1_ab$antibiotic_format <- "wide"
    v1_ab$antibiotic_prefix <- getv("antibiotic.prefix") %||% "ab_"
  } else if (identical(ab_profile, "estonia_long")) {
    v1_ab$antibiotic_format <- "long"
    v1_ab$antibiotic_test_column <- getv("antibiotic.test_column") %||% "sensitivityTest_noncdm"
    v1_ab$antibiotic_result_column <- getv("antibiotic.result_column") %||% "sensitivityResult_noncdm"
    v1_ab$antibiotic_value_column <- getv("antibiotic.value_column") %||% "sensitivityValue_noncdm"
    v1_ab$antibiotic_unit_column <- getv("antibiotic.unit_column") %||% "sensitivityUnit_noncdm"
    # provide default test type patterns if not explicitly set in Excel
    v1_ab$test_type_grad <- getv("test_type.grad", "test_type_grad") %||% " Grad$"
    v1_ab$test_type_mic <- getv("test_type.mic", "test_type_mic") %||% " MIK$"
    v1_ab$test_type_zone <- getv("test_type.zone", "test_type_zone") %||% " Disk$"
  } else if (!is.null(ab_profile)) {
    # custom → defer to explicit fields if present; fallback to wide
    v1_ab$antibiotic_format <- getv("antibiotic.format") %||% "wide"
    if (identical(v1_ab$antibiotic_format, "wide")) {
      v1_ab$antibiotic_prefix <- getv("antibiotic.prefix") %||% "ab_"
    } else {
      v1_ab$antibiotic_test_column <- getv("antibiotic.test_column")
      v1_ab$antibiotic_result_column <- getv("antibiotic.result_column")
      v1_ab$antibiotic_value_column <- getv("antibiotic.value_column")
      v1_ab$antibiotic_unit_column <- getv("antibiotic.unit_column")
    }
  }
  trans_chain <- getv("antibiotic.translation_chain")
  if (!is.null(trans_chain)) {
    if (is.character(trans_chain) && length(trans_chain) == 1L) {
      trans_chain <- trimws(unlist(strsplit(trans_chain, ",")))
    }
    v1_ab$antibiotic_translation_chain <- trans_chain
  }
  excel_v1_bridge <- utils::modifyList(excel_config, v1_ab)
  # Lookups include
  lookups_include <- getv("lookups.include")
  lookups_vec <- if (is.null(lookups_include) || tolower(as.character(lookups_include)) == "auto") {
    auto_detect_lookups(country_code)
  } else if (is.character(lookups_include)) {
    trimws(unlist(strsplit(lookups_include, ",")))
  } else {
    as.character(lookups_include)
  }
  # Lookup mappings in dot notation
  dot_keys <- grep("^lookup_mappings\\.", names(excel_config), value = TRUE)
  mappings <- list()
  if (length(dot_keys) > 0) {
    for (k in dot_keys) {
      m <- sub("^lookup_mappings\\.([^\\.]+)\\..*$", "\\1", k)
      p <- sub("^lookup_mappings\\.[^\\.]+\\.(.*)$", "\\1", k)
      if (nzchar(m) && nzchar(p)) {
        if (is.null(mappings[[m]])) mappings[[m]] <- list()
        val <- excel_config[[k]]
        if (identical(p, "output")) p <- "output_column"
        mappings[[m]][[p]] <- val
      }
    }
  }
  # Defaults (default.<table>.<field>)
  defaults <- list(patient = list(), isolate = list(), ehrbsi = list())
  def_keys <- grep("^default\\.", names(excel_config), value = TRUE)
  if (length(def_keys) > 0) {
    for (k in def_keys) {
      tbl <- sub("^default\\.([^\\.]+)\\..*$", "\\1", k)
      fld <- sub("^default\\.[^\\.]+\\.(.*)$", "\\1", k)
      if (!is.null(defaults[[tbl]])) defaults[[tbl]][[fld]] <- excel_config[[k]]
    }
  }
  # Special fields (special_field.<name>)
  special <- list()
  sp_keys <- grep("^special_field\\.", names(excel_config), value = TRUE)
  if (length(sp_keys) > 0) {
    for (k in sp_keys) {
      nm <- sub("^special_field\\.(.*)$", "\\1", k)
      special[[nm]] <- excel_config[[k]]
    }
  }
  # Cleanup
  cleanup <- getv("noncdm_cleanup")
  if (is.character(cleanup) && length(cleanup) == 1L) cleanup <- trimws(unlist(strsplit(cleanup, ",")))
  if (is.null(cleanup)) cleanup <- c()
  config <- list(
    date_format = date_format_val %||% "%d/%m/%Y",
    has_time = if (is.null(has_time_val)) FALSE else has_time_val,
    date_columns = getv("date.columns", "date_columns") %||% c("DateOfSpecCollection", "DateOfHospitalAdmission"),
    record_ids = list(
      bsi = record_bsi,
      patient = record_patient,
      isolate = record_isolate
    ),
    antibiotic = build_antibiotic_config(excel_v1_bridge),
    terminology = list(
      clinical = getv("terminology.clinical", "terminology_clinical") %||% "ICD-10",
      clinical_spec = getv("terminology.clinical_spec", "terminology_clinical_spec") %||% NA_character_,
      microbiological = getv("terminology.microbiological", "terminology_microbiological") %||% "SNOMED-CT",
      microbiological_spec = getv("terminology.microbiological_spec", "terminology_microbiological_spec") %||% NA_character_,
      hospitalisation = getv("terminology.hospitalisation", "terminology_hospitalisation") %||% "ICD-10"
    ),
    lookups = lookups_vec,
    lookup_mappings = if (length(mappings) > 0) mappings else list(),
    field_transforms = list(),
    defaults = defaults,
    noncdm_cleanup = cleanup,
    special_fields = special
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

#' Load shared lookup tables (if available)
#' @keywords internal
load_shared_lookups <- function() {
  out <- list()
  tryCatch({
    pkg_path <- system.file("reference", "dictionaries", "SharedLookups.xlsx", package = "EHRBSI", mustWork = FALSE)
    fs_path <- if (!nzchar(pkg_path)) file.path("reference", "dictionaries", "SharedLookups.xlsx") else pkg_path
    if (nzchar(fs_path) && file.exists(fs_path)) {
      sheets <- readxl::excel_sheets(fs_path)
      for (sh in sheets) {
        df <- readxl::read_xlsx(fs_path, sheet = sh)
        if (all(c("from_value", "to_value") %in% names(df))) {
          vec <- df$to_value
          names(vec) <- as.character(df$from_value)
          out[[sh]] <- vec
        } else {
          out[[sh]] <- df
        }
      }
    }
  }, error = function(e) {})
  out
}

#' Validate a country's configuration and provide friendly messages
#' @param country_code Two-letter country code
#' @param dictionary_path Optional path to Excel file
#' @param strict If TRUE, fail on warnings
#' @export
validate_country_config <- function(country_code, dictionary_path = NULL, strict = FALSE) {
  issues <- list(errors = character(0), warnings = character(0))
  cfg <- tryCatch(get_country_config(country_code, dictionary_path), error = function(e) {
    issues$errors <- c(issues$errors, e$message)
    return(NULL)
  })
  if (is.null(cfg)) return(issues)
  if (is.null(cfg$date_format) || !is.character(cfg$date_format)) {
    issues$warnings <- c(issues$warnings, "date_format missing or invalid; using %d/%m/%Y by default")
  }
  if (is.null(cfg$record_ids) || any(!c("bsi", "patient", "isolate") %in% names(cfg$record_ids))) {
    issues$errors <- c(issues$errors, "record_ids templates are incomplete (require bsi, patient, isolate)")
  }
  if (is.null(cfg$antibiotic) || is.null(cfg$antibiotic$format)) {
    issues$errors <- c(issues$errors, "antibiotic configuration missing format (wide/long)")
  } else if (identical(cfg$antibiotic$format, "long")) {
    needed <- c("test_column", "result_column", "value_column")
    miss <- needed[!needed %in% names(cfg$antibiotic)]
    if (length(miss) > 0) issues$errors <- c(issues$errors, paste0("antibiotic long format missing: ", paste(miss, collapse = ", ")))
  }
  if (!is.null(cfg$lookups) && length(cfg$lookups) > 0) {
    lk <- tryCatch(get_country_lookups(country_code, dictionary_path), error = function(e) NULL)
    if (is.null(lk) || length(lk) == 0) {
      issues$warnings <- c(issues$warnings, "No lookup tables could be loaded; check Lookups tab and shared lookups")
    }
  }
  if (strict && length(issues$errors) > 0) stop(paste(issues$errors, collapse = "\n"), call. = FALSE)
  issues
}

#' Convert a v1 Excel config to v2 and write a new workbook
#' @param country_code Two-letter country code
#' @param dictionary_path Optional source path
#' @param output_path Optional output file path (xlsx)
#' @return Output path
#' @export
convert_config_v1_to_v2 <- function(country_code, dictionary_path = NULL, output_path = NULL) {
  if (is.null(dictionary_path)) {
    dictionary_path <- file.path("reference", "dictionaries", paste0(country_code, ".xlsx"))
  }
  if (is.null(output_path)) {
    output_path <- file.path("reference", "dictionaries", paste0(country_code, "_v2.xlsx"))
  }
  excel_cfg <- load_country_config_from_excel(country_code, dictionary_path)
  cfg <- parse_excel_config(excel_cfg, country_code)
  kv <- list()
  kv$schema_version <- 2
  kv$`date.format` <- cfg$date_format
  kv$has_time <- as.logical(cfg$has_time)
  kv$`record_ids.bsi` <- cfg$record_ids$bsi
  kv$`record_ids.patient` <- cfg$record_ids$patient
  kv$`record_ids.isolate` <- cfg$record_ids$isolate
  if (!is.null(cfg$antibiotic$format) && cfg$antibiotic$format == "wide") {
    kv$`antibiotic.profile` <- "malta_wide"
    kv$`antibiotic.prefix` <- cfg$antibiotic$prefix %||% "ab_"
  } else if (!is.null(cfg$antibiotic$format) && cfg$antibiotic$format == "long") {
    kv$`antibiotic.profile` <- "estonia_long"
    kv$`antibiotic.test_column` <- cfg$antibiotic$test_column
    kv$`antibiotic.result_column` <- cfg$antibiotic$result_column
    kv$`antibiotic.value_column` <- cfg$antibiotic$value_column
    kv$`antibiotic.unit_column` <- cfg$antibiotic$unit_column
    if (!is.null(cfg$antibiotic$translation_chain)) kv$`antibiotic.translation_chain` <- paste(cfg$antibiotic$translation_chain, collapse = ",")
  }
  if (!is.null(cfg$lookups) && length(cfg$lookups) > 0) kv$`lookups.include` <- paste(cfg$lookups, collapse = ",")
  if (!is.null(cfg$lookup_mappings)) {
    for (nm in names(cfg$lookup_mappings)) {
      mp <- cfg$lookup_mappings[[nm]]
      for (p in names(mp)) {
        key <- paste0("lookup_mappings.", nm, ".", ifelse(p == "output_column", "output", p))
        kv[[key]] <- mp[[p]]
      }
    }
  }
  for (tbl in names(cfg$defaults)) {
    for (fld in names(cfg$defaults[[tbl]])) {
      kv[[paste0("default.", tbl, ".", fld)]] <- cfg$defaults[[tbl]][[fld]]
    }
  }
  if (!is.null(cfg$special_fields)) {
    for (fld in names(cfg$special_fields)) {
      kv[[paste0("special_field.", fld)]] <- cfg$special_fields[[fld]]
    }
  }
  if (!is.null(cfg$noncdm_cleanup) && length(cfg$noncdm_cleanup) > 0) kv$noncdm_cleanup <- paste(cfg$noncdm_cleanup, collapse = ",")
  wb <- openxlsx::createWorkbook()
  if (file.exists(dictionary_path)) {
    sheets <- tryCatch(readxl::excel_sheets(dictionary_path), error = function(e) character(0))
    copy_sheet <- function(name) {
      if (name %in% sheets) {
        df <- tryCatch(readxl::read_xlsx(dictionary_path, sheet = name), error = function(e) NULL)
        if (!is.null(df)) {
          openxlsx::addWorksheet(wb, name)
          openxlsx::writeData(wb, name, df)
        }
      }
    }
    copy_sheet("Dictionary")
    copy_sheet("Lookups")
  }
  openxlsx::addWorksheet(wb, "Config")
  config_df <- data.frame(config_key = names(kv), config_value = unname(unlist(kv)), stringsAsFactors = FALSE)
  openxlsx::writeData(wb, "Config", config_df)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
  return(output_path)
}

#' Migrate EE/MT v1 configs to v2 and write side-by-side files
#' @export
migrate_all_v1_to_v2 <- function() {
  out <- list()
  for (cc in c("EE", "MT")) {
    out[[cc]] <- tryCatch(convert_config_v1_to_v2(cc), error = function(e) e$message)
  }
  out
}


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

