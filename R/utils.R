# =============================================================================
# EHR-BSI Shared Utilities
# Centralized constants, date helpers, and visualization helpers
# =============================================================================

# =============================================================================
# NAMED CONSTANTS
# =============================================================================

#' @keywords internal
EPISODE_DURATION_DEFAULT <- 14L

#' @keywords internal
EPISODE_DURATION_MAX <- 365L

#' @keywords internal
HO_HA_THRESHOLD_DAYS <- 2L

#' @keywords internal
IMP_HA_THRESHOLD_DAYS <- 2L

#' @keywords internal
CC_CLUSTER_WINDOW_DAYS <- 2L

#' @keywords internal
CONTAMINANT_WINDOW_DAYS <- 2L

#' @keywords internal
EXCEL_DATE_ORIGIN <- "1899-12-30"

#' @keywords internal
PATIENT_DAYS_MULTIPLIER <- 5L

#' @keywords internal
RATE_PER_N <- 1000L

#' @keywords internal
TOP_N_PATHOGENS <- 20L

#' @keywords internal
MAX_AGE <- 120L

#' @keywords internal
AGE_BREAKS <- c(-Inf, 20, 40, 60, 80, Inf)

#' @keywords internal
AGE_LABELS <- c("< 20 years", "21 - 40 years", "41 - 60 years", "61 - 80 years", "81 + years")

#' @keywords internal
STANDARD_TABLE_NAMES <- c("ehrbsi", "patient", "isolate", "res")

#' @keywords internal
VALID_UNIT_SPECIALTIES <- c("GO", "ICU", "LTC", "MED", "OTH", "PED", "PSY", "RHB", "SUR")

#' @keywords internal
PREVIOUS_ADMISSION_GAP_DAYS <- 2L

# =============================================================================
# DATE UTILITIES
# =============================================================================

#' Robust date coercion
#'
#' Supports Date/POSIX, ISO, EU/US formats, with/without time, and Excel serials.
#'
#' @param x Value(s) to coerce to Date
#' @return Date vector
#' @export
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  # Excel serials or numeric-like strings
  if (is.numeric(x)) return(as.Date(x, origin = EXCEL_DATE_ORIGIN))
  if (is.character(x)) {
    xs <- trimws(x)
    num_idx <- suppressWarnings(!is.na(as.numeric(xs)))
    out <- rep(as.Date(NA), length(xs))
    if (any(num_idx)) {
      out[num_idx] <- as.Date(as.numeric(xs[num_idx]), origin = EXCEL_DATE_ORIGIN)
    }
    # Try parsing remaining with a broad set of formats
    try_formats <- c(
      "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
      "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
      "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
      "%d.%m.%Y %H:%M:%S", "%d.%m.%Y %H:%M", "%d.%m.%Y",
      "%d-%m-%Y %H:%M:%S", "%d-%m-%Y %H:%M", "%d-%m-%Y",
      "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M", "%Y/%m/%d"
    )
    need_parse <- which(is.na(out))
    if (length(need_parse) > 0) {
      parsed <- suppressWarnings(try(as.POSIXlt(xs[need_parse], tz = "", tryFormats = try_formats), silent = TRUE))
      if (!inherits(parsed, "try-error")) {
        out[need_parse] <- as.Date(parsed)
      }
    }
    return(out)
  }
  # Fallback: try generic parsing with safeguards
  parsed <- suppressWarnings(try(as.POSIXlt(x, tz = "", tryFormats = c("%Y-%m-%d", "%d/%m/%Y")), silent = TRUE))
  if (inherits(parsed, "try-error")) return(as.Date(NA))
  as.Date(parsed)
}

#' Normalize a date value to a year string
#'
#' Handles Date objects, numeric years, and character year strings.
#'
#' @param date_value A Date, numeric year, or character representation
#' @return Character year string (e.g. "2024")
#' @export
normalize_date_to_year <- function(date_value) {
  tryCatch({
    if (inherits(date_value, "Date")) {
      format(date_value, "%Y")
    } else if (is.numeric(date_value)) {
      as.character(date_value)
    } else if (!is.null(date_value) && grepl("^\\d{4}$", as.character(date_value))) {
      as.character(date_value)
    } else {
      format(as.Date(date_value), "%Y")
    }
  }, error = function(e) as.character(date_value))
}

#' Create time period from dates based on aggregation level
#'
#' @param dates Date vector
#' @param aggregation Character: "week", "month", "quarter", or "year"
#' @return Date vector with period start dates
#' @export
create_time_period <- function(dates, aggregation = "month") {
  switch(aggregation,
    "week" = as.Date(cut(dates, breaks = "week")),
    "month" = as.Date(paste0(format(dates, "%Y-%m"), "-01")),
    "quarter" = as.Date(paste0(
      format(dates, "%Y"), "-",
      sprintf("%02d", (as.numeric(format(dates, "%m")) - 1) %/% 3 * 3 + 1), "-01"
    )),
    "year" = as.Date(paste0(format(dates, "%Y"), "-01-01")),
    as.Date(paste0(format(dates, "%Y-%m"), "-01"))  # default to month
  )
}

#' Parse a date-like value to a Date, handling year-only values
#'
#' @param value Date, numeric year, or character date/year
#' @return Date vector
#' @export
parse_date_to_year_start <- function(value) {
  tryCatch({
    if (is.numeric(value)) {
      as.Date(paste0(value, "-01-01"))
    } else {
      as.Date(value)
    }
  }, error = function(e) {
    as.Date(paste0(as.character(value), "-01-01"))
  })
}

#' Apply a year filter to a data frame
#'
#' @param data Data frame to filter
#' @param year Character year to filter by (or "all" to skip)
#' @param year_col Name of the year column (default "episodeYear")
#' @param date_col Fallback date column to extract year from (default NULL)
#' @return Filtered data frame
#' @export
apply_year_filter <- function(data, year, year_col = "episodeYear", date_col = NULL) {
  if (is.null(year) || year == "all" || is.null(data) || nrow(data) == 0) {
    return(data)
  }
  if (year_col %in% names(data)) {
    data[as.character(data[[year_col]]) == year, , drop = FALSE]
  } else if (!is.null(date_col) && date_col %in% names(data)) {
    data$..tmp_year.. <- as.integer(format(as.Date(data[[date_col]]), "%Y"))
    result <- data[as.character(data$..tmp_year..) == year, , drop = FALSE]
    result$..tmp_year.. <- NULL
    result
  } else {
    data
  }
}

# =============================================================================
# VISUALIZATION UTILITIES
# =============================================================================

#' Create an empty placeholder plot with a message
#'
#' @param message Text to display
#' @param size Font size (default 6)
#' @return ggplot2 object
#' @export
create_empty_plot <- function(message = "No data available", size = 6) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message, size = size) +
    ggplot2::theme_void()
}

#' Abbreviate pathogen names for display
#'
#' Shortens genus names to standard abbreviations (e.g. "Staphylococcus" -> "S.").
#'
#' @param names Character vector of pathogen names
#' @return Character vector of abbreviated names
#' @export
abbreviate_pathogen_name <- function(names) {
  result <- gsub("([A-Z])[a-z]+ ([a-z]+)", "\\1. \\2", names)
  result <- gsub("Staphylococcus", "S.", result)
  result <- gsub("Enterococcus", "E.", result)
  result <- gsub("Escherichia", "E.", result)
  result <- gsub("Klebsiella", "K.", result)
  result <- gsub("Candida", "Cand.", result)
  result <- gsub("Pseudomonas", "P.", result)
  result <- gsub("Enterobacter", "Enterob.", result)
  result <- gsub("Proteus", "P.", result)
  result <- gsub("Streptococcus", "Strep.", result)
  result <- gsub("Cutibacterium", "C.", result)
  result
}

#' Get standard pathogen color palette
#'
#' Returns a named vector of colors for common BSI pathogens.
#' Includes both abbreviated and full names for matching flexibility.
#'
#' @return Named character vector of hex colors
#' @export
get_pathogen_colors <- function() {
  c(
    # Full and abbreviated names for monomicrobial/polymicrobial plots
    "E. coli" = "#8B4513", "Escherichia coli" = "#8B4513",
    "S. aureus" = "#FFD700", "Staphylococcus aureus" = "#FFD700",
    "S. epidermidis" = "#4F7942", "Staphylococcus epidermidis" = "#4F7942",
    "K. pneumoniae" = "#CD5C5C", "Klebsiella pneumoniae" = "#CD5C5C",
    "E. faecalis" = "#9ACD32", "Enterococcus faecalis" = "#9ACD32",
    "E. faecium" = "#008B8B", "Enterococcus faecium" = "#008B8B",
    "P. aeruginosa" = "#87CEEB", "Pseudomonas aeruginosa" = "#87CEEB",
    "P. mirabilis" = "#483D8B", "Proteus mirabilis" = "#483D8B",
    "S. hominis" = "#FF8C00", "Staphylococcus hominis" = "#FF8C00",
    "Enterob. cloacae" = "#000080", "Enterobacter cloacae" = "#000080",
    "S. pneumoniae" = "#DC143C", "Streptococcus pneumoniae" = "#DC143C",
    "Strep. pneumoniae" = "#DC143C",
    "S. haemolyticus" = "#8B008B", "Staphylococcus haemolyticus" = "#8B008B",
    "Candida albicans" = "#FF1493", "Cand. albicans" = "#FF1493",
    "C. albicans" = "#FF1493",
    # Additional abbreviated-only entries for specialty distribution
    "C. acnes" = "#000000",
    "S. spp." = "#4682B4",
    "K. oxytoca" = "#FF69B4",
    "Strep. pyogenes" = "#FF4500",
    "S. capitis" = "#008080",
    "S. marcescens" = "#C0C0C0",
    "T. glabrata" = "#2F4F4F"
  )
}

#' Assign colors to pathogens, generating extras for unrecognized names
#'
#' @param pathogen_names Character vector of pathogen names
#' @return Named character vector of hex colors
#' @export
assign_pathogen_colors <- function(pathogen_names) {
  palette <- get_pathogen_colors()
  colors <- palette[pathogen_names]
  missing_idx <- which(is.na(colors))
  if (length(missing_idx) > 0) {
    additional <- grDevices::rainbow(length(missing_idx), s = 0.6, v = 0.8)
    colors[missing_idx] <- additional
  }
  names(colors) <- pathogen_names
  colors
}

#' Shorten specialty names for plot labels
#'
#' @param specialties Character vector of specialty names
#' @return Character vector with shortened names (newlines for wrapping)
#' @export
shorten_specialty_names <- function(specialties) {
  result <- gsub("Interdisciplinary or unknown", "Interdisciplinary\nor unknown", specialties)
  result <- gsub("Surgery/operative disciplines", "Surgery/operative\ndisciplines", result)
  result <- gsub("Neurology and Neurosurgery", "Neurology and\nNeurosurgery", result)
  result
}

# =============================================================================
# DATA PROCESSING UTILITIES
# =============================================================================

#' Apply a transformation function to all standard EHR-BSI tables
#'
#' @param result_list List containing ehrbsi, patient, isolate, res tables
#' @param transform_fn Function to apply to each table
#' @return Modified result list
#' @export
apply_to_all_tables <- function(result_list, transform_fn) {
  for (tbl_name in STANDARD_TABLE_NAMES) {
    if (tbl_name %in% names(result_list) && !is.null(result_list[[tbl_name]]) &&
        nrow(result_list[[tbl_name]]) > 0) {
      result_list[[tbl_name]] <- transform_fn(result_list[[tbl_name]])
    }
  }
  result_list
}

#' Count isolates with fallback when EpisodeId is missing
#'
#' Counts by group columns using EpisodeId if available, otherwise creates
#' a temporary row-level ID for counting.
#'
#' @param data Data frame
#' @param group_cols Character vector of column names to group by
#' @return Aggregated data frame with group columns and EpisodeId count
#' @export
count_episodes_or_isolates <- function(data, group_cols) {
  formula_str <- paste("EpisodeId ~", paste(group_cols, collapse = " + "))
  if ("EpisodeId" %in% names(data)) {
    result <- stats::aggregate(stats::as.formula(formula_str), data = data, FUN = length)
  } else {
    data$EpisodeId <- seq_len(nrow(data))
    result <- stats::aggregate(stats::as.formula(formula_str), data = data, FUN = length)
  }
  result
}

#' Filter hospital data by hospital and year
#'
#' Filters ehrbsi, patient, isolate, res, and episodes tables for a specific
#' hospital and year. Used by hospital analysis tab and PDF report.
#'
#' @param current_data List with ehrbsi, patient, isolate, res tables
#' @param episodes Episodes data frame (or NULL)
#' @param selected_hospital Hospital ID to filter by
#' @param selected_date Date/year value for EHRBSI filtering
#' @param selected_year Character year for patient admission filtering
#' @return List with filtered ehrbsi, patient, isolate, res, episodes
#' @export
filter_hospital_data <- function(current_data, episodes, selected_hospital, selected_date, selected_year) {
  # Filter EHRBSI table
  ehrbsi_filtered <- NULL
  if (!is.null(current_data$ehrbsi)) {
    ehrbsi <- current_data$ehrbsi
    if ("HospitalId" %in% names(ehrbsi) && "DateUsedForStatistics" %in% names(ehrbsi) &&
        !is.null(selected_hospital) && !is.null(selected_date)) {
      ehrbsi_filtered <- ehrbsi[
        ehrbsi$HospitalId == selected_hospital &
          ehrbsi$DateUsedForStatistics == selected_date, , drop = FALSE
      ]
    }
  }

  # Filter patient table
  patient_filtered <- NULL
  if (!is.null(current_data$patient)) {
    patient <- current_data$patient
    if ("HospitalId" %in% names(patient)) {
      if ("DateOfHospitalAdmission" %in% names(patient)) {
        patient$admission_year <- format(as.Date(patient$DateOfHospitalAdmission), "%Y")
        patient_filtered <- patient[
          patient$HospitalId == selected_hospital &
            patient$admission_year == selected_year, , drop = FALSE
        ]
      } else {
        patient_filtered <- patient[patient$HospitalId == selected_hospital, , drop = FALSE]
      }
    }
  }

  # Filter isolate table based on patient records
  isolate_filtered <- NULL
  if (!is.null(current_data$isolate) && !is.null(patient_filtered)) {
    isolate <- current_data$isolate
    if ("ParentId" %in% names(isolate) && "RecordId" %in% names(patient_filtered)) {
      patient_record_ids <- patient_filtered$RecordId
      isolate_filtered <- isolate[isolate$ParentId %in% patient_record_ids, , drop = FALSE]
    }
  }

  # Filter res table based on isolate records
  res_filtered <- NULL
  if (!is.null(current_data$res) && !is.null(isolate_filtered)) {
    res <- current_data$res
    if ("ParentId" %in% names(res) && "RecordId" %in% names(isolate_filtered)) {
      isolate_record_ids <- isolate_filtered$RecordId
      res_filtered <- res[res$ParentId %in% isolate_record_ids, , drop = FALSE]
    }
  }

  # Filter episodes based on patient records
  episodes_filtered <- NULL
  if (!is.null(episodes) && !is.null(patient_filtered)) {
    if ("AdmissionRecordId" %in% names(episodes) && "RecordId" %in% names(patient_filtered)) {
      patient_record_ids <- patient_filtered$RecordId
      episodes_filtered <- episodes[episodes$AdmissionRecordId %in% patient_record_ids, , drop = FALSE]
    }
  }

  list(
    ehrbsi = ehrbsi_filtered,
    patient = patient_filtered,
    isolate = isolate_filtered,
    res = res_filtered,
    episodes = episodes_filtered
  )
}

#' Compute PreviousAdmission from admission/discharge history
#'
#' Detects whether a patient had a recent prior admission within three calendar
#' days (date of discharge = day one) before the current admission, as per
#' EHR-BSI protocol v1.0 (p.21).
#'
#' The gap is measured from the previous discharge date to the current admission
#' date. A gap of 0-2 days (i.e. three calendar days with discharge = day one)
#' qualifies as a previous admission.
#'
#' Used by EE and GEN country transforms.
#'
#' @param data Data frame with PatientId, DateOfHospitalAdmission,
#'   DateOfHospitalDischarge, and HospitalId columns
#' @param gap_days Maximum numeric day-gap (discharge-to-admission) to consider
#'   "previous" (default 2, i.e. within three calendar days where discharge = day 1)
#' @return Modified data frame with PreviousAdmission column
#' @export
compute_previous_admission <- function(data, gap_days = PREVIOUS_ADMISSION_GAP_DAYS) {
  # DateOfHospitalDischarge is required for correct gap calculation.
  # Fall back to admission-based gap if discharge dates are unavailable.
  has_discharge <- "DateOfHospitalDischarge" %in% names(data) &&
    !all(is.na(data$DateOfHospitalDischarge))

  if (!has_discharge) {
    warning("DateOfHospitalDischarge not available; ",
            "PreviousAdmission gap will be estimated from admission dates. ",
            "Results may be inaccurate.", call. = FALSE)
  }

  data %>%
    dplyr::arrange(PatientId, DateOfHospitalAdmission) %>%
    dplyr::group_by(PatientId) %>%
    dplyr::mutate(
      # Protocol: gap is from previous discharge to current admission
      prev_discharge = dplyr::lag(to_date(DateOfHospitalDischarge)),
      prev_HospitalId = dplyr::lag(HospitalId),
      discharge_to_admit_gap = if (has_discharge) {
        as.numeric(
          difftime(to_date(DateOfHospitalAdmission), prev_discharge,
                   units = "days")
        )
      } else {
        # Fallback: use admission-to-admission gap (less accurate)
        as.numeric(
          difftime(to_date(DateOfHospitalAdmission),
                   dplyr::lag(to_date(DateOfHospitalAdmission)),
                   units = "days")
        )
      },
      PreviousAdmission = dplyr::case_when(
        (discharge_to_admit_gap >= 0 & discharge_to_admit_gap <= !!gap_days) &
          (HospitalId == prev_HospitalId) ~ "CURR",
        (discharge_to_admit_gap >= 0 & discharge_to_admit_gap <= !!gap_days) &
          (HospitalId != prev_HospitalId) ~ "OHOSP",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-prev_discharge, -prev_HospitalId, -discharge_to_admit_gap)
}

#' Compute UnitId from HospitalId and UnitSpecialtyShort
#'
#' Shared logic used by EE and GEN country transforms.
#'
#' @param data Data frame with HospitalId and UnitSpecialtyShort columns
#' @return Character vector of UnitId values
#' @export
compute_unit_id <- function(data) {
  if (all(c("HospitalId", "UnitSpecialtyShort") %in% names(data))) {
    paste0(data$HospitalId, "_", data$UnitSpecialtyShort)
  } else {
    NA_character_
  }
}

#' Ensure all episode class columns exist after pivot_wider
#'
#' Adds missing episode class columns with value 0 and renames to standard BSI count names.
#'
#' @param data Data frame after pivot_wider with countEps_* columns
#' @return Data frame with standardized NumberOf* columns
#' @keywords internal
ensure_episode_class_columns <- function(data) {
  data %>%
    dplyr::mutate(
      countEps_CA = if ("countEps_CA" %in% names(.)) replace(countEps_CA, is.na(countEps_CA), 0) else 0,
      `countEps_HO-HA` = if ("countEps_HO-HA" %in% names(.)) replace(`countEps_HO-HA`, is.na(`countEps_HO-HA`), 0) else 0,
      `countEps_IMP-HA` = if ("countEps_IMP-HA" %in% names(.)) replace(`countEps_IMP-HA`, is.na(`countEps_IMP-HA`), 0) else 0,
      countEps_CC_CA = if ("countEps_CC_CA" %in% names(.)) replace(countEps_CC_CA, is.na(countEps_CC_CA), 0) else 0,
      `countEps_CC_HO-HA` = if ("countEps_CC_HO-HA" %in% names(.)) replace(`countEps_CC_HO-HA`, is.na(`countEps_CC_HO-HA`), 0) else 0,
      `countEps_CC_IMP-HA` = if ("countEps_CC_IMP-HA" %in% names(.)) replace(`countEps_CC_IMP-HA`, is.na(`countEps_CC_IMP-HA`), 0) else 0
    ) %>%
    dplyr::mutate(
      NumberOfCABSIs = countEps_CA,
      NumberOfHOHABSIs = `countEps_HO-HA`,
      NumberOfImportedHABSIs = `countEps_IMP-HA`,
      NumberOfTotalBSIs = NumberOfCABSIs + NumberOfHOHABSIs + NumberOfImportedHABSIs,
      NumberOfCABSIs_CC = countEps_CC_CA,
      NumberOfHOHABSIs_CC = `countEps_CC_HO-HA`,
      NumberOfImportedHABSIs_CC = `countEps_CC_IMP-HA`,
      NumberOfTotalBSIs_CC = NumberOfCABSIs_CC + NumberOfHOHABSIs_CC + NumberOfImportedHABSIs_CC
    ) %>%
    dplyr::select(-countEps_CA, -`countEps_HO-HA`, -`countEps_IMP-HA`,
                  -countEps_CC_CA, -`countEps_CC_HO-HA`, -`countEps_CC_IMP-HA`)
}

#' Build episode counts for a given grouping
#'
#' Shared aggregation logic used by aggregateEpisodes for all aggregation levels.
#'
#' @param eps_df Episode data frame
#' @param id_col Column name for the grouping ID (e.g. "HospitalId", "LaboratoryCode")
#' @param year_col Optional year column name (NULL for non-year aggregations)
#' @return Data frame with RecordId and episode count columns
#' @keywords internal
build_episode_counts <- function(eps_df, id_col, year_col = NULL) {
  select_cols <- c(id_col, "EpisodeClass", "EpisodeId", "AllCommensal")
  if (!is.null(year_col)) select_cols <- c(select_cols, year_col)

  result <- eps_df %>%
    dplyr::select(dplyr::all_of(select_cols)) %>%
    dplyr::distinct()

  if (!is.null(year_col)) {
    result <- result %>%
      dplyr::mutate(RecordId = paste0(.data[[id_col]], "-", .data[[year_col]]))
  } else {
    result <- result %>%
      dplyr::mutate(RecordId = .data[[id_col]])
  }

  result <- result %>%
    dplyr::mutate(AllCommensal = ifelse(is.na(AllCommensal), FALSE, AllCommensal)) %>%
    dplyr::group_by(RecordId, EpisodeClass) %>%
    dplyr::summarise(
      countEps = dplyr::n(),
      countEps_CC = sum(AllCommensal, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = EpisodeClass,
      values_from = c(countEps, countEps_CC),
      id_cols = c(RecordId),
      values_fill = list(countEps = 0, countEps_CC = 0)
    )

  ensure_episode_class_columns(result)
}
