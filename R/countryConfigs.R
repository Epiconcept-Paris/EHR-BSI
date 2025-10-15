#' Country-Specific Configuration for EHR-BSI Data Processing
#'
#' This file defines configuration objects for each country, enabling a unified
#' data processing pipeline while accommodating country-specific requirements.
#'
#' @keywords internal

#' Get country configuration
#'
#' @param country_code Two-letter country code (e.g., "MT", "EE")
#' @return List containing country-specific configuration
#' @export
get_country_config <- function(country_code) {
  if (!country_code %in% names(COUNTRY_CONFIGS)) {
    stop("Unknown country code: ", country_code, 
         ". Supported countries: ", paste(names(COUNTRY_CONFIGS), collapse = ", "))
  }
  COUNTRY_CONFIGS[[country_code]]
}

#' Get lookup tables for a country
#'
#' @param country_code Two-letter country code
#' @return List of lookup vectors for the specified country
#' @export
get_country_lookups <- function(country_code) {
  config <- get_country_config(country_code)
  lookups <- list()
  
  # Map country codes to full names for lookup tables
  country_name_map <- list(
    MT = "Malta",
    EE = "Estonia"
  )
  
  country_name <- country_name_map[[country_code]]
  if (is.null(country_name)) {
    country_name <- country_code
  }
  
  for (lookup_name in config$lookups) {
    lookup_table_name <- paste0(country_name, "_", lookup_name, "_Lookup")
    
    # Try to get the lookup table from the package data
    if (exists(lookup_table_name, envir = .GlobalEnv)) {
      lookups[[lookup_name]] <- get(lookup_table_name, envir = .GlobalEnv)
    } else if (exists(lookup_table_name)) {
      lookups[[lookup_name]] <- get(lookup_table_name)
    } else {
      warning("Lookup table not found: ", lookup_table_name, call. = FALSE)
    }
  }
  
  return(lookups)
}

#' Country Configuration Objects
#'
#' @format List of configuration objects, one per country
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

