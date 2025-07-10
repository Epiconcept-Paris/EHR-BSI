#' Process Estonia BSI data from raw format to EHR-BSI format
#'
#' @param raw_data Received from genericRecodeOrchestrator.R
#' @note The reporting year is automatically derived from the admission dates in the data
#'
#' @return Returns a list containing the four EHR-BSI data tables: ehrbsi, patient, isolate, res
#' @export
#'

# Internal helper functions (not exported)
.process_estonia_basic_cleaning <- function(raw_data, reporting_year) {
  # Validate required columns exist
  required_cols <- c("DateOfSpecCollection", "DateOfHospitalAdmission", 
                    "HospitalId", "PatientId", "IsolateId")
  validate_required_columns(raw_data, required_cols, "Estonia BSI data")
  
  # Create date-time strings for IDs
  recoded_data <- raw_data %>%
    dplyr::mutate(
      DateOfSpecCollection = as.POSIXct(DateOfSpecCollection, format = "%d/%m/%Y %H:%M"),
      sample_date_time = paste0(
        format(DateOfSpecCollection, "%d%m%Y"), "_",
        format(DateOfSpecCollection, "%H_%M")
      ),
      DateOfHospitalAdmission = as.POSIXct(DateOfHospitalAdmission, format = "%d/%m/%Y %H:%M"),
      admit_date_time = paste0(
        format(DateOfHospitalAdmission, "%d%m%Y"), "_",
        format(DateOfHospitalAdmission, "%H_%M")
      )
    )
  
  # Create dataset IDs (Estonia uses time components, so manual creation needed)
  recoded_data <- recoded_data %>%
    dplyr::mutate(
      record_id_bsi = paste0(HospitalId),
      record_id_patient = paste0(PatientId, "-", admit_date_time),
      record_id_isolate = paste0(IsolateId, "_", MicroorganismCode)
    ) %>%
    dplyr::select(-admit_date_time, -sample_date_time)
  
  # Recode dates
  fallback_date_cols <- c("DateOfSpecCollection", "DateOfHospitalAdmission", "DateOfHospitalDischarge")
  recoded_data <- parse_dates_with_fallback(recoded_data, fallback_date_cols, "%d/%m/%Y %H:%M")
  
  return(recoded_data)
}

.create_estonia_patient_table <- function(recoded_data) {
  # Create base patient table using shared function
  patient <- create_standard_patient_table(recoded_data)
  
  # Add Estonia-specific fields and logic
  patient <- patient %>%
    dplyr::mutate(
      UnitId = paste0(HospitalId, "_", UnitSpecialtyShort)
    ) %>%
    dplyr::arrange(PatientId, DateOfHospitalAdmission) %>%
    dplyr::group_by(PatientId) %>%
    dplyr::mutate(
      gap_days = as.numeric(
        difftime(DateOfHospitalAdmission, dplyr::lag(DateOfHospitalAdmission), units = "days")
      ),
      prev_HospitalId = dplyr::lag(HospitalId),
      PreviousAdmission = dplyr::case_when(
        (gap_days > 0 & gap_days <= 3) & (HospitalId == prev_HospitalId) ~ "CURR",
        (gap_days > 0 & gap_days <= 3) & (HospitalId != prev_HospitalId) ~ "OHOSP",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::ungroup()
  
  # Finalize table with standard column selection
  patient <- finalize_table(patient, get_standard_table_columns("patient"))
  
  return(patient)
}

.create_estonia_isolate_table <- function(recoded_data) {
  # Create base isolate table using shared function
  isolate <- create_standard_isolate_table(recoded_data)
  
  # Finalize table with standard column selection
  isolate <- finalize_table(isolate, get_standard_table_columns("isolate"))
  
  return(isolate)
}

.create_estonia_res_table <- function(recoded_data, metadata_path = NULL) {
  # Create lookup vectors using shared function
  estonia_mecres_lookup <- create_lookup_vector(Estonia_MecRes_Lookup, "resistance_type", "resistance_value")
  estonia_resrecode_lookup <- create_lookup_vector(Estonia_ResRecode_Lookup, "generic_result", "estonia_result")
  estonia_est2eng_lookup <- create_lookup_vector(Estonia_Ab_EST2ENG_Lookup, "english_name", "estonia_name")
  estonia_eng2hai_lookup <- create_lookup_vector(Estonia_Ab_ENG2HAI_Lookup, "generic_name", "english_name")
  
  res <- recoded_data %>%
    dplyr::filter(!is.na(sensitivityTest_noncdm) & sensitivityTest_noncdm != "") %>%
    dplyr::mutate(
      RecordId = paste0(record_id_isolate, "_", sensitivityTest_noncdm),
      ParentId = IsolateId,
      ResultPCRmec = NA_character_,
      ResultPbp2aAggl = NA_character_, 
      ResultESBL = NA_character_, 
      ResultCarbapenemase = NA_character_, 
      ZoneValue = NA_real_, 
      ZoneSIR = NA_character_, 
      ZoneSusceptibilitySign = NA_character_, 
      ZoneTestDiskLoad = NA_character_, 
      MICSusceptibilitySign = NA_character_, 
      MICValue = NA_real_, 
      MICSIR = NA_character_, 
      GradSusceptibilitySign = NA_character_, 
      GradValue = NA_real_, 
      GradSIR = NA_character_, 
      ReferenceGuidelinesSIR = NA_character_
    ) %>%
    dplyr::select(
      RecordId, ParentId, ResultPCRmec, ResultPbp2aAggl, ResultESBL, ResultCarbapenemase,
      ZoneSIR, ZoneValue, ZoneSusceptibilitySign, MICSusceptibilitySign, MICValue, MICSIR, 
      GradSusceptibilitySign, GradValue, GradSIR, ZoneTestDiskLoad, ReferenceGuidelinesSIR,
      sensitivityTest_noncdm, sensitivityResult_noncdm, sensitivityUnit_noncdm, sensitivityValue_noncdm
    ) %>%
    dplyr::distinct()
  
  # Classify test types
  res <- res %>%
    dplyr::mutate(
      test_tag = dplyr::case_when(
        # Mechanism/screening tests
        stringr::str_detect(sensitivityTest_noncdm, "^Karbapeneemide resistentsus") ~ "ResultCarbapenemase",
        sensitivityTest_noncdm == "Laia spektriga beetalaktamaasid" ~ "ResultESBL",
        stringr::str_detect(sensitivityTest_noncdm, "Metitsilliin-resistentsus") ~ "ResultPCRmec",
        sensitivityTest_noncdm == "Staphylococcus aureus DNA" ~ "ResultPbp2aAggl",
        # Other lab entries
        sensitivityTest_noncdm == "Mikroobide hulk külvis" ~ "CFUCount",
        sensitivityTest_noncdm == "Mikroobi resistentsus- või virulentsusmehhanism" ~ "ResVirMechanism",
        # Routine antibiotic sensitivity tests
        stringr::str_detect(sensitivityTest_noncdm, "\\sGrad$") ~ "Grad",
        stringr::str_detect(sensitivityTest_noncdm, "\\sMIK$") ~ "MIC",
        stringr::str_detect(sensitivityTest_noncdm, "\\sDisk$") ~ "Zone",
        TRUE ~ NA_character_
      )
    )
  
  # Extract antibiotic names
  res <- res %>%
    dplyr::mutate(
      Antibiotic = dplyr::case_when(
        test_tag == "Grad" ~ stringr::str_trim(stringr::str_remove(sensitivityTest_noncdm, "\\sGrad$")),
        test_tag == "MIC" ~ stringr::str_trim(stringr::str_remove(sensitivityTest_noncdm, "\\sMIK$")),
        test_tag == "Zone" ~ stringr::str_trim(stringr::str_remove(sensitivityTest_noncdm, "\\sDisk$")),
        is.na(test_tag) ~ sensitivityTest_noncdm,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::mutate(
      test_tag = dplyr::case_when(
        !is.na(Antibiotic) & is.na(test_tag) ~ "AnySensTest",
        TRUE ~ test_tag
      )
    )
  
  # Process mechanism resistance results
  nonSensresults <- res %>%
    dplyr::filter(is.na(Antibiotic)) %>%
    dplyr::filter(test_tag != "CFUCount") %>%
    dplyr::select(RecordId, test_tag, sensitivityResult_noncdm) %>%
    dplyr::mutate(
      # Map resistance mechanism results using lookup
      test_tag = dplyr::case_when(
        test_tag == "ResVirMechanism" & 
          sensitivityResult_noncdm %in% Estonia_MecRes_Lookup$resistance_value[Estonia_MecRes_Lookup$resistance_type == "ResultESBL"] ~ "ResultESBL",
        test_tag == "ResVirMechanism" & 
          sensitivityResult_noncdm %in% Estonia_MecRes_Lookup$resistance_value[Estonia_MecRes_Lookup$resistance_type == "ResultCarbapenemase"] ~ "ResultCarbapenemase",
        test_tag == "ResVirMechanism" & 
          sensitivityResult_noncdm %in% Estonia_MecRes_Lookup$resistance_value[Estonia_MecRes_Lookup$resistance_type == "ResultPCRmec"] ~ "ResultPCRmec",
        TRUE ~ test_tag
      )
    ) %>%
    dplyr::mutate(
      Result_noncdm = sensitivityResult_noncdm
    ) %>%
    tidyr::pivot_wider(
      id_cols = RecordId,
      names_from = test_tag,
      values_from = Result_noncdm,
      values_fn = dplyr::first
    )
  
  # Apply resistance recoding to all result columns
  result_cols <- setdiff(names(nonSensresults), "RecordId")
  for (col in result_cols) {
    nonSensresults <- recode_with_lookup(nonSensresults, col, estonia_resrecode_lookup)
  }
  
  # Process antibiotic sensitivity results
  Sensresults <- res %>%
    dplyr::filter(!is.na(Antibiotic)) %>%
    dplyr::mutate(RecordIdAb = paste0(RecordId, "_", Antibiotic)) %>%
    dplyr::filter(test_tag %in% c("Zone", "MIC", "Grad")) %>%
    dplyr::mutate(
      SusceptibilitySign = stringr::str_extract(sensitivityValue_noncdm, "^<=|^<|^>=|^>|^="),
      Value = stringr::str_remove(sensitivityValue_noncdm, "^<=|^<|^>=|^>|^="),
      Value = as.numeric(Value),
      SIR = sensitivityResult_noncdm
    ) %>%
    dplyr::select(ParentId, RecordId, RecordIdAb, Antibiotic, test_tag, SIR, SusceptibilitySign, Value) %>%
    tidyr::pivot_wider(
      id_cols = c(ParentId, RecordId, RecordIdAb, Antibiotic),
      names_from = test_tag,
      values_from = c(SIR, SusceptibilitySign, Value),
      names_glue = "{test_tag}{.value}",
      values_fn = dplyr::first
    )
  
  # Combine results
  res <- Sensresults %>%
    dplyr::left_join(nonSensresults, by = "RecordId")
  
     # Translate antibiotic names
   res <- recode_with_lookup(res, "Antibiotic", estonia_est2eng_lookup)
   res <- recode_with_lookup(res, "Antibiotic", estonia_eng2hai_lookup)
   
   # Recode antibiotic names to HAI short format
   if (!is.null(metadata_path)) {
     res <- recode_to_HAI_short(
       data = res,
       metadata_path = metadata_path,
       long_col = "Antibiotic",
       short_col = "Antibiotic"
     )
   }
  
  # Final column organization
  res <- res %>%
    dplyr::mutate(RecordId = RecordIdAb) %>%
    dplyr::select(
      ParentId, RecordId, Antibiotic,
      dplyr::starts_with("Result"),
      dplyr::starts_with("Zone"),
      dplyr::starts_with("MIC"),
      dplyr::starts_with("Grad"),
      dplyr::everything()
    )
  
  res <- finalize_table(res, arrange_cols = c("RecordId", "Antibiotic"))
  
  return(res)
}

.create_estonia_ehrbsi_table <- function(recoded_data, reporting_year, episode_duration) {
  # Create lookup vectors using shared function
  estonia_hosptype_lookup <- create_lookup_vector(Estonia_HospType_Lookup, "hosptype_code", "estonia_hosptype")
  estonia_geog_lookup <- create_lookup_vector(Estonia_HospGeog_Lookup, "nuts3_code", "estonia_hosptype")
  
    # Calculate reporting year from admission dates in the data
  # Use the most recent year if data spans multiple years
  # NOTE: MUST BE UPDATED, NEED REPORTING YEAR TO BE PASSED TO AGGREGATE FUNCTION
  # THUS GIVING A NEW RECORD FOR EACH HOSP-YEAR INTHE DATA
  reporting_year <- max(as.numeric(format(recoded_data$DateOfHospitalAdmission, "%Y")), na.rm = TRUE)
  
  # Create base EHRBSI table using shared function
  ehrbsi <- create_base_ehrbsi_table(recoded_data, "EE", reporting_year, episode_duration)
  
  # Add Estonia-specific fields
  ehrbsi <- ehrbsi %>%
    dplyr::mutate(
      ClinicalTerminology = "ICD-10",
      ClinicalTerminologySpec = NA_character_,
      ESurvBSI = 2, # level of automation? Full/semi/denom/manual/etc
      HospitalSize = NA_real_, # how many beds?
      ProportionPopulationCovered = 1, # Liidia reports 100% coverage
      GeoLocation = dplyr::recode(HospitalId, !!!estonia_geog_lookup, .default = NA_character_),
      HospitalType = dplyr::recode(HospitalId, !!!estonia_hosptype_lookup, .default = HospitalType)
    )
  
  # Finalize table with standard column selection
  ehrbsi <- finalize_table(ehrbsi, get_standard_table_columns("ehrbsi"))
  
  return(ehrbsi)
}



