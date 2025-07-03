#' Process Estonia BSI data from raw format to EHR-BSI format
#'
#' @param input_file Name of the input Excel file, defaults to "BSI_REPORT_2024_share.xlsx"
#' @param input_file_path Path to the input file, defaults to the working directory
#' @param dictionary_path Path to the data dictionary Excel file
#' @param value_maps_path Path to the value maps R script
#' @param metadata_path Path to the metadata Excel file for antibiotic recoding
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
#' result <- process_estonia_bsi(
#'   input_file_path = "Estonia/data/raw",
#'   dictionary_path = "Estonia/data/reference/dictionary_raw_BSI_Estonia.xlsx",
#'   write_to_file = TRUE,
#'   write_to_file_path = "Estonia/data/formatted"
#' )
#' }

# Internal helper functions (not exported)
.process_basic_cleaning <- function(raw_data, reporting_year) {
  # Validate required columns exist
  required_cols <- c("DateOfSpecCollection", "DateOfHospitalAdmission", 
                    "HospitalId", "PatientId", "IsolateId")
  missing_cols <- setdiff(required_cols, names(raw_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
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
  
  # Create dataset IDs
  recoded_data <- recoded_data %>%
    dplyr::mutate(
      record_id_bsi = paste0(HospitalId),
      record_id_patient = paste0(PatientId, "-", admit_date_time),
      record_id_isolate = paste0(IsolateId, "_", MicroorganismCode)
    ) %>%
    dplyr::select(-admit_date_time, -sample_date_time)
  
  # Recode dates
  if (exists("getAnyDictionaryValue")) {
    DateVariables <- getAnyDictionaryValue(varname = "date", search = "type", value = "generic_name")
    recoded_data[, DateVariables] <- lapply(
      recoded_data[, DateVariables], 
      function(x) as.Date(x, format = "%d/%m/%Y %H:%M")
    )
  } else {
    # Fallback if dictionary function not available
    date_cols <- c("DateOfSpecCollection", "DateOfHospitalAdmission", "DateOfHospitalDischarge")
    available_date_cols <- intersect(date_cols, names(recoded_data))
    recoded_data[available_date_cols] <- lapply(
      recoded_data[available_date_cols],
      function(x) as.Date(x, format = "%d/%m/%Y %H:%M")
    )
  }
  
  return(recoded_data)
}

.create_patient_table <- function(recoded_data) {
  patient <- recoded_data %>%
    dplyr::mutate(
      RecordId = record_id_patient,
      ParentId = record_id_bsi,
      UnitId = paste0(HospitalId, "_", UnitSpecialtyShort),
      PatientSpecialty = NA_character_,
      patientType = "INPAT",
      OutcomeOfCase = NA_character_,
      HospitalisationCode = NA_character_,
      HospitalisationCodeLabel = NA_character_,
      HospitalisationAdmissionCodeSystem = "ICD-10",
      HospitalisationCodeSystemVersion = NA_character_,
      HospitalisationAdmissionCodeSystemSpec = NA_character_
    ) %>%
    dplyr::select(
      RecordId, ParentId, UnitId, UnitSpecialtyShort, PatientSpecialty, 
      DateOfAdmissionCurrentWard, PatientId, Age, Sex, patientType,
      DateOfHospitalAdmission, DateOfHospitalDischarge, OutcomeOfCase,
      HospitalisationCode, HospitalisationCodeLabel,
      HospitalisationAdmissionCodeSystem, HospitalisationCodeSystemVersion,
      HospitalisationAdmissionCodeSystemSpec, HospitalId
    ) %>%
    dplyr::distinct() %>%
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
    dplyr::ungroup() %>%
    dplyr::select(-gap_days, -prev_HospitalId, -HospitalId)
  
  return(patient)
}

.create_isolate_table <- function(recoded_data) {
  isolate <- recoded_data %>%
  dplyr::mutate(RecordId = record_id_isolate,
         ParentId = PatientId,
         LaboratoryCode = NA, # ^NOT INCLUDED IN DATA FOR SAFETY - LIIDIA^
         MicroorganismCodeSystem = "SNOMED-CT",
         MicroorganismCodeSystemSpec = NA, # always NA
         MicroorganismCodeSystemVersion = NA #  ^Specific Question for Liidia^
         ) %>%
  dplyr::select(RecordId,  ParentId, DateOfSpecCollection, LaboratoryCode, IsolateId, Specimen,
         MicroorganismCode, MicroorganismCodeLabel, MicroorganismCodeSystem, MicroorganismCodeSystemSpec,
         MicroorganismCodeSystemVersion) %>%
  dplyr::distinct()

  return(isolate)

}

.create_res_table <- function(recoded_data, metadata_path = NULL) {
  # Load required lookup tables from data folder
  load(file.path("data", "Estonia_MecRes_Lookup.rda"))
  load(file.path("data", "Estonia_ResRecode_Lookup.rda"))
  load(file.path("data", "Estonia_Ab_EST2ENG_Lookup.rda"))
  load(file.path("data", "Estonia_Ab_ENG2HAI_Lookup.rda"))
  
  # Create lookup vectors for easier use with dplyr::recode
  estonia_mecres_lookup <- setNames(Estonia_MecRes_Lookup$resistance_type, Estonia_MecRes_Lookup$resistance_value)
  estonia_resrecode_lookup <- setNames(Estonia_ResRecode_Lookup$generic_result, Estonia_ResRecode_Lookup$estonia_result)
  estonia_est2eng_lookup <- setNames(Estonia_Ab_EST2ENG_Lookup$english_name, Estonia_Ab_EST2ENG_Lookup$estonia_name)
  estonia_eng2hai_lookup <- setNames(Estonia_Ab_ENG2HAI_Lookup$generic_name, Estonia_Ab_ENG2HAI_Lookup$english_name)
  
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
      Result_noncdm = dplyr::recode(sensitivityResult_noncdm, !!!estonia_resrecode_lookup, .default = sensitivityResult_noncdm)
    ) %>%
    tidyr::pivot_wider(
      id_cols = RecordId,
      names_from = test_tag,
      values_from = Result_noncdm,
      values_fn = dplyr::first
    )
  
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
   res <- res %>%
     dplyr::mutate(
       Antibiotic = dplyr::recode(Antibiotic, !!!estonia_est2eng_lookup, .default = Antibiotic),
       Antibiotic = dplyr::recode(Antibiotic, !!!estonia_eng2hai_lookup, .default = Antibiotic)
     )
   
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
    ) %>%
    dplyr::arrange(RecordId, Antibiotic)
  
  return(res)
}

.create_ehrbsi_table <- function(recoded_data, reporting_year, episode_duration) {
  ehrbsi <- recoded_data %>%
  dplyr::mutate(AggregationLevel = "HOSP",
         ClinicalTerminology = "ICD-10",
         ClinicalTerminologySpec = NA,
         DataSource = "EE-EHRBSI",
         DateUsedForStatistics = reporting_year, # HARD-CODED - Liidia says this dataset is 2024, need to make adaptive
         EpisodeDuration = episode_duration, # selected as 'usual', episode function should adapt to this
         ESurvBSI = 2, # level of automation? Full/semi/denom/manual/etc
         GeoLocation = NA, # Liidia can provide NUTS3
         HospitalId = HospitalId,
         HospitalSize = NA, # how many beds?
         HospitalType = HospitalType,
         LaboratoryCode = NA, # will not be provided
         MicrobiologicalTerminology = "SNOMED-CT",
         MicrobiologicalTerminologySpec = NA,
         NumberOfBloodCultureSets = NA, # Liidia - provide data for prev year # blood culture sets
         NumberOfHOHABSIs = NA, # TO BE CALCULATED ONCE EPISODES CALC'D
         NumberOfHospitalDischarges = NA,
         NumberOfHospitalPatientDays = NA,
         NumberOfImportedHABSIs = NA, # TO BE CALCULATED ONCE EPISODES CALC'D
         NumberOfTotalBSIs = NA, # TO BE CALCULATED ONCE EPISODES CALC'D
         ProportionPopulationCovered = 1, # Liidia reports 100% coverage
         RecordId = record_id_bsi,
         RecordType = "EHRBSI",
         RecordTypeVersion = NA,
         ReportingCountry = "EE",
         Status = "New/Update",
         Subject = "EHRBSI" ) %>%
    dplyr::select(AggregationLevel
           ,ClinicalTerminology
           ,ClinicalTerminologySpec
           ,DataSource
           ,DateUsedForStatistics
           ,EpisodeDuration
           ,ESurvBSI
           ,GeoLocation
           ,HospitalId
           ,HospitalSize
           ,HospitalType
           ,LaboratoryCode
           ,MicrobiologicalTerminology
           ,MicrobiologicalTerminologySpec
           ,NumberOfBloodCultureSets
           ,NumberOfHOHABSIs
           ,NumberOfHospitalDischarges
           ,NumberOfHospitalPatientDays
           ,NumberOfImportedHABSIs
           ,NumberOfTotalBSIs
           ,ProportionPopulationCovered
           ,RecordId
           ,RecordType
           ,RecordTypeVersion
           ,ReportingCountry
           ,Status
           ,Subject
    ) %>%
    distinct()
  
  return(ehrbsi)
}

.write_output_files <- function(result_list, output_path) {
  # Write RDS files to the specified path
  saveRDS(result_list$ehrbsi, file.path(output_path, "1.EHRBSI.rds"))
  saveRDS(result_list$patient, file.path(output_path, "2.EHRBSI$Patient.rds"))
  saveRDS(result_list$isolate, file.path(output_path, "3.EHRBSI$Patient$Isolate.rds"))
  saveRDS(result_list$res, file.path(output_path, "4.EHRBSI$Patient$Isolate$Res.rds"))
}

