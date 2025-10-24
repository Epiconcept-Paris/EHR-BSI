calculateEpisodes <- function(patient_df,
                              isolate_df,
                              commensal_df, 
                              episodeDuration = 14){
  comm_codes <- unique(commensal_df$SNOMED.Code)
  
  # Robust date coercion: supports Date/POSIX, ISO, EU/US, with/without time, and Excel serials
  to_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    # Excel serials or numeric-like strings
    if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
    if (is.character(x)) {
      xs <- trimws(x)
      num_idx <- suppressWarnings(!is.na(as.numeric(xs)))
      out <- rep(as.Date(NA), length(xs))
      if (any(num_idx)) {
        out[num_idx] <- as.Date(as.numeric(xs[num_idx]), origin = "1899-12-30")
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
  
  isolates_flagged <- isolate_df %>%
    mutate(org_type = if_else(MicroorganismCode %in% comm_codes,
                              "CC", "RP"))

  ## ------------------------------------------------------------------
  ## 2.  Attach admission dates so we know which isolates belong where
  ## ------------------------------------------------------------------
  # ParentId in isolate now links to patient's RecordId (not PatientId)
  iso_in_admission <- isolates_flagged %>%
    inner_join(patient_df %>%
                 select(AdmissionRecordId = RecordId,
                        PatientId,
                        HospitalId,
                        DateOfHospitalAdmission,
                        DateOfHospitalDischarge),
               by = c("ParentId" = "AdmissionRecordId"),
               relationship = "many-to-many"  ) %>%
    # Rename ParentId to AdmissionRecordId for downstream use
    rename(AdmissionRecordId = ParentId) %>%
    # Convert dates to Date class for consistent comparison (handles POSIXct/character)
    filter(to_date(DateOfSpecCollection) >= to_date(DateOfHospitalAdmission),
           is.na(DateOfHospitalDischarge) |
             to_date(DateOfSpecCollection) <= to_date(DateOfHospitalDischarge))
  
  ## ---- RULE 1  – recognised pathogens (one pos = onset) ----------------
  rule1 <- iso_in_admission %>%
    filter(org_type == "RP") %>%
    transmute(AdmissionRecordId, PatientId, HospitalId, OnsetDate = to_date(DateOfSpecCollection),
              MicroorganismCode, MicroorganismCodeLabel, BSI_case = TRUE, DateOfHospitalAdmission, DateOfHospitalDischarge)
  
  ## ---- RULE 2  – ≥2 concordant CC in 3 days ----------------------------
  rule2 <- iso_in_admission %>%
    filter(org_type == "CC", !is.na(DateOfSpecCollection)) %>%
    arrange(PatientId, MicroorganismCode, DateOfSpecCollection) %>%
    group_by(PatientId, MicroorganismCode) %>%
    mutate(cluster_first = flag_cc_clusters(to_date(DateOfSpecCollection), episodeDuration)) %>%
    ungroup() %>%
    filter(cluster_first) %>%
    transmute(AdmissionRecordId, PatientId, HospitalId, OnsetDate = to_date(DateOfSpecCollection),
              MicroorganismCode, MicroorganismCodeLabel, BSI_case = TRUE, DateOfHospitalAdmission, DateOfHospitalDischarge)
  
  bsi_core <- bind_rows(rule1, rule2) %>%
    distinct()
  
  
  # ----------------------------
  #  Apply the episode check for every patient
  # ----------------------------
  epi_core <- bsi_core %>%
    arrange(PatientId, OnsetDate) %>%
    group_by(PatientId) %>%      # ➊ count episodes across all admissions for each patient
    group_modify(~assign_episodes(.x, episodeDuration)) %>%
    ungroup() %>%
    mutate(EpisodeId = paste0(EpisodeStartDate, "-", PatientId, "-",
                              sprintf("%01d", EpisodeNumber)))
  
  epi_core <- epi_core %>% 
    group_by(EpisodeId) %>% 
    mutate(Polymicrobial = n_distinct(MicroorganismCode) > 1) %>% 
    slice_head(n = 1) %>%           # or slice(1)
    ungroup()
  
  
  ## add the *previous* discharge date for each patient
  adm_tbl <- patient_df %>%
    select(RecordId, PatientId, DateOfHospitalAdmission,DateOfHospitalDischarge) %>%
    distinct() %>%
    arrange(PatientId, DateOfHospitalAdmission,DateOfHospitalDischarge) %>%
    group_by(PatientId) %>%
    mutate(PrevDischarge = lag(DateOfHospitalDischarge)) %>%
    ungroup() %>%
    filter(!is.na(PrevDischarge)) %>%
    select(RecordId, PatientId, PrevDischarge) %>%
    distinct()
  
  ## ── 2 · Merge admission info into the episode table ──────────────
  epi_full <- epi_core %>%
    select(-PatientId) %>%
    left_join(adm_tbl, by = c("AdmissionRecordId"="RecordId")) %>%
    ## day-of-stay is counted with admission = day 1
    mutate(
      # Convert both dates to Date class to ensure compatible subtraction (handles POSIXct from Estonia)
      DaysSinceAdmission = as.numeric(as.Date(EpisodeStartDate) - as.Date(DateOfHospitalAdmission), units = "days"),
      DaysAfterPrevDisch = as.numeric(as.Date(EpisodeStartDate) - as.Date(PrevDischarge), units = "days")
    )
  
  ## ── 3 · Apply the decision tree for the case definition ──────────────
  epi_full <- epi_full %>%
    mutate(
      EpisodeClass = case_when(
        !is.na(DaysSinceAdmission) & DaysSinceAdmission >= 2               ~ "HO-HA",
        !is.na(DaysAfterPrevDisch)  & DaysAfterPrevDisch  <= 2            ~ "IMP-HA",
        TRUE                                                             ~ "CA"
      ),
      EpisodeOrigin = if_else(EpisodeClass == "CA", "Community", "Healthcare"),
      # Add episodeYear extracted from EpisodeStartDate
      episodeYear = as.numeric(format(as.Date(EpisodeStartDate), "%Y"))
    )
  
  ## ── 4 · Return the enriched table ────────────────────────────────
  epi_full<- epi_full %>%
    select(-PrevDischarge, -DaysSinceAdmission, -DaysAfterPrevDisch) %>%
    relocate(EpisodeClass, EpisodeOrigin, episodeYear, .after = EpisodeStartDate) %>%
    distinct()
  
  
  ## ── 5 · Create episode summary table (one row per episode with pathogen info) ────
  # Get all pathogens per episode from bsi_core (before deduplication)
  episode_pathogens <- bsi_core %>%
    arrange(PatientId, OnsetDate) %>%
    group_by(PatientId) %>%
    group_modify(~assign_episodes(.x, episodeDuration)) %>%
    ungroup() %>%
    mutate(EpisodeId = paste0(EpisodeStartDate, "-", PatientId, "-",
                              sprintf("%01d", EpisodeNumber))) %>%
    group_by(EpisodeId) %>%
    summarise(
      # Use MicroorganismCodeLabel if available, fallback to MicroorganismCode
      Pathogens = if ("MicroorganismCodeLabel" %in% names(cur_data())) {
        paste(sort(unique(MicroorganismCodeLabel)), collapse = "; ")
      } else {
        paste(sort(unique(MicroorganismCode)), collapse = "; ")
      },
      PathogenCount = n_distinct(MicroorganismCode),
      Polymicrobial = n_distinct(MicroorganismCode) > 1,
      .groups = "drop"
    )
  
  # Join with episode metadata
  episode_summary <- epi_full %>%
    select(EpisodeId, EpisodeStartDate, EpisodeClass, EpisodeOrigin, episodeYear, 
           AdmissionRecordId, PatientId, HospitalId) %>%
    distinct() %>%
    left_join(episode_pathogens, by = "EpisodeId")
  
  
  # Basic df for calculating some stats
  calc_df <- epi_full %>%
    select(EpisodeId, EpisodeOrigin) %>%
    filter(!is.na(EpisodeId)) %>%
    distinct()
  
  
  # Print stats
  cat("TOTAL BSI episodes: ", length(unique(calc_df$EpisodeId))," \n ", 
      "OF WHICH COMMUNITY-ACQUIRED: ", sum(calc_df$EpisodeOrigin=="Community"), "(",
      round(((sum(calc_df$EpisodeOrigin=="Community")/length(unique(calc_df$EpisodeId)))*100),1),"%)"," \n ", 
      "VS HOSP-ACQUIRED: ", sum(calc_df$EpisodeOrigin=="Healthcare"), "(",
      round(((sum(calc_df$EpisodeOrigin=="Healthcare")/length(unique(calc_df$EpisodeId)))*100),1),"%)")
  
  return(list(episodes = epi_full, episode_summary = episode_summary))
  
}



aggregateEpisodes <- function(eps_df, ehrbsi, aggregation_level = "HOSP", hospital_lab_map = NULL) {
  # Use provided hospital-to-lab mapping, or extract from ehrbsi if not provided
  if (is.null(hospital_lab_map) && aggregation_level %in% c("LAB", "LAB-YEAR")) {
    if ("LaboratoryCode" %in% names(ehrbsi) && "HospitalId" %in% names(ehrbsi)) {
      hospital_lab_map <- ehrbsi %>%
        select(HospitalId, LaboratoryCode) %>%
        distinct() %>%
        filter(!is.na(LaboratoryCode))  # Only keep rows where LaboratoryCode is not NA
    }
  }
  
  # Add LaboratoryCode to eps_df if we have the mapping
  if (!is.null(hospital_lab_map) && nrow(hospital_lab_map) > 0 && "HospitalId" %in% names(eps_df)) {
    # Get unique values BEFORE conversion for diagnostics
    eps_hosp_ids_original <- unique(eps_df$HospitalId)
    map_hosp_ids_original <- unique(hospital_lab_map$HospitalId)
    
    # Ensure HospitalId is character type in both data frames for proper matching
    eps_df <- eps_df %>%
      mutate(HospitalId = as.character(HospitalId))
    
    hospital_lab_map <- hospital_lab_map %>%
      mutate(HospitalId = as.character(HospitalId),
             LaboratoryCode = as.character(LaboratoryCode))
    
    # Perform the join
    eps_df <- eps_df %>%
      left_join(hospital_lab_map, by = "HospitalId")
    
    # Check if join worked
    if (all(is.na(eps_df$LaboratoryCode))) {
      warning("LAB aggregation: left_join failed to match any HospitalIds.\n",
              "  HospitalIds in episodes (n=", length(eps_hosp_ids_original), "): ", 
              paste(head(eps_hosp_ids_original, 5), collapse = ", "), 
              if (length(eps_hosp_ids_original) > 5) "..." else "", "\n",
              "  HospitalIds in mapping (n=", length(map_hosp_ids_original), "): ", 
              paste(head(map_hosp_ids_original, 5), collapse = ", "),
              if (length(map_hosp_ids_original) > 5) "..." else "", "\n",
              "  Using HospitalId as fallback for LAB aggregation.",
              call. = FALSE)
    }
  }
  
  # Determine grouping columns and RecordId creation based on aggregation level
  if (aggregation_level == "HOSP") {
    # Group by Hospital only
    aggregateResults <- eps_df %>%
      select(HospitalId, EpisodeClass, EpisodeId) %>%
      distinct() %>%
      mutate(RecordId = HospitalId) %>%
      group_by(RecordId, EpisodeClass) %>%
      summarise(countEps = n(), .groups = "drop") %>%
      pivot_wider(names_from = EpisodeClass,
                  values_from = countEps,
                  id_cols = c(RecordId)) %>%
      # Ensure all episode class columns exist (even if 0 episodes of that type)
      mutate(CA = if ("CA" %in% names(.)) CA else 0,
             `HO-HA` = if ("HO-HA" %in% names(.)) `HO-HA` else 0,
             `IMP-HA` = if ("IMP-HA" %in% names(.)) `IMP-HA` else 0) %>%
      mutate(NumberOfCABSIs = case_when(
               is.na(CA) ~ 0,
               TRUE ~ CA
             ),
             NumberOfHOHABSIs = 
               case_when(
                 is.na(`HO-HA`)~0,
                 TRUE~`HO-HA`
               ),
             NumberOfImportedHABSIs =           
               case_when(
                 is.na(`IMP-HA`)~0,
                 TRUE~`IMP-HA`
               ),
             NumberOfTotalBSIs = NumberOfCABSIs+NumberOfHOHABSIs+NumberOfImportedHABSIs) %>%
      select(-CA,-`HO-HA`,-`IMP-HA`)
    
  } else if (aggregation_level == "HOSP-YEAR") {
    # Group by Hospital and Year
    aggregateResults <- eps_df %>%
      select(HospitalId, EpisodeClass, EpisodeId, episodeYear) %>%
      distinct() %>%
      mutate(RecordId = paste0(HospitalId, "-", episodeYear)) %>%
      group_by(RecordId, EpisodeClass) %>%
      summarise(countEps = n(), .groups = "drop") %>%
      pivot_wider(names_from = EpisodeClass,
                  values_from = countEps,
                  id_cols = c(RecordId)) %>%
      # Ensure all episode class columns exist (even if 0 episodes of that type)
      mutate(CA = if ("CA" %in% names(.)) CA else 0,
             `HO-HA` = if ("HO-HA" %in% names(.)) `HO-HA` else 0,
             `IMP-HA` = if ("IMP-HA" %in% names(.)) `IMP-HA` else 0) %>%
      mutate(NumberOfCABSIs = case_when(
               is.na(CA) ~ 0,
               TRUE ~ CA
             ),
             NumberOfHOHABSIs = 
               case_when(
                 is.na(`HO-HA`)~0,
                 TRUE~`HO-HA`
               ),
             NumberOfImportedHABSIs =           
               case_when(
                 is.na(`IMP-HA`)~0,
                 TRUE~`IMP-HA`
               ),
             NumberOfTotalBSIs = NumberOfCABSIs+NumberOfHOHABSIs+NumberOfImportedHABSIs) %>%
      select(-CA,-`HO-HA`,-`IMP-HA`)
    
  } else if (aggregation_level == "LAB") {
    # Group by Laboratory (use HospitalId as fallback if LaboratoryCode not available)
    if ("LaboratoryCode" %in% names(eps_df)) {
      aggregateResults <- eps_df %>%
        select(LaboratoryCode, EpisodeClass, EpisodeId) %>%
        distinct() %>%
        mutate(RecordId = LaboratoryCode) %>%
        group_by(RecordId, EpisodeClass) %>%
        summarise(countEps = n(), .groups = "drop") %>%
        pivot_wider(names_from = EpisodeClass,
                    values_from = countEps,
                    id_cols = c(RecordId)) %>%
        mutate(NumberOfCABSIs = case_when(
                 is.na(CA) ~ 0,
                 TRUE ~ CA
               ),
               NumberOfHOHABSIs = 
                 case_when(
                   is.na(`HO-HA`)~0,
                   TRUE~`HO-HA`
                 ),
               NumberOfImportedHABSIs =           
                 case_when(
                   is.na(`IMP-HA`)~0,
                   TRUE~`IMP-HA`
                 ),
               NumberOfTotalBSIs = NumberOfCABSIs+NumberOfHOHABSIs+NumberOfImportedHABSIs) %>%
        select(-CA,-`HO-HA`,-`IMP-HA`)
    } else {
      # Fallback to HospitalId if no LaboratoryCode
      aggregateResults <- eps_df %>%
        select(HospitalId, EpisodeClass, EpisodeId) %>%
        distinct() %>%
        mutate(RecordId = HospitalId) %>%
        group_by(RecordId, EpisodeClass) %>%
        summarise(countEps = n(), .groups = "drop") %>%
        pivot_wider(names_from = EpisodeClass,
                    values_from = countEps,
                    id_cols = c(RecordId)) %>%
        mutate(NumberOfCABSIs = case_when(
                 is.na(CA) ~ 0,
                 TRUE ~ CA
               ),
               NumberOfHOHABSIs = 
                 case_when(
                   is.na(`HO-HA`)~0,
                   TRUE~`HO-HA`
                 ),
               NumberOfImportedHABSIs =           
                 case_when(
                   is.na(`IMP-HA`)~0,
                   TRUE~`IMP-HA`
                 ),
               NumberOfTotalBSIs = NumberOfCABSIs+NumberOfHOHABSIs+NumberOfImportedHABSIs) %>%
        select(-CA,-`HO-HA`,-`IMP-HA`)
    }
    
  } else if (aggregation_level == "LAB-YEAR") {
    # Group by Laboratory and Year
    if ("LaboratoryCode" %in% names(eps_df)) {
      aggregateResults <- eps_df %>%
        select(LaboratoryCode, EpisodeClass, EpisodeId, episodeYear) %>%
        distinct() %>%
        mutate(RecordId = paste0(LaboratoryCode, "-", episodeYear)) %>%
        group_by(RecordId, EpisodeClass) %>%
        summarise(countEps = n(), .groups = "drop") %>%
        pivot_wider(names_from = EpisodeClass,
                    values_from = countEps,
                    id_cols = c(RecordId)) %>%
        mutate(NumberOfCABSIs = case_when(
                 is.na(CA) ~ 0,
                 TRUE ~ CA
               ),
               NumberOfHOHABSIs = 
                 case_when(
                   is.na(`HO-HA`)~0,
                   TRUE~`HO-HA`
                 ),
               NumberOfImportedHABSIs =           
                 case_when(
                   is.na(`IMP-HA`)~0,
                   TRUE~`IMP-HA`
                 ),
               NumberOfTotalBSIs = NumberOfCABSIs+NumberOfHOHABSIs+NumberOfImportedHABSIs) %>%
        select(-CA,-`HO-HA`,-`IMP-HA`)
    } else {
      # Fallback to HospitalId if no LaboratoryCode
      aggregateResults <- eps_df %>%
        select(HospitalId, EpisodeClass, EpisodeId, episodeYear) %>%
        distinct() %>%
        mutate(RecordId = paste0(HospitalId, "-", episodeYear)) %>%
        group_by(RecordId, EpisodeClass) %>%
        summarise(countEps = n(), .groups = "drop") %>%
        pivot_wider(names_from = EpisodeClass,
                    values_from = countEps,
                    id_cols = c(RecordId)) %>%
        mutate(NumberOfCABSIs = case_when(
                 is.na(CA) ~ 0,
                 TRUE ~ CA
               ),
               NumberOfHOHABSIs = 
                 case_when(
                   is.na(`HO-HA`)~0,
                   TRUE~`HO-HA`
                 ),
               NumberOfImportedHABSIs =           
                 case_when(
                   is.na(`IMP-HA`)~0,
                   TRUE~`IMP-HA`
                 ),
               NumberOfTotalBSIs = NumberOfCABSIs+NumberOfHOHABSIs+NumberOfImportedHABSIs) %>%
        select(-CA,-`HO-HA`,-`IMP-HA`)
    }
    
  } else {
    stop("Unknown aggregation_level: ", aggregation_level, 
         ". Must be one of: HOSP, HOSP-YEAR, LAB, LAB-YEAR", call. = FALSE)
  }
  
  # Join aggregated results back to ehrbsi table by RecordId
  # Remove the episode count columns if they exist (they'll be re-added by the join)
  cols_to_remove <- c("NumberOfTotalBSIs", "NumberOfHOHABSIs", "NumberOfImportedHABSIs", "NumberOfCABSIs")
  cols_to_remove <- intersect(cols_to_remove, names(ehrbsi))
  if (length(cols_to_remove) > 0) {
    ehrbsi <- ehrbsi %>% select(-all_of(cols_to_remove))
  }
  
  # Join the aggregated results
  ehrbsi <- ehrbsi %>%
    left_join(aggregateResults, by = "RecordId")
  
  # Reorder columns to match expected output (only select columns that exist)
  desired_cols <- c(
    "RecordId", "RecordType", "RecordTypeVersion", "Subject", "Status",
    "DataSource", "ReportingCountry", "DateUsedForStatistics", "HospitalId",
    "LaboratoryCode", "GeoLocation", "HospitalSize", "HospitalType", "ESurvBSI",
    "AggregationLevel", "EpisodeDuration", "ClinicalTerminology", "ClinicalTerminologySpec",
    "MicrobiologicalTerminology", "MicrobiologicalTerminologySpec",
    "NumberOfBloodCultureSets", "NumberOfHospitalDischarges", "NumberOfHospitalPatientDays",
    "ProportionPopulationCovered", "NumberOfHOHABSIs", "NumberOfImportedHABSIs",
    "NumberOfCABSIs", "NumberOfTotalBSIs"
  )
  existing_cols <- intersect(desired_cols, names(ehrbsi))
  ehrbsi <- ehrbsi %>% select(all_of(existing_cols))
  
  # Overwrite aggregate table
  return(ehrbsi)
  
}
