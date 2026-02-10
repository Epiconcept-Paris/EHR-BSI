calculateEpisodes <- function(patient_df,
                              isolate_df,
                              commensal_df, 
                              episodeDuration = EPISODE_DURATION_DEFAULT){
  comm_codes <- unique(commensal_df$SNOMED.Code)
  
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
    filter(to_date(DateOfSpecimenCollection) >= to_date(DateOfHospitalAdmission),
           is.na(DateOfHospitalDischarge) |
             to_date(DateOfSpecimenCollection) <= to_date(DateOfHospitalDischarge))
  
  ## ---- RULE 1  – recognised pathogens (one pos = onset) ----------------
  rule1 <- iso_in_admission %>%
    filter(org_type == "RP") %>%
    transmute(AdmissionRecordId, PatientId, HospitalId, OnsetDate = to_date(DateOfSpecimenCollection),
              MicroorganismCode, MicroorganismCodeLabel, BSI_case = TRUE, DateOfHospitalAdmission, DateOfHospitalDischarge,
              org_type)
  
  ## ---- RULE 2  – ≥2 concordant CC in 3 days ----------------------------
  rule2 <- iso_in_admission %>%
    filter(org_type == "CC", !is.na(DateOfSpecimenCollection)) %>%
    arrange(PatientId, MicroorganismCode, DateOfSpecimenCollection) %>%
    group_by(PatientId, MicroorganismCode) %>%
    mutate(cluster_first = flag_cc_clusters(to_date(DateOfSpecimenCollection), episodeDuration)) %>%
    ungroup() %>%
    filter(cluster_first) %>%
    transmute(AdmissionRecordId, PatientId, HospitalId, OnsetDate = to_date(DateOfSpecimenCollection),
              MicroorganismCode, MicroorganismCodeLabel, BSI_case = TRUE, DateOfHospitalAdmission, DateOfHospitalDischarge,
              org_type)
  
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
  
  # Calculate episode-level characteristics before deduplication
  # Ensure org_type column exists
  if (!"org_type" %in% names(epi_core)) {
    warning("org_type column not found in epi_core. Setting AllCommensal to FALSE.")
    epi_core$org_type <- NA_character_
  }
  
  # Debug: Check org_type distribution
  message("calculateEpisodes: org_type distribution in epi_core:")
  message("  CC: ", sum(epi_core$org_type == "CC", na.rm = TRUE))
  message("  RP: ", sum(epi_core$org_type == "RP", na.rm = TRUE))
  message("  NA: ", sum(is.na(epi_core$org_type)))
  
  episode_chars <- epi_core %>%
    group_by(EpisodeId) %>%
    summarise(
      Polymicrobial = n_distinct(MicroorganismCode) > 1,
      # Episode is AllCommensal only if ALL isolates are CC type (no RPs at all)
      AllCommensal = if (all(is.na(org_type))) FALSE else all(org_type == "CC", na.rm = TRUE),
      .groups = "drop"
    )
  
  # Debug: Check AllCommensal distribution
  message("calculateEpisodes: AllCommensal distribution in episode_chars:")
  message("  TRUE (all CC): ", sum(episode_chars$AllCommensal, na.rm = TRUE))
  message("  FALSE (has RP): ", sum(!episode_chars$AllCommensal, na.rm = TRUE))
  
  # Deduplicate episodes and add characteristics
  epi_core <- epi_core %>% 
    group_by(EpisodeId) %>% 
    slice_head(n = 1) %>%           # or slice(1)
    ungroup() %>%
    left_join(episode_chars, by = "EpisodeId")
  
  
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
        !is.na(DaysSinceAdmission) & DaysSinceAdmission >= HO_HA_THRESHOLD_DAYS  ~ "HO-HA",
        !is.na(DaysAfterPrevDisch)  & DaysAfterPrevDisch  <= IMP_HA_THRESHOLD_DAYS ~ "IMP-HA",
        TRUE                                                                       ~ "CA"
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
  # Build column list dynamically to handle cases where columns might not exist
  episode_summary_cols <- c("EpisodeId", "EpisodeStartDate", "EpisodeClass", "EpisodeOrigin", "episodeYear", 
                            "AdmissionRecordId", "PatientId", "HospitalId")
  if ("Polymicrobial" %in% names(epi_full)) episode_summary_cols <- c(episode_summary_cols, "Polymicrobial")
  if ("AllCommensal" %in% names(epi_full)) episode_summary_cols <- c(episode_summary_cols, "AllCommensal")
  
  episode_summary <- epi_full %>%
    select(all_of(episode_summary_cols)) %>%
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
  # Check if AllCommensal column exists, if not, create it with FALSE values
  if (!"AllCommensal" %in% names(eps_df)) {
    eps_df$AllCommensal <- FALSE
    warning("AllCommensal column not found in episodes data. Setting all episodes to non-commensal.")
  }
  
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
  
  # Determine grouping column and year column, then build counts via shared helper
  if (aggregation_level == "HOSP") {
    aggregateResults <- build_episode_counts(eps_df, id_col = "HospitalId")
    
  } else if (aggregation_level == "HOSP-YEAR") {
    aggregateResults <- build_episode_counts(eps_df, id_col = "HospitalId", year_col = "episodeYear")
    
  } else if (aggregation_level == "LAB") {
    id_col <- if ("LaboratoryCode" %in% names(eps_df)) "LaboratoryCode" else "HospitalId"
    aggregateResults <- build_episode_counts(eps_df, id_col = id_col)
    
  } else if (aggregation_level == "LAB-YEAR") {
    id_col <- if ("LaboratoryCode" %in% names(eps_df)) "LaboratoryCode" else "HospitalId"
    aggregateResults <- build_episode_counts(eps_df, id_col = id_col, year_col = "episodeYear")
    
  } else {
    stop("Unknown aggregation_level: ", aggregation_level, 
         ". Must be one of: HOSP, HOSP-YEAR, LAB, LAB-YEAR", call. = FALSE)
  }
  
  # Join aggregated results back to ehrbsi table by RecordId
  # Remove the episode count columns if they exist (they'll be re-added by the join)
  cols_to_remove <- c("NumberOfTotalBSIs", "NumberOfHOHABSIs", "NumberOfImportedHABSIs", "NumberOfCABSIs",
                      "NumberOfTotalBSIs_CC", "NumberOfHOHABSIs_CC", "NumberOfImportedHABSIs_CC", "NumberOfCABSIs_CC")
  cols_to_remove <- intersect(cols_to_remove, names(ehrbsi))
  if (length(cols_to_remove) > 0) {
    ehrbsi <- ehrbsi %>% select(-all_of(cols_to_remove))
  }
  
  # Determine join key based on aggregation level
  # The EHRBSI table might have RecordId constructed differently, so we need to 
  # join on the appropriate grouping column rather than assuming RecordId matches
  if (aggregation_level == "HOSP") {
    # For HOSP: join on HospitalId
    join_col <- "HospitalId"
    # Rename RecordId to HospitalId in aggregateResults for the join
    names(aggregateResults)[names(aggregateResults) == "RecordId"] <- "HospitalId"
  } else if (aggregation_level == "HOSP-YEAR") {
    # For HOSP-YEAR: need to construct join key from HospitalId + Year
    join_col <- "join_key"
    if ("DateUsedForStatistics" %in% names(ehrbsi)) {
      ehrbsi$join_key <- paste0(ehrbsi$HospitalId, "-", ehrbsi$DateUsedForStatistics)
    } else {
      ehrbsi$join_key <- ehrbsi$HospitalId  # fallback
    }
    names(aggregateResults)[names(aggregateResults) == "RecordId"] <- "join_key"
  } else if (aggregation_level == "LAB") {
    # For LAB: join on LaboratoryCode (or HospitalId fallback)
    join_col <- if ("LaboratoryCode" %in% names(ehrbsi) && !all(is.na(ehrbsi$LaboratoryCode))) {
      "LaboratoryCode"
    } else {
      "HospitalId"
    }
    names(aggregateResults)[names(aggregateResults) == "RecordId"] <- join_col
  } else if (aggregation_level == "LAB-YEAR") {
    # For LAB-YEAR: need to construct join key
    join_col <- "join_key"
    if ("LaboratoryCode" %in% names(ehrbsi) && !all(is.na(ehrbsi$LaboratoryCode))) {
      if ("DateUsedForStatistics" %in% names(ehrbsi)) {
        ehrbsi$join_key <- paste0(ehrbsi$LaboratoryCode, "-", ehrbsi$DateUsedForStatistics)
      } else {
        ehrbsi$join_key <- ehrbsi$LaboratoryCode
      }
    } else {
      if ("DateUsedForStatistics" %in% names(ehrbsi)) {
        ehrbsi$join_key <- paste0(ehrbsi$HospitalId, "-", ehrbsi$DateUsedForStatistics)
      } else {
        ehrbsi$join_key <- ehrbsi$HospitalId
      }
    }
    names(aggregateResults)[names(aggregateResults) == "RecordId"] <- "join_key"
  } else {
    # Default: try RecordId
    join_col <- "RecordId"
  }
  
  # Ensure join column types match
  if (join_col %in% names(ehrbsi) && join_col %in% names(aggregateResults)) {
    ehrbsi[[join_col]] <- as.character(ehrbsi[[join_col]])
    aggregateResults[[join_col]] <- as.character(aggregateResults[[join_col]])
  }
  
  # Debug: print join information
  message("aggregateEpisodes: Joining by '", join_col, "'")
  message("  AggregateResults rows: ", nrow(aggregateResults))
  if (join_col %in% names(ehrbsi)) {
    message("  EHRBSI keys (sample): ", paste(head(unique(ehrbsi[[join_col]]), 5), collapse = ", "))
  }
  if (join_col %in% names(aggregateResults)) {
    message("  AggregateResults keys (sample): ", paste(head(unique(aggregateResults[[join_col]]), 5), collapse = ", "))
  }
  
  # Join the aggregated results
  ehrbsi <- ehrbsi %>%
    left_join(aggregateResults, by = join_col)
  
  # Clean up temporary join key if created
  if (join_col == "join_key" && "join_key" %in% names(ehrbsi)) {
    ehrbsi <- ehrbsi %>% select(-join_key)
  }
  
  # Debug: verify join results
  if ("NumberOfTotalBSIs" %in% names(ehrbsi)) {
    non_na <- sum(!is.na(ehrbsi$NumberOfTotalBSIs))
    message("  After join: ", non_na, " of ", nrow(ehrbsi), " rows have episode counts")
  }
  
  # Reorder columns to match expected output (only select columns that exist)
  desired_cols <- c(
    "RecordId", "RecordType", "RecordTypeVersion", "Subject", "Status",
    "DataSource", "ReportingCountry", "DateUsedForStatistics", "HospitalId",
    "LaboratoryCode", "GeoLocation", "HospitalSize", "HospitalType", "ESurvBSI",
    "AggregationLevel", "EpisodeDuration", "ClinicalTerminology", "ClinicalTerminologySpec",
    "MicrobiologicalTerminology", "MicrobiologicalTerminologySpec",
    "NumberOfBloodCultureSets", "NumberOfHospitalDischarges", "NumberOfHospitalPatientDays",
    "ProportionPopulationCovered", "NumberOfHOHABSIs", "NumberOfImportedHABSIs",
    "NumberOfCABSIs", "NumberOfTotalBSIs",
    "NumberOfHOHABSIs_CC", "NumberOfImportedHABSIs_CC", "NumberOfCABSIs_CC", "NumberOfTotalBSIs_CC"
  )
  existing_cols <- intersect(desired_cols, names(ehrbsi))
  ehrbsi <- ehrbsi %>% select(all_of(existing_cols))
  
  # Overwrite aggregate table
  return(ehrbsi)
  
}
