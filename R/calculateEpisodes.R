calculateEpisodes <- function(patient_df,
                              isolate_df,
                              commensal_df, 
                              episodeDuration = 14){
  comm_codes <- unique(commensal_df$SNOMED.Code)
  
  isolates_flagged <- isolate_df %>%
    mutate(org_type = if_else(MicroorganismCode %in% comm_codes,
                              "CC", "RP"))

  ## ------------------------------------------------------------------
  ## 2.  Attach admission dates so we know which isolates belong where
  ## ------------------------------------------------------------------
  iso_in_admission <- isolates_flagged %>%
    mutate(PatientId = ParentId) %>%
    select(-ParentId) %>%
    inner_join(patient_df %>%
                 select(AdmissionRecordId = RecordId,
                        PatientId,
                        HospitalId = ParentId,
                        DateOfHospitalAdmission,
                        DateOfHospitalDischarge),
               by = "PatientId",
               relationship = "many-to-many"  ) %>%
    filter(DateOfSpecCollection >= DateOfHospitalAdmission,
           is.na(DateOfHospitalDischarge) |
             DateOfSpecCollection <= DateOfHospitalDischarge)
  
  ## ---- RULE 1  – recognised pathogens (one pos = onset) ----------------
  rule1 <- iso_in_admission %>%
    filter(org_type == "RP") %>%
    transmute(AdmissionRecordId, PatientId, HospitalId, OnsetDate = DateOfSpecCollection,
              MicroorganismCode, BSI_case = TRUE, DateOfHospitalAdmission, DateOfHospitalDischarge)
  
  ## ---- RULE 2  – ≥2 concordant CC in 3 days ----------------------------
  rule2 <- iso_in_admission %>%
    filter(org_type == "CC", !is.na(DateOfSpecCollection)) %>%
    arrange(PatientId, MicroorganismCode, DateOfSpecCollection) %>%
    group_by(PatientId, MicroorganismCode) %>%
    mutate(cluster_first = flag_cc_clusters(DateOfSpecCollection, episodeDuration)) %>%
    ungroup() %>%
    filter(cluster_first) %>%
    transmute(AdmissionRecordId, PatientId, HospitalId, OnsetDate = DateOfSpecCollection,
              MicroorganismCode, BSI_case = TRUE, DateOfHospitalAdmission, DateOfHospitalDischarge)
  
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
      DaysSinceAdmission = as.numeric(EpisodeStartDate - DateOfHospitalAdmission, units = "days"),
      DaysAfterPrevDisch = as.numeric(EpisodeStartDate - PrevDischarge,  units = "days")
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
      episodeYear = as.numeric(format(EpisodeStartDate, "%Y"))
    )
  
  ## ── 4 · Return the enriched table ────────────────────────────────
  epi_full<- epi_full %>%
    select(-PrevDischarge, -DaysSinceAdmission, -DaysAfterPrevDisch) %>%
    relocate(EpisodeClass, EpisodeOrigin, episodeYear, .after = EpisodeStartDate) %>%
    distinct()
  
  
  
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
  
  return(epi_full)
  
}



aggregateEpisodes <- function(eps_df, ehrbsi) {
  # Aggregate to ehrbsi level, now including episodeYear as a grouping variable
  aggregateResults <- eps_df %>%
    select(HospitalId, EpisodeClass, EpisodeId, episodeYear) %>%
    distinct() %>%
    #mutate(RecordId = paste0(HospitalId,"-",episodeYear)) %>%
    group_by(HospitalId, EpisodeClass) %>%
    mutate(countEps = n()) %>%
    select(HospitalId, EpisodeClass, countEps) %>%
    distinct() %>%
    pivot_wider(names_from = EpisodeClass,
                values_from = countEps,
                id_cols = c(HospitalId)) %>%
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
  
  
  # Adding ParentId back to orig_df, now joining on both HospitalId and year
  ehrbsi <- ehrbsi %>%
    select(-NumberOfTotalBSIs,-NumberOfHOHABSIs,-NumberOfImportedHABSIs) %>%
    left_join(aggregateResults, by = c("RecordId"="HospitalId")) %>%
    select(RecordId
           ,RecordType
           ,RecordTypeVersion
           ,Subject
           ,Status
           ,DataSource
           ,ReportingCountry
           ,DateUsedForStatistics
           ,HospitalId
           ,LaboratoryCode
           ,GeoLocation
           ,HospitalSize
           ,HospitalType
           ,ESurvBSI
           ,AggregationLevel
           ,EpisodeDuration
           ,ClinicalTerminology
           ,ClinicalTerminologySpec
           ,MicrobiologicalTerminology
           ,MicrobiologicalTerminologySpec
           ,NumberOfBloodCultureSets
           ,NumberOfHospitalDischarges
           ,NumberOfHospitalPatientDays
           ,ProportionPopulationCovered
           ,NumberOfHOHABSIs
           ,NumberOfImportedHABSIs
           ,NumberOfCABSIs
           ,NumberOfTotalBSIs
    )
  
  # Overwrite aggregate table
  return(ehrbsi)
  
}
