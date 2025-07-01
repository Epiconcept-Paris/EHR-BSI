


createBSIdf <- function(patient_df,
                        isolate_df,
                        commensal_df){
  
  comm_codes <- unique(commensal_df$MicroorganismCode)
  
  isolates_flagged <- isolate_df %>%
    rename(IsolateRecordId = RecordId) %>% 
    mutate(org_type = if_else(MicroorganismCode %in% comm_codes,
                              "CC", "RP"))
  
  ## ------------------------------------------------------------------
  ## 2.  Attach admission dates so we know which isolates belong where
  ## ------------------------------------------------------------------
  iso_in_admission <- isolates_flagged %>%
    inner_join(patient_df %>%
                 select(AdmissionRecordId = RecordId,
                        PatientId,
                        DateOfHospitalAdmission,
                        DateOfHospitalDischarge),
               by = c("ParentId" = "PatientId"),
               relationship = "many-to-many"  ) %>%
    filter(DateOfSpecCollection >= DateOfHospitalAdmission,
           is.na(DateOfHospitalDischarge) |
             DateOfSpecCollection <= DateOfHospitalDischarge)
  
  ## ------------------------------------------------------------------
  ## 3A.  Rule-1 admissions  ─ recognised pathogen
  ## ------------------------------------------------------------------
  rule1 <- iso_in_admission %>%
    filter(org_type == "RP") %>%
    group_by(AdmissionRecordId, MicroorganismCode) %>%
    summarise(onset_date_pathogen = min(DateOfSpecCollection),
              .groups = "drop") %>%
    ungroup()
  
  ## ------------------------------------------------------------------
  ## 3B.  Rule-2 admissions  ─ ≥2 concordant CC within 3 calendar days
  ## ------------------------------------------------------------------
  rule2 <- iso_in_admission %>%                     # still inside createBSIdf()
    filter(org_type == "CC") %>%
    group_by(AdmissionRecordId, MicroorganismCode) %>%       # keep species
    mutate(first_date = min(DateOfSpecCollection)) %>%       # day-1 of window
    summarise(
      n_distinct_samples = n_distinct(
        IsolateId[ DateOfSpecCollection <= first_date + days(2) ] ), # use isolate ID to ensure separate cultures
      onset_date_cc      = first(first_date),
      .groups = "drop"
    ) %>%
    filter(n_distinct_samples >= 2)          
  
  ## ------------------------------------------------------------------
  ## 4.  Combine the two rules to create overall BSI flag + onset
  ## ------------------------------------------------------------------
  bsi_combined <- full_join(rule1, rule2, 
                            by = c("AdmissionRecordId", "MicroorganismCode")) %>%
    group_by(AdmissionRecordId, MicroorganismCode) %>%
    mutate(
      OnsetDate = pmin(onset_date_pathogen, onset_date_cc, na.rm = TRUE),
      BSI_case   = TRUE
    ) %>%
    select(AdmissionRecordId, MicroorganismCode, BSI_case, OnsetDate)
  
  ## ------------------------------------------------------------------
  ## 5.  Add the results back to the patient/admission table
  ## ------------------------------------------------------------------
  
  
  bsi_df <- patient_df %>%
    left_join(bsi_combined, by = c("RecordId" = "AdmissionRecordId")) %>%
    mutate(
      BSI_case   = if_else(is.na(BSI_case), FALSE, BSI_case),
      OnsetDate = as.Date(OnsetDate)          # drop time component
    ) %>% 
    select(RecordId, PatientId, ParentId, BSI_case, MicroorganismCode, OnsetDate) %>%
    distinct()
  
  return(bsi_df)
}



# ----------------------------
# Function that assigns episode id within one patient
# ----------------------------
assign_episodes <- function(df_one_pt, episodeDuration) {
  ep_id      <- 0L
  active_yet <- FALSE
  
  epi_vec    <- integer(nrow(df_one_pt))
  start_vec  <- as.Date(rep(NA, nrow(df_one_pt)))
  
  for (i in seq_len(nrow(df_one_pt))) {
    cur_date <- df_one_pt$OnsetDate[i]
    cur_org  <- df_one_pt$MicroorganismCode[i]
    
    if (!active_yet) {
      ep_id      <- ep_id + 1L
      active_yet <- TRUE
      epi_start  <- cur_date
      epi_orgs   <- cur_org
    } else {
      gap_days <- as.integer(cur_date - epi_start)
      same_org <- cur_org %in% epi_orgs
      
      need_new <- case_when(
        same_org  & gap_days <= as.numeric(episodeDuration-1) ~ FALSE,
        !same_org & gap_days <= 2L  ~ FALSE,
        TRUE                        ~ TRUE
      )
      
      if (need_new) {
        ep_id     <- ep_id + 1L
        epi_start <- cur_date
        epi_orgs  <- cur_org
      } else {
        epi_orgs  <- union(epi_orgs, cur_org)
      }
    }
    epi_vec[i]   <- ep_id
    start_vec[i] <- epi_start
  }
  
  df_one_pt %>%
    mutate(EpisodeNumber    = epi_vec,
           EpisodeStartDate = start_vec)
}


defineEpisodes <- function(bsi_df, episodeDuration = 14) {
  
  
  # Take the df with defined BSI cases (based on one RP or two CCs)
  onset_tbl <- bsi_df %>%
    filter(BSI_case, !is.na(OnsetDate)) %>% 
    select(PatientId, OnsetDate, MicroorganismCode, RecordId) %>%
    arrange(PatientId, OnsetDate)
  
  
  # ----------------------------
  #  Apply the episode check for every patient
  # ----------------------------
  epi_core <- onset_tbl %>% 
    group_by(PatientId) %>%
    group_modify(~assign_episodes(.x, episodeDuration)) %>% 
    ungroup() %>%
    mutate(EpisodeId = paste0(PatientId, "_E", sprintf("%03d", EpisodeNumber)))
  
  
  
  episode_tbl <- epi_core %>% 
    group_by(EpisodeId, PatientId, EpisodeStartDate) %>% 
    summarise(
      Polymicrobial = n_distinct(MicroorganismCode) > 1,
      RecordId = first(RecordId),
      .groups = "drop"
    )
  
  
  return(episode_tbl)
  
}



## --------------------------------------------------------------------
## episodes_df  – result of defineEpisodes() or definePolyEpisodes()
## patient_df   – original PATIENT table (must contain RecordId,
##                PatientId, DateOfHospitalAdmission, DateOfHospitalDischarge)
## --------------------------------------------------------------------
classifyEpisodeOrigin <- function(episodes_df,
                                  patient_df) {
  
  ## ── 1 · Admission & discharge dates per admission ────────────────
  # adm_tbl <- patient_df %>%
  #   select(RecordId,
  #          PatientId,
  #          AdmissionDate = DateOfHospitalAdmission,
  #          DischargeDate = DateOfHospitalDischarge) %>%
  #   distinct()
  
  adm_tbl <- patient_df %>% 
    ## collapse to ONE row per admission id
    group_by(RecordId) %>% 
    summarise(
      PatientId     = first(PatientId),
      ParentId     = first(ParentId),
      AdmissionDate = min(DateOfHospitalAdmission,  na.rm = TRUE),
      ## take the latest non-NA discharge; if all NA → keep NA
      DischargeDate = { d <- DateOfHospitalDischarge[!is.na(DateOfHospitalDischarge)]
      if (length(d) == 0) NA_real_ else max(d) },
      .groups = "drop"
    ) %>%
    ungroup()
  
  ## add the *previous* discharge date for each patient
  adm_tbl <- adm_tbl %>%
    arrange(PatientId, AdmissionDate) %>%
    group_by(PatientId) %>%
    mutate(PrevDischarge = lag(DischargeDate)) %>%
    ungroup()
  
  ## ── 2 · Merge admission info into the episode table ──────────────
  epi_full <- episodes_df %>%
    select(-PatientId) %>%
    left_join(adm_tbl, by = "RecordId") %>%
    ## day-of-stay is counted with admission = day 1
    mutate(
      DaysSinceAdmission = as.numeric(EpisodeStartDate - AdmissionDate, units = "days"),
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
      EpisodeOrigin = if_else(EpisodeClass == "CA", "Community", "Healthcare")
    )
  
  ## ── 4 · Return the enriched table ────────────────────────────────
  epi_full %>%
    select(-PrevDischarge, -DaysSinceAdmission, -DaysAfterPrevDisch) %>%
    relocate(EpisodeClass, EpisodeOrigin, .after = EpisodeStartDate)
}

