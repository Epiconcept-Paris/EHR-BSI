
## helper ─ find all ≥2-positive CC windows inside the admission
flag_cc_clusters <- function(dates) {
  dates <- sort(as.Date(dates[!is.na(dates)]))   # keep only real dates
  n     <- length(dates)
  out   <- logical(n)
  
  i <- 1L
  while (i < n) {
    
    if (dates[i + 1] - dates[i] <= 2) {          # 2nd culture within 3 days?
      out[i] <- TRUE                             # → mark start of the cluster
      
      ## jump to first sample that is *outside* the 14-day episode window
      nxt <- which(dates > dates[i] + 13)
      if (length(nxt) == 0) break                # none found → we are done
      i <- nxt[1]
    } else {
      i <- i + 1L
    }
  }
  out
}


createBSIdf <- function(patient_df,
                        isolate_df,
                        commensal_df){
  
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
    mutate(cluster_first = flag_cc_clusters(DateOfSpecCollection)) %>%
    ungroup() %>%
    filter(cluster_first) %>%
    transmute(AdmissionRecordId, PatientId, HospitalId, OnsetDate = DateOfSpecCollection,
              MicroorganismCode, BSI_case = TRUE, DateOfHospitalAdmission, DateOfHospitalDischarge)
  
  bsi_core <- bind_rows(rule1, rule2) %>%
    distinct()
  
  
  return(bsi_core)
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


defineEpisodes <- function(bsi_core, episodeDuration = 14) {
  
  
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
  
  return(epi_core)
  
}



## --------------------------------------------------------------------
## episodes_df  – result of defineEpisodes() or definePolyEpisodes()
## patient_df   – original PATIENT table (must contain RecordId,
##                PatientId, DateOfHospitalAdmission, DateOfHospitalDischarge)
## --------------------------------------------------------------------
classifyEpisodeOrigin <- function(epi_core,
                                  patient_df) {
  
  ## ── 1 · Admission & discharge dates per admission ────────────────
  # adm_tbl <- patient_df %>%
  #   select(RecordId,
  #          PatientId,
  #          AdmissionDate = DateOfHospitalAdmission,
  #          DischargeDate = DateOfHospitalDischarge) %>%
  #   distinct()
  
  # adm_tbl <- patient_df %>% 
  #   ## collapse to ONE row per admission id
  #   group_by(PatientId) %>% 
  #   summarise(
  #     RecordId     = first(RecordId),
  #     PatientId     = first(PatientId),
  #     AdmissionDate = min(DateOfHospitalAdmission,  na.rm = TRUE),
  #     ## take the latest non-NA discharge; if all NA → keep NA
  #     DischargeDate = { d <- DateOfHospitalDischarge[!is.na(DateOfHospitalDischarge)]
  #     if (length(d) == 0) NA_real_ else max(d) },
  #     .groups = "drop"
  #   ) %>%
  #   ungroup()
  
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
      EpisodeOrigin = if_else(EpisodeClass == "CA", "Community", "Healthcare")
    )
  
  ## ── 4 · Return the enriched table ────────────────────────────────
  epi_full<- epi_full %>%
    select(-PrevDischarge, -DaysSinceAdmission, -DaysAfterPrevDisch) %>%
    relocate(EpisodeClass, EpisodeOrigin, .after = EpisodeStartDate) %>%
    distinct()
}

