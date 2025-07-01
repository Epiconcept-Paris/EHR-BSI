library(dplyr)
library(lubridate)

createBSIdf <- function(patient_df,
                        isolate_df,
                        commensal_df){
  
  ## ------------------------------------------------------------------
  ## 1.  Flag each isolate as recognised pathogen (RP) or
  ##     common commensal / contaminant (CC) -- ANY THAT ISN'T CC is RP  # TEMPORARY TEMPORARY - Find RP list
  ## ------------------------------------------------------------------
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
    group_by(AdmissionRecordId) %>%
    summarise(onset_date_pathogen = min(DateOfSpecCollection),
              .groups = "drop")
  
  ## ------------------------------------------------------------------
  ## 3B.  Rule-2 admissions  ─ ≥2 concordant CC within 3 calendar days
  ## ------------------------------------------------------------------
  rule2 <- iso_in_admission %>%
    filter(org_type == "CC") %>%
    group_by(AdmissionRecordId, MicroorganismCode) %>%
    mutate(first_date = min(DateOfSpecCollection)) %>%
    summarise(n_in_window = sum(DateOfSpecCollection <= first_date + days(2)),
              first_date = first(first_date),
              .groups = "drop") %>%
    filter(n_in_window >= 2) %>%               # ≥ 2 cultures → candidate BSI
    group_by(AdmissionRecordId) %>%
    summarise(onset_date_cc = min(first_date), # earliest qualifying CC date
              .groups = "drop")
  
  ## ------------------------------------------------------------------
  ## 4.  Combine the two rules to create overall BSI flag + onset
  ## ------------------------------------------------------------------
  bsi_combined <- full_join(rule1, rule2, by = "AdmissionRecordId") %>%
    mutate(
      OnsetDate = pmin(onset_date_pathogen, onset_date_cc, na.rm = TRUE),
      BSI_case   = TRUE
    ) %>%
    select(AdmissionRecordId, BSI_case, OnsetDate)
  
  ## ------------------------------------------------------------------
  ## 5.  Add the results back to the patient/admission table
  ## ------------------------------------------------------------------
  patient_df %>%
    left_join(bsi_combined, by = c("RecordId" = "AdmissionRecordId")) %>%
    mutate(
      BSI_case   = if_else(is.na(BSI_case), FALSE, BSI_case),
      OnsetDate = as.Date(OnsetDate)          # drop time component
    ) %>% 
    select(RecordId, PatientId, BSI_case, OnsetDate) %>%
    distinct()
}





defineEpisodes <- function(bsi_df, episodeDuration = 14) {
  ## bsi_df is the object returned by createBSIdf()  ─ it must contain
  ##   • RecordId   (admission-level record id)
  ##   • PatientId
  ##   • BSI_case   (TRUE / FALSE)
  ##   • OnsetDate  (Date)
  
  ## 1 ─ Keep only bona-fide BSI onsets and sort them chronologically
  epi_core <- bsi_df %>%
    filter(BSI_case, !is.na(OnsetDate)) %>%       # just the true BSIs
    arrange(PatientId, OnsetDate) %>%
    
    ## 2 ─ For each patient, flag where a *new* episode starts
    group_by(PatientId) %>%
    mutate(
      ## first record is always a new episode;
      ## any later onset that is ≥ episodeDuration days after the previous
      ## also starts a new one
      new_episode = if_else(
        row_number() == 1L |
          OnsetDate > lag(OnsetDate) + days(episodeDuration - 1),
        1L, 0L
      ),
      EpisodeNumber = cumsum(new_episode)         # 1, 2, 3 …
    ) %>%
    ungroup()
  
  ## 3 ─ Give each episode a start date and an id that is unique per patient
  epi_core <- epi_core %>%
    group_by(PatientId, EpisodeNumber) %>%
    mutate(
      EpisodeStartDate = min(OnsetDate),
      EpisodeId = paste0(PatientId, "_E", sprintf("%03d", EpisodeNumber))
    ) %>%
    ungroup() %>%
    select(RecordId, EpisodeId, EpisodeNumber, EpisodeStartDate)
  
  ## 4 ─ Bring the episode information back to the full BSI table
  bsi_df %>%
    left_join(epi_core, by = "RecordId") %>%
    arrange(PatientId, EpisodeNumber, OnsetDate)
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
      PatientId     = first(PatientId),                       # same within id
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
    left_join(adm_tbl, by = "RecordId") %>%
    ## use EpisodeStartDate if it exists, otherwise OnsetDate
    mutate(Onset = coalesce(EpisodeStartDate, OnsetDate)) %>%
    ## day-of-stay is counted with admission = day 1
    mutate(
      DaysSinceAdmission = as.numeric(Onset - AdmissionDate, units = "days"),
      DaysAfterPrevDisch = as.numeric(Onset - PrevDischarge,  units = "days")
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
    relocate(EpisodeClass, EpisodeOrigin, .after = OnsetDate)
}


# episodeDuration  … length of an episode in days   (defaults to 14)
# polyWindow       … time span in which “extra” species
#                    still belong to the *same* episode (defaults to 3)
definePolyEpisodes <- function(bsi_df,
                               isolate_df,
                               commensal_df,
                               episodeDuration = 14,
                               polyWindow      = 3) {
  
  ## ────────────────────────────────────────────────────────────────
  ##  0 · Helper — select only the isolates that really count
  ##      for the BSI definition   (same logic used in defineBSI)
  ## ────────────────────────────────────────────────────────────────
  comm_codes <- unique(commensal_df$MicroorganismCode)
  
  isolates_flagged <- isolate_df |>
    mutate(org_type = if_else(MicroorganismCode %in% comm_codes,
                              "CC", "RP"))
  
  ## recognised-pathogen isolates are always in
  iso_rule1 <- isolates_flagged |>
    filter(org_type == "RP")
  
  ##  ≥2 concordant CC within 3 days   →  include *all* isolates of
  ##  those species for that patient
  rule2_species <- isolates_flagged |>
    filter(org_type == "CC") |>
    arrange(PatientId, MicroorganismCode, DateOfSpecCollection) |>
    group_by(PatientId, MicroorganismCode) |>
    mutate(first_date = first(DateOfSpecCollection)) |>
    summarise(
      n_in_window = sum(DateOfSpecCollection <= first_date + days(2)),
      .groups = "drop"
    ) |>
    filter(n_in_window >= 2)
  
  iso_rule2 <- inner_join(
    isolates_flagged |> filter(org_type == "CC"),
    rule2_species,
    by = c("PatientId", "MicroorganismCode")
  )
  
  ##  All “qualifying” BSI isolates in one table
  iso_bsi <- bind_rows(iso_rule1, iso_rule2) |>
    select(PatientId, MicroorganismCode,
           DateOfSpecCollection, RecordId) |>
    arrange(PatientId, DateOfSpecCollection)
  
  
  ## ────────────────────────────────────────────────────────────────
  ##  1 · Build polymicrobial episodes patient-by-patient
  ##      (small loop inside each patient – fast + clear)
  ## ────────────────────────────────────────────────────────────────
  poly_epi_tbl <- iso_bsi |>
    group_by(PatientId) |>
    group_modify(\(df, key) {
      
      df <- arrange(df, DateOfSpecCollection)
      n  <- nrow(df)
      
      ep_num      <- integer(n)          # will hold episode numbers
      current_ep  <- 1L
      start_date  <- df$DateOfSpecCollection[1]
      species_set <- df$MicroorganismCode[1]
      
      ep_num[1] <- current_ep
      
      for (i in 2:n) {
        d  <- df$DateOfSpecCollection[i]
        sp <- df$MicroorganismCode[i]
        
        ##  Rule-1 : classic 14-day boundary
        new_ep <- d > start_date + days(episodeDuration - 1)
        
        ##  Rule-2 : new species appearing ≥ polyWindow days
        ##           after the episode start
        new_sp_late <- !(sp %in% species_set) &&
          d >= start_date + days(polyWindow)
        
        if (new_ep || new_sp_late) {
          current_ep  <- current_ep + 1L
          start_date  <- d
          species_set <- sp               # reset
        } else {
          species_set <- union(species_set, sp)
        }
        
        ep_num[i] <- current_ep
      }
      
      df$EpisodeNumber <- ep_num
      df
    }) |>
    ungroup()
  
  
  ## ────────────────────────────────────────────────────────────────
  ##  2 · Episode-level metadata
  ## ────────────────────────────────────────────────────────────────
  episode_meta <- poly_epi_tbl |>
    group_by(PatientId, EpisodeNumber) |>
    summarise(
      EpisodeStartDate = min(DateOfSpecCollection),
      EpisodeEndDate   = max(DateOfSpecCollection),      # for convenience
      Polymicrobial    = n_distinct(MicroorganismCode) > 1,
      .groups = "drop"
    ) |>
    arrange(PatientId, EpisodeStartDate) |>
    group_by(PatientId) |>
    mutate(
      EpisodeId = paste0(PatientId,
                         "_PE",
                         sprintf("%03d", row_number()))
    ) |>
    mutate(EpisodeEndDate = EpisodeStartDate + days(episodeDuration - 1)) |>
    ungroup()
  
  
  ## ────────────────────────────────────────────────────────────────
  ##  3 · Attach the episode information back to the BSI table
  ##      (each onset must fall inside the [start, end] window)
  ## ────────────────────────────────────────────────────────────────
  bsi_df |>
    ## Cartesian, then thin down to the matching episode
    inner_join(episode_meta, by = "PatientId") |>
    filter(OnsetDate >= EpisodeStartDate,
           OnsetDate <= EpisodeEndDate) |>
    group_by(RecordId) |>
    slice_min(EpisodeStartDate, n = 1) |>
    ungroup() |>
    ## Bring everything back to the original BSI table
    right_join(bsi_df, by = names(bsi_df)) |>
    relocate(EpisodeId, EpisodeNumber,
             EpisodeStartDate, Polymicrobial,
             .after = OnsetDate)
}