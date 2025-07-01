


# Make the raw reporting template tables (pre-episode calc)
result <- process_estonia_bsi(input_file = "BSI_REPORT_2024_share.xlsx",
                                input_file_path = "C:/Users/j.humphreys/Documents/Development/epi_ehr_bsi/Estonia/data/raw/",
                                dictionary_path = "reference/dictionary_raw_BSI_Estonia.xlsx",
                                value_maps_path = "reference/Lookup_Tables.r",
                                metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx",
                                reporting_year = as.numeric(format(Sys.Date(), "%Y")),
                                episode_duration = 14,
                                write_to_file = TRUE,
                                write_to_file_path = NULL,
                                return_format = "list")



# Grab the two tables which are required for episode calc and case origin classification
patient <- result$patient
isolate <- result$isolate


# Load the commensals table
commensal_df = read.csv("reference/CommonCommensals.csv")



# PRACTICING FUNCTIONS FOR EPISODES/ORIGINS -----------------------------


# Classify each admission as a BSI based on case definition (including CC two-isolate check)
bsi_df <- createBSIdf(patient, isolate, commensal_df)


cat("There were", length(unique(bsi_df$RecordId)), "total BSI admissions (not episodes)")


# Define each of those bsi cases as a distinct episode or a continuation of an earlier one
eps_df <- defineEpisodes(bsi_df, episodeDuration = 14)


cat("There were", length(unique(eps_df$EpisodeId)), "total BSIs (distinct episodes)")

# ------- from where?

# Classify each episode according to whether community or hospital acquired
orig_df <- classifyEpisodeOrigin(eps_df, patient)



# Basic df for calculating some stats
calc_df <- orig_df %>%
  select(EpisodeId, EpisodeOrigin) %>%
  filter(!is.na(EpisodeId)) %>%
  distinct()


# Print stats
cat("TOTAL BSI episodes: ", length(unique(orig_df$EpisodeId))," \n ", 
    "OF WHICH COMMUNITY-ACQUIRED: ", sum(calc_df$EpisodeOrigin=="Community"), "(",
    round(((sum(calc_df$EpisodeOrigin=="Community")/length(unique(orig_df$EpisodeId)))*100),1),"%)"," \n ", 
    "VS HOSP-ACQUIRED: ", sum(calc_df$EpisodeOrigin=="Healthcare"), "(",
    round(((sum(calc_df$EpisodeOrigin=="Healthcare")/length(unique(orig_df$EpisodeId)))*100),1),"%)")


# Aggregate to ehrbsi level
aggregateResults <- orig_df %>%
  distinct() %>%
  group_by(ParentId, EpisodeClass) %>%
  mutate(countEps = n()) %>%
  select(ParentId, EpisodeClass, countEps) %>%
  distinct() %>%
  pivot_wider(names_from = EpisodeClass,
              values_from = countEps,
              id_cols = ParentId) %>%
  rename(NumCommunity_noncdm = CA
         ,NumberOfHOHABSIs = `HO-HA`
         ,NumberOfImportedHABSIs=`IMP-HA`) %>%
  mutate(NumberOfTotalBSIs = NumCommunity_noncdm + NumberOfHOHABSIs + NumberOfImportedHABSIs) %>%
  select(-NumCommunity_noncdm)



# Join back to ehrbsi table
ehrbsi <- result$ehrbsi


# Adding ParentId back to orig_df
ehrbsi <- ehrbsi %>%
  select(-NumberOfTotalBSIs,-NumberOfHOHABSIs,-NumberOfImportedHABSIs) %>%
  left_join(aggregateResults, by = c("RecordId"="ParentId"))


# Overwrite default/empty table with final aggregate results
saveRDS(ehrbsi, "1.EHRBSI.rds")


