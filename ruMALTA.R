devtools::load_all()  # This loads your entire package properly

# Make the raw reporting template tables (pre-episode calc)
result <- process_malta_bsi(input_file = "BSI_REPORT_Malta.csv",
                           input_file_path = "C:/Users/j.humphreys/Documents/Development/epi_ehr_bsi/Malta/data/raw/",
                           dictionary_path = "reference/dictionary_raw_BSI_Malta.xlsx",
                           value_maps_path = "reference/Lookup_Tables.r",
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

bsi_df <- bsi_df %>% mutate(BSIisolates = paste0(AdmissionRecordId,OnsetDate,MicroorganismCode)) %>%
  distinct()

# In the case of Malta there are 1095 patient ids, with 1178 unique admissions
# And given they have already checked for BSIs according to case def, we should expect 1179
cat("There are", length(unique(bsi_df$PatientId)), "patients")
cat("with", length(unique(bsi_df$AdmissionRecordId)), "admissions")
cat("and", length(unique(bsi_df$BSIisolates)), "isolates with unique onset dates or pathogens")


# Define each of those bsi cases as a distinct episode or a continuation of an earlier one
eps_df <- defineEpisodes(bsi_df, episodeDuration = 14)


# There are about 40 CC episodes according to Maltese excel, we will be missing these
cat("There were", length(unique(eps_df$EpisodeId)), "total BSIs (distinct episodes)")


# Missing patient IDs need to be dealt with
# Classify each episode according to whether community or hospital acquired
orig_df <- classifyEpisodeOrigin(eps_df, patient)




#### CHECK NUMS -------------------

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


#### CHECK NUMS -------------------






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
  rename(NumberOfCABSIs = CA
         ,NumberOfHOHABSIs = `HO-HA`
         ,NumberOfImportedHABSIs=`IMP-HA`) %>%
  mutate(NumberOfTotalBSIs = NA)



# Join back to ehrbsi table
ehrbsi <- result$ehrbsi


# Adding ParentId back to orig_df
ehrbsi <- ehrbsi %>%
  select(-NumberOfTotalBSIs,-NumberOfHOHABSIs,-NumberOfImportedHABSIs) %>%
  left_join(aggregateResults, by = c("RecordId"="ParentId")) %>%
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


# Overwrite default/empty table with final aggregate results
saveRDS(ehrbsi, "1.EHRBSI_Malta.rds")




library(openxlsx)

### For demonstration
# Read the RDS files
ehrbsi   <- readRDS("1.EHRBSI_Malta.rds")
patient  <- readRDS("2.EHRBSI$Patient.rds")
isolate  <- readRDS("3.EHRBSI$Patient$Isolate.rds")
res      <- readRDS("4.EHRBSI$Patient$Isolate$Res.rds")

# Create a new workbook
wb <- createWorkbook()

# Add each data frame as a new worksheet
addWorksheet(wb, "EHRBSI")
writeData(wb, sheet = "EHRBSI", ehrbsi)

addWorksheet(wb, "Patient")
writeData(wb, sheet = "Patient", patient)

addWorksheet(wb, "Isolate")
writeData(wb, sheet = "Isolate", isolate)

addWorksheet(wb, "Res")
writeData(wb, sheet = "Res", res)

# Save the workbook to the working directory
saveWorkbook(wb, "EHRBSI_Malta_data.xlsx", overwrite = TRUE) 