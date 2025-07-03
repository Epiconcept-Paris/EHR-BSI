

# Reload package for testing
devtools::load_all()


# Load the commensals table
commensal_df = read.csv("reference/CommonCommensals.csv")



# Make the raw reporting template tables (pre-episode calc)
result <- process_country_bsi(
  country = "MT",
  input_file = "BSI_REPORT_Malta.csv",
  input_file_path = "C:/Users/j.humphreys/Documents/Development/epi_ehr_bsi/Malta/data/raw/",
  dictionary_path = "reference/dictionary_raw_BSI_Malta.xlsx",
  metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx",
  reporting_year = as.numeric(format(Sys.Date(), "%Y")),
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)



# Make the raw reporting template tables (pre-episode calc)
result <- process_country_bsi(
  country = "EE",
  input_file = "BSI_REPORT_2024_share.xlsx",
  input_file_path = "C:/Users/j.humphreys/Documents/Development/epi_ehr_bsi/Estonia/data/raw/",
  dictionary_path = "reference/dictionary_raw_BSI_Estonia.xlsx",
  metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx",
  reporting_year = as.numeric(format(Sys.Date(), "%Y")),
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)




