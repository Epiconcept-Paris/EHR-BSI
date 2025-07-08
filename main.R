

# Reload package for testing
devtools::load_all()



# Make the raw reporting template tables for MALTA (inc episode calc)
result <- process_country_bsi(
  country = "MT",
  input_file = "BSI_REPORT_Malta.csv",
  input_file_path = "C:/Users/j.humphreys/Documents/Development/epi_ehr_bsi/Malta/data/raw/",
  dictionary_path = "reference/dictionary_raw_BSI_Malta.xlsx",
  metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx",
  commensal_path = "reference/CommonCommensals.csv",
  reporting_year = as.numeric(format(Sys.Date(), "%Y")),
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)



# Make the raw reporting template tables for ESTONIA (inc episode calc)
result <- process_country_bsi(
  country = "EE",
  input_file = "BSI_REPORT_2024_share.xlsx",
  input_file_path = "C:/Users/j.humphreys/Documents/Development/epi_ehr_bsi/Estonia/data/raw/",
  dictionary_path = "reference/dictionary_raw_BSI_Estonia.xlsx",
  metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx", # or leave to default (same path)
  commensal_path = "reference/CommonCommensals.csv", # or leave to default (same path)
  reporting_year = as.numeric(format(Sys.Date(), "%Y")),
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)




