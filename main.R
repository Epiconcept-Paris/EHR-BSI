

# Reload package for testing
devtools::load_all()



# Load Malta data
malta_data <- read.csv("path")

# Make the raw reporting template tables for MALTA (inc episode calc)
result <- process_country_bsi(
  country = "MT",
  input_data = malta_data,
  dictionary_path = "reference/dictionary_raw_BSI_Malta.xlsx",
  metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx",
  commensal_path = "reference/CommonCommensals.csv",
  reporting_year = as.numeric(format(Sys.Date(), "%Y")),
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)



# Load Estonia data
estonia_data <- readxl::read_xlsx("path")

# Make the raw reporting template tables for ESTONIA (inc episode calc)
result <- process_country_bsi(
  country = "EE",
  input_data = estonia_data,
  dictionary_path = "reference/dictionary_raw_BSI_Estonia.xlsx",
  metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx", # or leave to default (same path)
  commensal_path = "reference/CommonCommensals.csv", # or leave to default (same path)
  reporting_year = as.numeric(format(Sys.Date(), "%Y")),
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)



devtools::document()
devtools::load_all()

# Full dashboard (launches in browser/viewer)
#visual_bsi_dashboard(result)
visual_bsi_dashboard()

