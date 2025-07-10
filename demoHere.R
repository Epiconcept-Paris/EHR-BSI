

# Load package for testing
devtools::load_all()



### Run the shiny app (all functionality in one place)
# 1. Upload your data by clicking 'Raw', select country name, choose raw dataset from local files
# 2. Click 'Process'
# 3. Explore ehrbsi aggregate dataset in visuals (in development)
visual_bsi_dashboard()




### Or: Run transformation & episode calculation scripts only -----
###     this will also output an xlsx reporting template for ------
###     your data after converting to reporting template format ---


# MALTA --

# Load Malta data
malta_data <- read.csv("path_to_your_data")

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

# ESTONIA --

# Load Estonia data
estonia_data <- readxl::read_xlsx("path_to_your_data")

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

