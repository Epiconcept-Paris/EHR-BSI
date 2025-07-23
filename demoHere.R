



### Or: Run transformation & episode calculation scripts only -----
###     this will also output an xlsx reporting template for ------
###     your data after converting to reporting template format ---


# MALTA --
devtools::document()
# Load package for testing
devtools::load_all()


# Load Malta data
malta_data <- read.csv("xxx Documents/Development/epi_ehr_bsi/Malta/data/raw/BSI_REPORT_Malta.csv")

# Make the raw reporting template tables for MALTA (inc episode calc)
result <- process_country_bsi(
  country = "MT",
  input_data = malta_data,
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)

# ESTONIA --

devtools::document()
# Load package for testing
devtools::load_all()

# Load EE data
estonia_data <- read_xlsx("xxx Documents/Development/epi_ehr_bsi/Estonia/data/raw/BSI_REPORT_2024_share.xlsx")


# Make the raw reporting template tables for ESTONIA (inc episode calc)
result <- process_country_bsi(
  country = "EE",
  input_data = estonia_data,
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)






###### R SHINY 

devtools::document()
# Load package for testing
devtools::load_all()



### Run the shiny app (all functionality in one place)
# 1. Upload your data by clicking 'Raw', select country name, choose raw dataset from local files
# 2. Click 'Process'
# 3. Explore ehrbsi aggregate dataset in visuals (in development)
visual_bsi_dashboard()


