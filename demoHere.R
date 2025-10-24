
devtools::document()
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
devtools::document()
# Load package for testing
devtools::load_all()


# Load Malta data
malta_data <- read.csv("file_path_here")

# Make the raw reporting template tables for MALTA (inc episode calc)
result <- process_country_bsi(
  country = "MT",
  input_data = malta_data,
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  aggregation_level = "LAB",
  calculate_episodes = TRUE
)

# ESTONIA --

devtools::document()
# Load package for testing
devtools::load_all()

# Load EE data
estonia_data <- read_xlsx("file_path_here")


# Make the raw reporting template tables for ESTONIA (inc episode calc)
result <- process_country_bsi(
  country = "EE",
  input_data = estonia_data,
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)





# CZECH REPUBLIC --

# Load CZ data
czech_data <- read_xlsx("file_path_here")

# Load package for testing
devtools::document()
devtools::load_all()

# Make the raw reporting template tables for CZECH (inc episode calc)
result <- process_country_bsi(
  country = "CZ",
  input_data = czech_data,
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)



