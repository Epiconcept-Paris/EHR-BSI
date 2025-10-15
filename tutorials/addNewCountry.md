# How to Add a New Country to the EHR-BSI Package

This document outlines the key steps required to extend the `EHR-BSI` package to support a new country, using the existing implementations for Malta (MT) and Estonia (EE) as a reference.

Let's assume you are adding a new country with the two-letter code "XX".

## Summary of Steps

1.  **Prepare a Data Dictionary**: Create a country-specific dictionary to map raw data to the package's standardized format.
2.  **Add Lookup Tables**: If necessary, define lookup tables for country-specific codes and values.
3.  **Create the Recoding Script**: Develop a new R script containing functions to clean the data and generate the four standard EHR-BSI tables (`Patient`, `Isolate`, `Res`, `EHRBSI`).
4.  **Update the Main Orchestrator**: Modify the central `process_country_bsi` function to recognize the new country and execute its processing script.
5.  **Test the Implementation**: Add a new section to `main.R` to run the pipeline for the new country.

---

## Step 1: Prepare Country-Specific Excel File

The package uses country-specific Excel files to manage both column renaming (dictionary) and value mappings (lookups).

### Create the Excel File

-   Create a new Excel file named `XX.xlsx` and place it in the `reference/dictionaries/` directory.
-   The file must contain two tabs:

#### Tab 1: Dictionary

This tab handles column renaming from raw data to standardized EHR-BSI format.

**Structure:**
- Column A: `raw_column_name` - The original column name in your country's data
- Column B: `standard_column_name` - The standardized EHR-BSI column name

**Example:**
| raw_column_name | standard_column_name |
|-----------------|---------------------|
| PatientCounter  | PatientId           |
| Gender          | Sex                 |
| Pathogen        | MicroorganismCode   |

#### Tab 2: Lookups

This tab contains all value mapping tables in a long format (stacked).

**Structure:**
- Column A: `lookup_name` - Name of the lookup table (e.g., "XX_HospType")
- Column B: `from_value` - The original value in your data
- Column C: `to_value` - The standardized value

**Example:**
| lookup_name | from_value | to_value |
|-------------|------------|----------|
| XX_HospType | HOSP_A     | TERT     |
| XX_HospType | HOSP_B     | SEC      |
| XX_Outcome  | DEAD       | D        |
| XX_Outcome  | ALIVE      | A        |

### Shared Lookups

Include the standard Name_Lookup and Specialty_Lookup entries in your country's Lookups tab. These are shared across all countries but must be included in each country file.

## Step 2: Update Country Configuration

Add your country's configuration to `R/countryConfigs.R` in the `COUNTRY_CONFIGS` list:

```r
XX = list(
  # Date format specifications
  date_format = "%d/%m/%Y",
  has_time = FALSE,
  date_columns = c("DateOfSpecCollection", "DateOfHospitalAdmission"),
  
  # Record ID templates
  record_ids = list(
    bsi = "{HospitalId}-{year}",
    patient = "{PatientId}-{admit_date}",
    isolate = "{IsolateId}_{MicroorganismCode}"
  ),
  
  # Antibiotic data format
  antibiotic = list(
    format = "wide",  # or "long" for Estonia-style
    prefix = "ab_"    # for wide format
  ),
  
  # Terminology systems
  terminology = list(
    clinical = "ICD-10",
    clinical_spec = NA_character_,
    microbiological = "SNOMED-CT",
    microbiological_spec = NA_character_
  ),
  
  # Lookup tables to load (matches lookup_name in Excel)
  lookups = c("HospType", "PathogenCode", "Outcome"),
  
  # Lookup mappings configuration
  lookup_mappings = list(
    HospType = list(
      column = "HospitalId",
      from = "xx_hosptype",
      to = "hosptype_code",
      output_column = "HospitalType"
    )
  ),
  
  # Field transformations (R functions)
  field_transforms = list(),
  
  # Table-specific defaults
  defaults = list(
    patient = list(),
    isolate = list(),
    ehrbsi = list()
  ),
  
  # Columns to remove after processing
  noncdm_cleanup = c(),
  
  # Special field handling
  special_fields = list()
)
```

## Step 3: Create the Country-Specific Recoding Script

Create a new file `R/recodingXX.R` with internal helper functions. The package uses a unified approach, so most functionality is already available in shared functions.

**Template:**

```r
#' Process XX BSI data from raw format to EHR-BSI format
#' @param raw_data Received from genericRecodeOrchestrator.R
#' @return Returns a list containing the four EHR-BSI data tables
#' @export

.process_xx_basic_cleaning <- function(raw_data) {
  config <- get_country_config("XX")
  recoded_data <- process_basic_cleaning(raw_data, config, "XX")
  return(recoded_data)
}

.create_xx_patient_table <- function(recoded_data) {
  config <- get_country_config("XX")
  patient <- create_standard_patient_table(recoded_data, config = config)
  patient <- finalize_table(patient, get_standard_table_columns("patient"))
  return(patient)
}

.create_xx_isolate_table <- function(recoded_data) {
  config <- get_country_config("XX")
  isolate <- create_standard_isolate_table(recoded_data, config = config)
  isolate <- finalize_table(isolate, get_standard_table_columns("isolate"))
  return(isolate)
}

.create_xx_res_table <- function(recoded_data, metadata_path = NULL) {
  config <- get_country_config("XX")
  res <- create_standard_res_table(recoded_data, config, "XX", metadata_path)
  return(res)
}

.create_xx_ehrbsi_table <- function(recoded_data, episode_duration) {
  config <- get_country_config("XX")
  ehrbsi <- create_base_ehrbsi_table(recoded_data, "XX", episode_duration, 
                                     record_id_col = "record_id_bsi", config = config)
  ehrbsi <- finalize_table(ehrbsi, get_standard_table_columns("ehrbsi"))
  return(ehrbsi)
}
```

Most of the heavy lifting is done by the shared functions in `R/recodingFuncs.R`:
- `process_basic_cleaning()` - Handles column renaming, lookup application, date parsing, and record ID creation
- `create_standard_patient_table()` - Creates standardized patient table
- `create_standard_isolate_table()` - Creates standardized isolate table
- `create_standard_res_table()` - Creates standardized resistance table (handles both wide and long formats)
- `create_base_ehrbsi_table()` - Creates standardized EHRBSI summary table

**Key Points:**
- Date parsing is configured in the country config (`date_format`, `has_time`)
- Record IDs are created from templates in the config (`record_ids`)
- Lookups are automatically applied based on `lookup_mappings` in the config
- Custom transformations can be added via `field_transforms` in the config

## Step 4: Update the Main Orchestrator

The main `process_country_bsi` function in `R/genericRecodeOrchestrator.R` must be updated to add your country processing block.

Add an `else if` block in the processing section:
```r
} else if (country == "XX") {
  recoded_data <- .process_xx_basic_cleaning(raw_data)
  
  # Create the four tables
  patient <- .create_xx_patient_table(recoded_data)
  isolate <- .create_xx_isolate_table(recoded_data)
  res <- .create_xx_res_table(recoded_data, metadata_path)
  ehrbsi <- .create_xx_ehrbsi_table(recoded_data, episode_duration)
}
```

The country code validation is now automatic through the config system - once you add your country to `COUNTRY_CONFIGS`, it will be recognized.

## Step 5: Test the Implementation

Test your new implementation:

```r
# Load package
devtools::load_all()

# Load your raw data
xx_data <- read.csv("path/to/your/data.csv")  # or readxl::read_xlsx() for Excel

# Process the data
result <- process_country_bsi(
  country = "XX",
  input_data = xx_data,
  dictionary_path = "reference/dictionaries/XX.xlsx",  # Optional, auto-detected
  episode_duration = 14,
  calculate_episodes = TRUE,
  write_to_file = TRUE,
  write_to_file_path = "output/"
)

# Check results
names(result)  # Should show: ehrbsi, patient, isolate, res
head(result$patient)
```

Verify:
- Column names are correctly standardized
- Lookups are applied correctly
- All four tables are generated
- Episode calculations complete successfully
- Output Excel file is created

## Summary

The new harmonized system centralizes all country-specific configuration in three places:
1. **Excel file** (`reference/dictionaries/XX.xlsx`) - Dictionary and lookup data
2. **Config object** (`R/countryConfigs.R`) - Processing rules and parameters
3. **Recoding script** (`R/recodingXX.R`) - Wrapper functions (mostly boilerplate)

This approach minimizes code duplication and makes it easier to maintain and extend the package. 
