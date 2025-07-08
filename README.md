# EHR-BSI

An R package for processing Electronic Health Record Bloodstream Infection (EHR-BSI) data, specifically for data transformation and episode calculation.

## Overview

This package processes raw BSI surveillance data into standardised EHR-BSI format tables and performs episode calculations according to ECDC surveillance definitions. The package has been harmonized to handle multiple countries (currently Estonia and Malta) through a unified workflow.

## Quick Start

The main workflow is demonstrated in `main.R`:

```r

# Ensure epiuf installed
#devtools::install_github("Epiconcept-Paris/STRAP-epiuf")

# Load package
devtools::load_all()

# Load commensals reference data
commensal_df = read.csv("reference/CommonCommensals.csv")

# Process country data with integrated episode calculation
result <- process_country_bsi(
  country = "MT",  # or "EE" for Estonia
  input_file = "BSI_REPORT_Malta.csv",
  input_file_path = "path/to/your/data/",
  dictionary_path = "reference/dictionary_raw_BSI_Malta.xlsx",
  metadata_path = "reference/MetaDataSet_57 (2025-03-13).xlsx",
  reporting_year = as.numeric(format(Sys.Date(), "%Y")),
  episode_duration = 14,
  write_to_file = TRUE,
  return_format = "list",
  calculate_episodes = TRUE
)
```

## Main Functions

### `process_country_bsi()` 
**Source:** `R/genericRecodeOrchestrator.R`

The primary harmonized function for transforming raw BSI data from multiple countries into EHR-BSI format with integrated episode calculation.

**Parameters:**
- `country`: Country code ("MT" for Malta, "EE" for Estonia)
- `input_file`: Name of the input file (defaults: "BSI_REPORT_Malta.csv" for MT, "BSI_REPORT_2024_share.xlsx" for EE)
- `input_file_path`: Path to the input file
- `dictionary_path`: Path to the data dictionary Excel file
- `metadata_path`: Path to the metadata Excel file for antibiotic recoding
- `reporting_year`: Year for DateUsedForStatistics field (default: current year)
- `episode_duration`: Duration for episode calculation in days (default: 14)
- `write_to_file`: Whether to write output Excel file to disk (default: FALSE)
- `write_to_file_path`: Path for output files (default: working directory)
- `return_format`: Return format - "list" (default)
- `calculate_episodes`: Whether to perform episode calculation (default: TRUE)

**Returns:** A list containing four EHR-BSI data tables: `ehrbsi`, `patient`, `isolate`, and `res`

**Key Features:**
- **Country-agnostic:** Automatically handles country-specific data formats and processing rules
- **Integrated episode calculation:** Episodes are calculated and aggregated back into the ehrbsi table
- **Flexible input:** Supports both CSV (Malta) and Excel (Estonia) input formats
- **Automatic output:** Generates Excel workbook with separate worksheets for each table

### Episode Calculation Functions

#### `calculateEpisodes()`
**Source:** `R/calculateEpisodes.R`

Identifies BSI cases and groups them into distinct episodes using ECDC surveillance definitions.

**Parameters:**
- `patient_df`: Patient data table
- `isolate_df`: Isolate data table  
- `commensal_df`: Table of commensal organisms
- `episodeDuration`: Duration in days for episode calculation (default: 14)

**BSI Case Definition Logic:**
- **Rule 1:** Recognized pathogen (RP) isolated in a blood culture
- **Rule 2:** ≥2 concordant common commensal (CC) isolates within 3 calendar days in two separate samples

**Episode Grouping Logic:**
- Same organism within episode duration = same episode
- Different organism within 2 days = same episode (polymicrobial)
- Otherwise = new episode

**Episode Origin Classification:**
- **HO-HA (Hospital-Onset Healthcare-Associated):** Onset ≥2 days after admission
- **IMP-HA (Imported Healthcare-Associated):** Onset ≤2 days after previous discharge  
- **CA (Community-Acquired):** All other cases

**Returns:** Data frame with episode IDs, start dates, origin classifications, and polymicrobial flags

#### `aggregateEpisodes()`
**Source:** `R/calculateEpisodes.R`

Aggregates episode counts back to the ehrbsi summary table.

**Parameters:**
- `eps_df`: Output from `calculateEpisodes()`
- `ehrbsi`: EHR-BSI summary table

**Returns:** Updated ehrbsi table with episode counts by origin (NumberOfCABSIs, NumberOfHOHABSIs, NumberOfImportedHABSIs, NumberOfTotalBSIs)

## Workflow Overview

1. **Data Loading:** Raw surveillance data is loaded (CSV for Malta, Excel for Estonia)
2. **Dictionary Application:** Data dictionaries are applied for standardization using `epiuf` package
3. **Country-Specific Processing:** Internal helper functions handle country-specific data transformations
4. **Table Creation:** Four EHR-BSI tables are generated (ehrbsi, patient, isolate, res)
5. **Episode Calculation:** BSI episodes are identified and classified using ECDC definitions
6. **Aggregation:** Episode counts are aggregated back to the ehrbsi summary table
7. **Output:** Results are returned as a list and optionally saved as Excel workbook

## Helper Functions

### Episode Calculation Helpers
**Source:** `R/episodeHelpers.R`

- `assign_episodes()`: Assigns episode numbers within a single patient
- `flag_cc_clusters()`: Identifies clusters of common commensal isolates for Rule 2 BSI cases

### Data Processing Helpers
- `ID_Create()`: Creates relational identifiers between tables
- `UNK_O_Handler()`: Handles unknown/other organism codes
- Various country-specific cleaning and transformation functions

## Reference Files

- `reference/CommonCommensals.csv`: List of commensal organisms for BSI case definition
- `reference/Lookup_Tables.r`: Value mapping tables for data recoding
- `reference/dictionary_raw_BSI_Estonia.xlsx`: Data dictionary for Estonia
- `reference/dictionary_raw_BSI_Malta.xlsx`: Data dictionary for Malta  
- `reference/MetaDataSet_57 (2025-03-13).xlsx`: Metadata for antibiotic resistance recoding

## Output Tables

The package generates four standardized EHR-BSI tables:
- **EHRBSI:** Summary statistics and metadata including episode counts by origin
- **Patient:** Patient demographics and admission details  
- **Isolate:** Microbiological isolate information
- **Res:** Antimicrobial resistance test results

When `write_to_file = TRUE`, these tables are saved as an Excel workbook with separate worksheets for each table (filename: `{country}_EHRBSI_FullReportingTemplate.xlsx`).

## Requirements

- R packages: `dplyr`, `readxl`, `stringr`, `tidyr`, `epiuf`, `openxlsx`
- For Malta processing: additional `tidyverse` dependency