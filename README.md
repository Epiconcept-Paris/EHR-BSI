# EHR-BSI

An R package for processing Electronic Health Record Bloodstream Infection (EHR-BSI) data, specifically for data transformation and episode calculation.

## Overview

This package processes raw BSI surveillance data into standardised EHR-BSI format tables and performs episode calculations according to ECDC surveillance definitions.

## Test Main Functions Using `ruNEE.R`

### Data Processing Functions

#### `process_estonia_bsi()`
**Source:** `R/Estonia_Processor.R`

The primary function for transforming raw Estonian BSI data into EHR-BSI format.

**Parameters:**
- `input_file`: Name of the input Excel file (default: "BSI_REPORT_2024_share.xlsx")
- `input_file_path`: Path to the input file 
- `dictionary_path`: Path to the data dictionary Excel file
- `value_maps_path`: Path to the value maps R script (default: "reference/Lookup_Tables.r")
- `metadata_path`: Path to the metadata Excel file for antibiotic recoding
- `reporting_year`: Year for DateUsedForStatistics field (default: current year)
- `episode_duration`: Duration for episode calculation in days (default: 14)
- `write_to_file`: Whether to write output files to disk (default: FALSE)
- `write_to_file_path`: Path for output files
- `return_format`: Whether to return "list" (default) or "separate" objects

**Returns:** A list containing four EHR-BSI data tables: `ehrbsi`, `patient`, `isolate`, and `res`

### Episode Calculation Functions

#### `createBSIdf()`
**Source:** `R/calculateEpisodes.R`

Classifies hospital admissions as BSI cases based on ECDC case definitions.

**Parameters:**
- `patient_df`: Patient data table
- `isolate_df`: Isolate data table  
- `commensal_df`: Table of commensal organisms

**Logic:**
- **Rule 1:** Recognized pathogen (RP) isolated in a blood culture
- **Rule 2:** ≥2 concordant common commensal (CC) isolates within 3 calendar days in two separate samples

**Returns:** Data frame with BSI case flags and onset dates

#### `defineEpisodes()`
**Source:** `R/calculateEpisodes.R`

Groups BSI cases into distinct episodes based on ECDC temporal and microbiological criteria.

**Parameters:**
- `bsi_df`: Output from `createBSIdf()`
- `episodeDuration`: Duration in days for episode calculation (default: 14)

**Logic:**
- Same organism within episode duration = same episode
- Different organism within 2 days = same episode, polymicrobial
- Otherwise = new episode

**Returns:** Data frame with episode IDs, start dates, and polymicrobial flags

#### `classifyEpisodeOrigin()`
**Source:** `R/calculateEpisodes.R`

Classifies each BSI episode as community-acquired (CA) or healthcare-associated (HA).

**Parameters:**
- `episodes_df`: Output from `defineEpisodes()`
- `patient_df`: Patient data table with admission/discharge dates

**Classification Logic:**
- **HO-HA (Hospital-Onset Healthcare-Associated):** Onset ≥2 days after admission
- **IMP-HA (Imported Healthcare-Associated):** Onset ≤2 days after previous discharge
- **CA (Community-Acquired):** All other cases

**Returns:** Data frame with episode origin classifications


## Workflow in `ruNEE.R`

1. **Data Processing:** `process_estonia_bsi()` transforms raw data into EHR-BSI format
2. **BSI Case Definition:** `createBSIdf()` identifies BSI cases using ECDC criteria
3. **Episode Grouping:** `defineEpisodes()` groups cases into distinct episodes  
4. **Origin Classification:** `classifyEpisodeOrigin()` determines episode acquisition source
NEED TO BE IMPLEMENTED AS FUNCTIONS:
5. **Aggregation:** Results are aggregated and merged back into final EHR-BSI tables
6. **Output:** Final tables are saved as RDS files and Excel workbook

## Key Reference Files

- `reference/CommonCommensals.csv`: List of commensal organisms for BSI case definition
- `reference/Lookup_Tables.r`: Value mapping tables for data recoding
- `reference/dictionary_raw_BSI_Estonia.xlsx`: Data dictionary for Estonia
- `reference/MetaDataSet_57 (2025-03-13).xlsx`: Metadata for BSI

## Output Tables

The package generates four standardized EHR-BSI tables:
- **EHRBSI:** Summary statistics and metadata
- **Patient:** Patient demographics and admission details  
- **Isolate:** Microbiological isolate information
- **Res:** Antimicrobial resistance test results
- **Denoms:** Not currently implemented - no country with necessary data