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

## Step 1: Prepare a Data Dictionary

The package uses the `epiuf` framework, which relies on an Excel-based data dictionary to standardize column names and variable values.

-   Create a new Excel file named `dictionary_raw_BSI_XX.xlsx` and place it in the `reference/` directory.
-   Follow the structure of the existing dictionaries (`dictionary_raw_BSI_Malta.xlsx`, `dictionary_raw_BSI_Estonia.xlsx`) to define mappings for your country's raw data file.

## Step 2: Add Lookup Tables (If Needed)

If your country's data uses local codes that need to be mapped to the package's standard values (e.g., for hospital wards, organism names, outcome codes), you must add lookup tables.

-   Open the `reference/Lookup_Tables.R` file.
-   Add new data frames for your country's lookups. For example:
    ```r
    XX_PathogenCode_Lookup <- data.frame(
      xx_pathogen_name = c("Staph. aureus", "E. coli"),
      microorganism_code = c("115329001", "118941005")
    )
    ```

## Step 3: Create the Country-Specific Recoding Script

This is the most significant part of the process. You need to create an R script with a set of functions that handle the unique structure and format of your country's data.

-   Create a new file in the `R/` directory named `recodingXX.R`.
-   Inside this file, you must define five internal functions. These functions will be called by the main orchestrator. Use the existing `R/recodingMalta.R` and `R/recodingEstonia.R` files as a guide.

#### 1. `.process_xx_basic_cleaning(raw_data, reporting_year)`
   - **Purpose**: Perform initial data cleaning, transformations, and ID creation.
   - **Key Tasks**:
     - Parse dates into a standard format. Note the different approaches in Malta (`parse_dates_with_fallback`) and Estonia (`as.POSIXct`).
     - Create the unique, hierarchical record IDs (`record_id_bsi`, `record_id_patient`, `record_id_isolate`).
     - Recode variables using the lookup tables you defined in Step 2.

#### 2. `.create_xx_patient_table(recoded_data)`
   - **Purpose**: Generate the standardized `Patient` table.
   - **Recommendation**: Use the shared `create_standard_patient_table()` function as a starting point, and then add or modify columns as required for your country's data. For an example of custom logic, see how `PreviousAdmission` is calculated in `.create_estonia_patient_table`.

#### 3. `.create_xx_isolate_table(recoded_data)`
   - **Purpose**: Generate the standardized `Isolate` table.
   - **Recommendation**: Use the shared `create_standard_isolate_table()` function as a base. You may need to add logic to map country-specific organism codes, as seen in `.create_malta_isolate_table`.

#### 4. `.create_xx_res_table(recoded_data, ...)`
   - **Purpose**: Generate the standardized `Res` (antimicrobial resistance) table.
   - **This is the most complex function and is highly dependent on the source data format.**
     - **Wide Format (like Malta)**: If your data has one column per antibiotic (e.g., `ab_PEN`, `ab_AMP`), you will need to pivot the data into a long format. See `.create_malta_res_table`.
     - **Long Format (like Estonia)**: If your data is already in a long format (e.g., columns for `sensitivityTest`, `sensitivityResult`), you will need to parse these records to extract the antibiotic name, test type (MIC, Zone, etc.), and result. This is a more complex process; review `.create_estonia_res_table` carefully. This may also require additional dependencies like the `metadata_path` to map antibiotic names to the HAI standard.

#### 5. `.create_xx_ehrbsi_table(recoded_data, reporting_year, episode_duration)`
   - **Purpose**: Generate the top-level `EHRBSI` summary table.
   - **Recommendation**: Use the shared `create_base_ehrbsi_table()` function and then add or modify columns with country-specific metadata (e.g., `HospitalType`, `ProportionPopulationCovered`).

## Step 4: Update the Main Orchestrator

The main `process_country_bsi` function in `R/genericRecodeOrchestrator.R` must be updated to recognize the new country.

1.  **Add to Country Validation**:
    -   Find the line `if (!country %in% c("MT", "EE"))` and add your new country code:
        ```r
        if (!country %in% c("MT", "EE", "XX"))
        ```

2.  **Add the Country Processing Block**:
    -   Add an `else if` block to the main processing section to call the five functions you created in `recodingXX.R`.
        ```r
        } else if (country == "XX") {
          recoded_data <- .process_xx_basic_cleaning(raw_data, reporting_year)
          
          # Create the four tables
          patient <- .create_xx_patient_table(recoded_data)
          isolate <- .create_xx_isolate_table(recoded_data)
          res <- .create_xx_res_table(recoded_data) # Add metadata_path if needed
          ehrbsi <- .create_xx_ehrbsi_table(recoded_data, reporting_year, episode_duration)
        }
        ```

## Step 5: Test the Implementation

Finally, test your new implementation by running it from the `main.R` script.

-   Make sure `devtools::load_all()` is called at the top of the script. This will make the new functions in `R/recodingXX.R` available to the package.
-   Add a new block to `main.R` to call `process_country_bsi` for your country:
    ```r
    # Process data for country XX
    result <- process_country_bsi(
      country = "XX",
      input_file = "your_raw_data_file.csv",
      input_file_path = "path/to/your/data/",
      # ... other parameters as needed ...
      write_to_file = TRUE
    )
    ```
-   Run the script and check the console for errors and verify the contents of the generated Excel file. 
