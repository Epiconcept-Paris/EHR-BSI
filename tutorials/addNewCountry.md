# How to Add a New Country to the EHR-BSI Package

This document outlines the **simplified** process to extend the `EHR-BSI` package to support a new country. The system is now **config-driven**, meaning most countries can be added by only editing Excel files—no R coding required!

Let's assume you are adding a new country with the two-letter code "XX".

## Summary of Steps (Simplified!)

1.  **Create Country Excel File**: Copy the template and customize three tabs (Dictionary, Lookups, Config)
2.  **Test the Implementation**: Use `process_country_bsi()` - it automatically works with your new country!
3.  **(Optional) Add R Logic**: Only if you need complex transformations that can't be expressed in Excel

**That's it!** No need to modify orchestrator code, create R recoding scripts, or write boilerplate functions.

---

## Step 1: Create Country Excel File

The package uses country-specific Excel files to manage column renaming, value mappings, and processing configuration.

### Create the Excel File

-   Create a new Excel file named `XX.xlsx` and place it in the `reference/dictionaries/` directory.
-   The file must contain **three tabs**: Dictionary, Lookups, and Config

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

#### Tab 3: Config

This tab contains **all processing configuration** in a simple key-value format. This is the **major simplification** - no R coding needed!

**Structure:**
- Column A: `config_key` - The configuration parameter name
- Column B: `config_value` - The value for this parameter
- Column C: `config_type` - The data type (string, numeric, boolean, list)

**Example Config Tab:**
| config_key | config_value | config_type |
|------------|-------------|-------------|
| date_format | %d/%m/%Y | string |
| has_time | FALSE | boolean |
| date_columns | DateOfSpecCollection,DateOfHospitalAdmission | list |
| antibiotic_format | wide | string |
| antibiotic_prefix | ab_ | string |
| record_id_bsi | {HospitalId}-{year} | string |
| record_id_patient | {PatientId}-{admit_date} | string |
| terminology_clinical | ICD-10 | string |
| terminology_microbiological | SNOMED-CT | string |
| default_patient_LaboratoryCode | XX001 | string |
| default_ehrbsi_ESurvBSI | 2 | numeric |

**Complete examples:**
- See `reference/dictionaries/MT_Config_Template.csv` for Malta (wide antibiotic format)
- See `reference/dictionaries/EE_Config_Template.csv` for Estonia (long antibiotic format)
- See `reference/dictionaries/CONFIG_REFERENCE.md` for all possible configuration keys

**Key configuration sections:**
1. **Date handling**: Format, time inclusion, columns to parse
2. **Record IDs**: Templates for BSI, patient, and isolate IDs
3. **Antibiotic format**: Wide (Malta-style) or long (Estonia-style) with test types
4. **Terminology**: Clinical and microbiological coding systems
5. **Lookups**: Which lookup tables to load
6. **Lookup mappings**: How to apply lookups to columns
7. **Table defaults**: Default values for patient, isolate, and EHRBSI tables
8. **Cleanup**: Non-CDM columns to remove

## Step 2: Test Your Configuration

**That's it!** The system automatically recognizes your new country once the Excel file exists. Test it:

```r
# Load package
devtools::load_all()

# Load your raw data
xx_data <- read.csv("path/to/your/data.csv")  # or readxl::read_xlsx() for Excel

# Process the data - it just works!
result <- process_country_bsi(
  country = "XX",
  input_data = xx_data,
  episode_duration = 14,
  calculate_episodes = TRUE,
  write_to_file = TRUE,
  write_to_file_path = "output/"
)

# Check results
names(result)  # Should show: ehrbsi, patient, isolate, res
head(result$patient)
```

**Verification checklist:**
- ✓ Column names are correctly standardized (from Dictionary tab)
- ✓ Lookups are applied correctly (from Lookups tab)
- ✓ All four tables are generated (using Config tab settings)
- ✓ Episode calculations complete successfully
- ✓ Output Excel file is created

**No orchestrator modifications needed!** The system is fully dynamic.

## Step 3: (Optional) Add Custom R Logic

**Only needed if you have complex transformations that can't be expressed in Excel.**

Most countries don't need this step! But if you do:

Add to `R/countryConfigs.R` in the `COUNTRY_R_TRANSFORMS` list:

```r
COUNTRY_R_TRANSFORMS <- list(
  # ... existing countries ...
  
  XX = list(
    field_transforms = list(
      CustomField = function(data) {
        # Your complex R logic here
        dplyr::case_when(
          data$field1 == "A" & data$field2 > 10 ~ "Category1",
          data$field1 == "B" ~ "Category2",
          TRUE ~ NA_character_
        )
      },
      AnotherCustomField = function(data) {
        # More custom logic
        paste0(data$HospitalId, "_", data$UnitId)
      }
    )
  )
)
```

**Examples of when you need R logic:**
- Conditional logic based on multiple fields (see Malta's `patientType` transform)
- Gap analysis between admissions (see Estonia's `PreviousAdmission` transform)
- Complex string manipulations
- Date calculations beyond simple parsing

**Examples of what Config tab can handle (no R needed):**
- Column renaming
- Value lookups/mappings
- Date format parsing
- Record ID templating
- Default values
- Simple field mapping

## Summary

The new system simplifies country onboarding to:

### Required (Everyone):
1. **Excel file** (`reference/dictionaries/XX.xlsx`) - All three tabs:
   - Dictionary: Column name mappings
   - Lookups: Value mappings
   - Config: Processing configuration

### Optional (Complex Cases Only):
2. **R transforms** (`COUNTRY_R_TRANSFORMS` in `R/countryConfigs.R`) - Only for complex logic

### Not Needed Anymore:
- ❌ Country-specific recoding R files
- ❌ Orchestrator modifications
- ❌ Boilerplate wrapper functions

**Benefits:**
- ✅ 95% of countries need only Excel edits
- ✅ No R coding expertise required for standard cases
- ✅ Self-documenting configuration
- ✅ Easy to maintain and update
- ✅ Automatic recognition by system 
