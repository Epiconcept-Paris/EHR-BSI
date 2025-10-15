# Country Configuration Reference

This document describes all possible configuration keys for the Config tab in country Excel files.

## Config Tab Structure

The Config tab should have three columns:
- `config_key`: The configuration parameter name
- `config_value`: The value for this parameter
- `config_type`: The data type (string, numeric, boolean, list)

## Required Configuration Keys

### Date Handling
- `date_format`: Date format string (e.g., "%d/%m/%Y" or "%d/%m/%Y %H:%M")
- `has_time`: Whether dates include time component (TRUE/FALSE)
- `date_columns`: Comma-separated list of date columns to parse

### Record IDs
- `record_id_bsi`: Template for BSI-level record IDs (e.g., "{HospitalId}-{year}")
- `record_id_patient`: Template for patient-level record IDs (e.g., "{PatientId}-{admit_date}")
- `record_id_isolate`: Template for isolate-level record IDs (e.g., "{IsolateId}_{MicroorganismCode}")

### Antibiotic Data Format

#### For Wide Format (Malta-style):
- `antibiotic_format`: "wide"
- `antibiotic_prefix`: Prefix for antibiotic columns (e.g., "ab_")

#### For Long Format (Estonia-style):
- `antibiotic_format`: "long"
- `antibiotic_test_column`: Column containing test names
- `antibiotic_result_column`: Column containing test results
- `antibiotic_value_column`: Column containing test values
- `antibiotic_unit_column`: Column containing units
- `antibiotic_translation_chain`: Comma-separated list of translation steps (e.g., "EST2ENG,ENG2HAI")
- `test_type_grad`: Pattern to identify gradient diffusion tests (e.g., " Grad$")
- `test_type_mic`: Pattern to identify MIC tests (e.g., " MIK$")
- `test_type_zone`: Pattern to identify zone tests (e.g., " Disk$")

### Terminology Systems
- `terminology_clinical`: Clinical coding system (e.g., "ICD-10" or "SNOMED-CT")
- `terminology_clinical_spec`: Specification version (optional)
- `terminology_microbiological`: Microbiological coding system (e.g., "SNOMED-CT")
- `terminology_microbiological_spec`: Specification version (optional)
- `terminology_hospitalisation`: Hospitalisation coding system (e.g., "ICD-10")

### Lookups
- `lookups`: Comma-separated list of lookup table names to load from Lookups tab

### Lookup Mappings

For each lookup mapping, use the pattern `lookup_mapping_{NAME}_{PROPERTY}`:

- `lookup_mapping_{NAME}_column`: Source column to map from
- `lookup_mapping_{NAME}_from`: Source column in lookup table
- `lookup_mapping_{NAME}_to`: Target column in lookup table
- `lookup_mapping_{NAME}_output_column`: Output column name in data
- `lookup_mapping_{NAME}_fallback`: (optional) Default value for unmapped items
- `lookup_mapping_{NAME}_fallback_prefix`: (optional) Prefix to add to unmapped values

Example:
```
lookup_mapping_HospType_column,HospitalId,string
lookup_mapping_HospType_from,malta_hosptype,string
lookup_mapping_HospType_to,hosptype_code,string
lookup_mapping_HospType_output_column,HospitalType,string
lookup_mapping_HospType_fallback,NOT CODED,string
```

### Table Defaults

For each table type (patient, isolate, ehrbsi), use the pattern `default_{TABLE}_{FIELD}`:

- `default_patient_{FIELD}`: Default value for patient table field
- `default_isolate_{FIELD}`: Default value for isolate table field
- `default_ehrbsi_{FIELD}`: Default value for EHRBSI table field

Example:
```
default_patient_LaboratoryCode,MT001,string
default_isolate_Specimen,BLOOD,string
default_ehrbsi_ESurvBSI,2,numeric
```

### Cleanup
- `noncdm_cleanup`: Comma-separated list of non-CDM columns to remove after processing

### Special Fields
- `special_field_{TARGET}`: Map source field to target field (e.g., "special_field_DateOfSpecCollection,EpisodeStartDate_noncdm")

## Data Types

- `string`: Text values
- `numeric`: Numbers (integers or decimals)
- `boolean`: TRUE or FALSE
- `list`: Comma-separated values that will be split into a vector

## Complete Example

See `MT_Config_Template.csv` and `EE_Config_Template.csv` for complete working examples.

## Notes for Complex Logic

If your country requires complex R logic (e.g., conditional transformations based on multiple fields), you should:

1. Define all standard configuration in the Config tab
2. Add R functions to `COUNTRY_R_TRANSFORMS` in `R/countryConfigs.R` for the complex logic only

Example of R-only transforms:
```r
COUNTRY_R_TRANSFORMS <- list(
  XX = list(
    field_transforms = list(
      ComplexField = function(data) {
        # Your custom R logic here
        dplyr::case_when(
          data$field1 == "A" & data$field2 > 10 ~ "Category1",
          data$field1 == "B" ~ "Category2",
          TRUE ~ NA_character_
        )
      }
    )
  )
)
```

## Migration from Old System

If you have an existing country configuration in `COUNTRY_CONFIGS`, extract the values into the Config tab format:

1. Copy configuration values from `COUNTRY_CONFIGS[["{country_code}"]]`
2. Flatten nested lists using the naming patterns described above
3. Create CSV file with config_key, config_value, config_type columns
4. Import CSV as new "Config" tab in your country Excel file
5. Test that the new system produces identical results

