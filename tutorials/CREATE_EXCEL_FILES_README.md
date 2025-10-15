# Creating Country-Specific Excel Files

## Important: Run this script to generate the Excel files

The codebase has been migrated from using `epiuf` package and R-based lookup tables to using country-specific Excel files. To complete the migration, you need to generate the Excel files from the existing lookup data.

## How to Run

1. Open RStudio and navigate to the project directory
2. Run the following command in the R console:

```r
source("create_country_excel_files.R")
```

This will create:
- `reference/dictionaries/MT.xlsx` - Malta dictionary and lookups
- `reference/dictionaries/EE.xlsx` - Estonia dictionary and lookups

## Excel File Structure

Each Excel file contains two tabs:

### Dictionary Tab
- Column A: `raw_column_name` - Original column names from raw data
- Column B: `standard_column_name` - Standardized EHR-BSI column names

### Lookups Tab
- Column A: `lookup_name` - Name of the lookup table
- Column B: `from_value` - Original value in raw data
- Column C: `to_value` - Standardized/mapped value

All lookup tables are stacked in one long table on this tab.

## After Running

Once the Excel files are created:
1. Verify they contain the expected data by opening them
2. You can delete the `create_country_excel_files.R` script if desired (it's only needed once)
3. The old `reference/Lookup_Tables.R` and `.rda` files have been removed

## Dependencies

The script requires:
- `openxlsx` package for creating Excel files
- `data.table` package for data manipulation

Install if needed:
```r
install.packages(c("openxlsx", "data.table"))
```

