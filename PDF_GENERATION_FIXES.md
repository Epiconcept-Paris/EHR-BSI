# PDF Generation Troubleshooting & Fixes

## Issues Found and Fixed

### 1. ✅ YAML Syntax Error  
**Issue:** Quarto doesn't support `!r` tags like R Markdown  
**Fix:** Changed `report_date: !r Sys.Date()` to `report_date: NULL` and handle date in R code

### 2. ✅ Module Path Not Found
**Issue:** `reportModules.R` couldn't be found when template copied to temp directory  
**Fix:** Pass absolute path to `module_path` as a parameter to Quarto

### 3. ✅ NULL Checking Errors
**Issue:** `nrow(NULL)` returns `NULL`, not `0`, causing "missing value where TRUE/FALSE needed"  
**Fix:** Split NULL checks and nrow() checks into separate steps throughout all functions

### 4. ✅ Date Formatting Error
**Issue:** Trying to format an already-formatted date string  
**Fix:** Check if date is already character before formatting

### 5. ✅ File Path Handling
**Issue:** PDF being saved to repo directory instead of download  
**Fix:** Working directory management and proper file copying from temp location

### 6. 🔧 IN PROGRESS: Empty Sections
**Issue:** Episodes/Context/Antibiograms/Hospital sections show only headers  
**Current Status:** Added debug output and fixed episode data passing

## Current Debug Strategy

Added debug output to identify what data is being received:

```r
# In bsi_report.qmd episodes-check chunk
DEBUG: Episodes data has XXX rows
DEBUG: episode_summary has XXX rows and columns: ...
```

## Expected Behavior

When working correctly, the PDF should contain:

1. ✅ **Summary** - Data cleaning pie chart, facilities info
2. ✅ **Patient Demographics** - Gender/age distributions  
3. 🔧 **Episodes** - Episode composition, pathogen charts, infection types
4. 🔧 **Context** - Specialty analysis
5. 🔧 **Antibiograms** - Resistance tables
6. 🔧 **Hospital Analysis** - Ward-level data

## Next Steps to Complete

1. **Check Debug Output** - Run PDF generation and check console for DEBUG lines
2. **Verify episode_summary** - Ensure it has required columns (`Polymicrobial`, `Pathogens`)
3. **Test Visualizations** - Check if plot functions work with actual data
4. **Remove Debug Output** - Once working, remove cat() statements

## Testing Procedure

```r
# 1. Start app
library(EHRBSI)
visual_bsi_dashboard()

# 2. Upload test data
# Use one of: reference/dictionaries/CZ.xlsx, EE.xlsx, or MT.xlsx

# 3. Process data
# Click "Process" button

# 4. Generate PDF
# Click "Download PDF Report"

# 5. Check console for debug output
# Look for "DEBUG: Episodes data has..." messages

# 6. Verify PDF content
# Open downloaded PDF and check all sections
```

## Common Issues

### Issue: "params$report_data$episodes is NULL"
**Cause:** Episodes not being generated or passed  
**Fix:** Check that `calculateEpisodes()` ran successfully

### Issue: "episode_summary is NULL"
**Cause:** `calculateEpisodes()` didn't return episode_summary  
**Fix:** Verify calculateEpisodes returns list with episode_summary

### Issue: Plots show "No data available"
**Cause:** Data structure doesn't match expected format  
**Fix:** Check required columns exist (Polymicrobial, Pathogens, etc.)

## File Modifications Summary

- `R/visualiseDashboard.r` - PDF download handler with better data passing
- `inst/reports/bsi_report.qmd` - Fixed conditionals and added debug output
- `R/reportModules.R` - Fixed NULL checking in all functions
- `DESCRIPTION` - Added DT, rmarkdown, knitr dependencies

