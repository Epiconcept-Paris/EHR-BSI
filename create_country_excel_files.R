# Temporary script to create country-specific Excel files with Dictionary and Lookups tabs
# This script reads from Lookup_Tables.R and creates properly structured Excel files

library(openxlsx)
library(data.table)

# Source the lookup tables
source("reference/Lookup_Tables.R")

# ============================================================================
# CREATE MALTA EXCEL FILE (MT.xlsx)
# ============================================================================

# --- Dictionary Tab for Malta ---
# This should contain raw_column_name to standard_column_name mappings
# For Malta, the epiuf dictionary was previously used, so we need to define the mappings
malta_dictionary <- data.frame(
  raw_column_name = c(
    "PatientCounter", "Gender", "HospitalUnitType", "DateOfHospitalisation",
    "DateUsedForStatistics", "Pathogen", "ESBL", "ResultCarbapenemases",
    "ResultGradSign", "ResultGradValue", "ResultGradSIR", "ResultMICSign",
    "ResultMICValue", "ResultMICSIR", "ResultZoneSign", "ResultZoneValue",
    "ResultZoneSIR", "DiskLoad"
  ),
  standard_column_name = c(
    "PatientId", "Sex", "PatientSpecialty", "DateOfHospitalAdmission",
    "DateOfSpecCollection", "MicroorganismCode", "ResultESBL", "ResultCarbapenemase",
    "GradSusceptibilitySign", "GradValue", "GradSIR", "MICSusceptibilitySign",
    "MICValue", "MICSIR", "ZoneSusceptibilitySign", "ZoneValue",
    "ZoneSIR", "ZoneTestDiskLoad"
  ),
  stringsAsFactors = FALSE
)

# --- Lookups Tab for Malta ---
# Stack all Malta lookups into one long table

# Name_Lookup (shared)
name_lookup_long <- data.frame(
  lookup_name = "Name_Lookup",
  from_value = Name_Lookup$earsval,
  to_value = Name_Lookup$ehrval,
  stringsAsFactors = FALSE
)

# Specialty_Lookup (shared)
specialty_lookup_long <- data.frame(
  lookup_name = "Specialty_Lookup",
  from_value = Specialty_Lookup$fromearsval,
  to_value = Specialty_Lookup$ehrval,
  stringsAsFactors = FALSE
)

# Malta_UnitSpecialty
malta_unitspec_long <- data.frame(
  lookup_name = "Malta_UnitSpecialty",
  from_value = Malta_UnitSpecialty_Lookup$malta_code,
  to_value = Malta_UnitSpecialty_Lookup$generic_code,
  stringsAsFactors = FALSE
)

# Malta_Outcome
malta_outcome_long <- data.frame(
  lookup_name = "Malta_Outcome",
  from_value = Malta_Outcome_Lookup$malta_code,
  to_value = Malta_Outcome_Lookup$generic_code,
  stringsAsFactors = FALSE
)

# Malta_HospType
malta_hosptype_long <- data.frame(
  lookup_name = "Malta_HospType",
  from_value = Malta_HospType_Lookup$malta_hosptype,
  to_value = Malta_HospType_Lookup$hosptype_code,
  stringsAsFactors = FALSE
)

# Malta_PathogenCode
malta_pathogen_long <- data.frame(
  lookup_name = "Malta_PathogenCode",
  from_value = Malta_PathogenCode_Lookup$malta_pathogen_name,
  to_value = Malta_PathogenCode_Lookup$microorganism_code,
  stringsAsFactors = FALSE
)

# Combine all Malta lookups
malta_lookups <- rbind(
  name_lookup_long,
  specialty_lookup_long,
  malta_unitspec_long,
  malta_outcome_long,
  malta_hosptype_long,
  malta_pathogen_long
)

# Create Malta workbook
wb_malta <- createWorkbook()
addWorksheet(wb_malta, "Dictionary")
addWorksheet(wb_malta, "Lookups")

writeData(wb_malta, "Dictionary", malta_dictionary)
writeData(wb_malta, "Lookups", malta_lookups)

saveWorkbook(wb_malta, "reference/dictionaries/MT.xlsx", overwrite = TRUE)
cat("Created reference/dictionaries/MT.xlsx\n")

# ============================================================================
# CREATE ESTONIA EXCEL FILE (EE.xlsx)
# ============================================================================

# --- Dictionary Tab for Estonia ---
# Estonia uses column names directly, so this might be minimal or empty
# Based on the code, Estonia doesn't seem to require as much renaming
estonia_dictionary <- data.frame(
  raw_column_name = c(
    "PatientCounter", "Gender", "HospitalUnitType", "DateOfHospitalisation",
    "DateUsedForStatistics", "Pathogen", "ESBL", "ResultCarbapenemases",
    "ResultGradSign", "ResultGradValue", "ResultGradSIR", "ResultMICSign",
    "ResultMICValue", "ResultMICSIR", "ResultZoneSign", "ResultZoneValue",
    "ResultZoneSIR", "DiskLoad"
  ),
  standard_column_name = c(
    "PatientId", "Sex", "PatientSpecialty", "DateOfHospitalAdmission",
    "DateOfSpecCollection", "MicroorganismCode", "ResultESBL", "ResultCarbapenemase",
    "GradSusceptibilitySign", "GradValue", "GradSIR", "MICSusceptibilitySign",
    "MICValue", "MICSIR", "ZoneSusceptibilitySign", "ZoneValue",
    "ZoneSIR", "ZoneTestDiskLoad"
  ),
  stringsAsFactors = FALSE
)

# --- Lookups Tab for Estonia ---

# Estonia_MecRes (Note: this is a special lookup with resistance_type and resistance_value)
estonia_mecres_long <- data.frame(
  lookup_name = "Estonia_MecRes",
  from_value = Estonia_MecRes_Lookup$resistance_value,
  to_value = Estonia_MecRes_Lookup$resistance_type,
  stringsAsFactors = FALSE
)

# Estonia_ResRecode
estonia_resrecode_long <- data.frame(
  lookup_name = "Estonia_ResRecode",
  from_value = Estonia_ResRecode_Lookup$estonia_result,
  to_value = Estonia_ResRecode_Lookup$generic_result,
  stringsAsFactors = FALSE
)

# Estonia_Ab_EST2ENG
estonia_ab_est2eng_long <- data.frame(
  lookup_name = "Estonia_Ab_EST2ENG",
  from_value = Estonia_Ab_EST2ENG_Lookup$estonia_name,
  to_value = Estonia_Ab_EST2ENG_Lookup$english_name,
  stringsAsFactors = FALSE
)

# Estonia_Ab_ENG2HAI
estonia_ab_eng2hai_long <- data.frame(
  lookup_name = "Estonia_Ab_ENG2HAI",
  from_value = Estonia_Ab_ENG2HAI_Lookup$english_name,
  to_value = Estonia_Ab_ENG2HAI_Lookup$generic_name,
  stringsAsFactors = FALSE
)

# Estonia_HospType
estonia_hosptype_long <- data.frame(
  lookup_name = "Estonia_HospType",
  from_value = Estonia_HospType_Lookup$estonia_hosptype,
  to_value = Estonia_HospType_Lookup$hosptype_code,
  stringsAsFactors = FALSE
)

# Estonia_HospGeog
estonia_hospgeog_long <- data.frame(
  lookup_name = "Estonia_HospGeog",
  from_value = Estonia_HospGeog_Lookup$estonia_hosptype,
  to_value = Estonia_HospGeog_Lookup$nuts3_code,
  stringsAsFactors = FALSE
)

# Combine all Estonia lookups
estonia_lookups <- rbind(
  name_lookup_long,
  specialty_lookup_long,
  estonia_mecres_long,
  estonia_resrecode_long,
  estonia_ab_est2eng_long,
  estonia_ab_eng2hai_long,
  estonia_hosptype_long,
  estonia_hospgeog_long
)

# Create Estonia workbook
wb_estonia <- createWorkbook()
addWorksheet(wb_estonia, "Dictionary")
addWorksheet(wb_estonia, "Lookups")

writeData(wb_estonia, "Dictionary", estonia_dictionary)
writeData(wb_estonia, "Lookups", estonia_lookups)

saveWorkbook(wb_estonia, "reference/dictionaries/EE.xlsx", overwrite = TRUE)
cat("Created reference/dictionaries/EE.xlsx\n")

cat("\nExcel files created successfully!\n")

