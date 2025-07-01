# Create a naming lookup
Name_Lookup <- data.table::data.table(
  earsval = c("PatientCounter", "Gender", "HospitalUnitType", "DateOfHospitalisation",
              "DateUsedForStatistics", "Pathogen",
              "ESBL", "ResultCarbapenemases", "ResultGradSign", "ResultGradValue",
              "ResultGradSIR", "ResultMICSign", "ResultMICValue", "ResultMICSIR",
              "ResultZoneSign", "ResultZoneValue", "ResultZoneSIR", "DiskLoad"),
  ehrval = c("PatientId", "Sex", "PatientSpecialty", "DateOfHospitalAdmission",
             "DateOfSpecimenCollection", "MicroorganismCode",
             "ResultESBL", "ResultCarbapenemase", "GradSusceptibilitySign", "GradValue",
             "GradSIR", "MICSusceptibilitySign", "MICValue", "MICSIR",
             "ZoneSusceptibilitySign", "ZoneValue", "ZoneSIR", "ZoneTestDiskLoad")
)

# Create a lookup for specialties - note the current mapping limitations
# between Emergency department, primary care, and mixed - unknown
Specialty_Lookup <- data.table::data.table(
  fromearsval = c("ED", "ICU", "INFECT", "INTMED", "OTH",  "OBGYN", "ONCOL", "PEDS", "PEDSICU", "PHC", "SURG", "UNK",  "URO"),
  ehrval = c("MIX", "ICUMIX", "MEDID", "MEDGEN", "OTH", "GOGYN", "MEDONCO", "PEDGEN", "ICUPED", "MIX", "SURGEN", "", "SURURO"),
  toearsval = c("UNK", "ICU", "INFECT", "INTMED", "OTH", "OBGYN", "ONCOL", "PEDS",  "PEDSICU", "UNK", "SURG", "UNK", "URO")
)

usethis::use_data(Name_Lookup, Specialty_Lookup, internal = FALSE, overwrite = TRUE)
