#' Function to convert data from EARS-Net format into EHR-BSI format
#'
#' @param input_file_path Path to the input file, defaults to the working directory
#' @param write_to_file Whether to write the output list into separate output CSV-files
#' @param write_to_file_path File path where to write the files, defaults to the working directory
#' @param reporting_unit Reporting unit: "Hospital" is recommended, also accepts "Laboratory" or "Both" (Hospital and laboratory), or "Country"
#'
#' @return Returns a list of objects (data.tables) in EHR-BSI format
#' @export
#'
#' @examples
#' EHRBSI_Convertor(input_file = "ADD_TEST.csv")
EHRBSI_Convertor <- function(input_file = NULL,
                             input_file_path = NULL,
                             write_to_file = FALSE,
                             write_to_file_path = NULL,
                             reporting_unit = "Both"){

  # 0. Read in the EARS-Net csv
  if(is.null(input_file_path)){
    input_file_path <- getwd()
  }

  if(is.null(input_file)){
    input_file <- c("1.AMRTEST.csv")
  }

  temp <- fread(file.path(input_file_path,
                          input_file))

  # TODO: Some variables in the dataset are key for sensible
  # conversions - currently only checked for reporting unit, i.e.
  # HospitalId, LaboratoryCode and ReportingCountry

  # Further for example the following could be pre-checked:
  # PatientCounter, DateUsedForStatictics (Date of specimen),
  # DateOfHospitalisation, Pathogen, Antibiotic, SIR

  # 1. Set option for primary reporting unit used for filtering duplicates below
  # To be seen if hospital-lab/lab-hospital should be another option

  if(reporting_unit %in% c("Both", "both") | (any(grepl("^hosp", reporting_unit, ignore.case = TRUE)) &
     any(grepl("^lab", reporting_unit, ignore.case = TRUE)))){
    if (length(setdiff(c("HospitalId", "LaboratoryCode"), names(temp))) > 0) {
      stop(
        "Cannot proceed with the indicated 'reporting_unit'. One or both of the following variables are missing from the dataset: ",
        paste(c("HospitalId", "LaboratoryCode"), collapse = ", ")
      )
    }
    reporting_unit = c("HospitalId", "LaboratoryCode")
  }else if(grepl("^hosp", reporting_unit, ignore.case = TRUE)){
    if (!"HospitalId"%in% names(temp)) {
      stop(
        "Cannot proceed with the indicated 'reporting_unit'. The following variable is missing from the dataset: ",
        paste("HospitalId")
      )
    }
    reporting_unit = "HospitalId"
  }else if (grepl("^lab", reporting_unit, ignore.case = TRUE)){
    if (!"LaboratoryCode"%in% names(temp)) {
      stop(
        "Cannot proceed with the indicated 'reporting_unit'. The following variable is missing from the dataset: ",
        paste("LaboratoryCode")
      )
    }
    reporting_unit = "LaboratoryCode"
  }else if (grepl("^cou", reporting_unit, ignore.case = TRUE)){
    if (!"ReportingCountry"%in% names(temp)) {
      stop(
        "Cannot proceed with the indicated 'reporting_unit'. The following variable is missing from the dataset: ",
        paste("ReportingCountry")
      )
    }
    reporting_unit = "ReportingCountry"
  }else{
    stop("Please indicate the 'reporting_unit' as 'Hospital' or 'Laboratory' or 'both' or
         'Country'!")
  }

  # 2. Remove full duplicates (RecordId does not count)
  temp <- temp[!duplicated(temp,
                           by = setdiff(names(temp),
                                        c("RecordId")))]

  # 3. Split EARS-Net-file into EHRBSI-data.tables
  # Consider changing to neater with .SD and .SDCols
  ehrbsi <-  temp[,intersect(c("RecordId",
                               "RecordType",
                               "RecordTypeVersion",
                               "Subject",
                               "DataSource",
                               "DateUsedForStatistics",
                               "ReportingCountry",
                               "HospitalId",
                               "LaboratoryCode"),
                             names(temp)), with = FALSE]

  patient <- temp[,intersect(c("RecordId",
                               "ReportingCountry",
                               "HospitalId",
                               "LaboratoryCode",
                               "PatientCounter",
                               "Age",
                               "Gender",
                               "HospitalUnitType",
                               "patientType",
                               "DateOfHospitalisation",
                               "DateUsedForStatistics"),
                             names(temp)), with = FALSE]

  isolate <- temp[,intersect(c("RecordId",
                               "ReportingCountry",
                               "HospitalId",
                               "LaboratoryCode",
                               "PatientCounter",
                               "IsolateId",
                               "Specimen",
                               "DateUsedForStatistics",
                               "Pathogen"),
                             names(temp)), with = FALSE]

  res <-     temp[,intersect(c("RecordId",
                               "ReportingCountry",
                               "HospitalId",
                               "LaboratoryCode",
                               "PatientCounter",
                               "IsolateId",
                               "DateUsedForStatistics",
                               "Pathogen",
                               "Antibiotic",
                               "DiskLoad",
                               "ESBL",
                               "ReferenceGuidelinesSIR",
                               "ResultCarbapenemases",
                               "ResultEtestSign",
                               "ResultEtestSIR",
                               "ResultEtestValue",
                               "ResultGradSign",
                               "ResultGradSIR",
                               "ResultGradValue",
                               "ResultMICSign",
                               "ResultMICSIR",
                               "ResultMICValue",
                               "ResultPbp2aAggl",
                               "ResultPCRmec",
                               "ResultZoneSign",
                               "ResultZoneSIR",
                               "ResultZoneValue",
                               "Serotype",
                               "SIR"),
                             names(temp)), with = FALSE]

  # 4. Re-create the relational identifiers

  res[,RecordId := 1:nrow(res)]

  ID_Create(res,
            cols = c(reporting_unit, "PatientCounter", "IsolateId"),
            new_col = "ParentId")

  ID_Create(isolate,
            cols = c(reporting_unit, "PatientCounter"),
            new_col = "ParentId")

  ID_Create(isolate,
            cols = c(reporting_unit, "PatientCounter", "IsolateId"),
            new_col = "RecordId")

  ID_Create(patient,
            cols = c(reporting_unit, "PatientCounter"),
            new_col = "RecordId")

  ID_Create(patient,
            cols = reporting_unit,
            new_col = "ParentId")

  ID_Create(ehrbsi,
            cols = reporting_unit,
            new_col = "RecordId")


  # 5. Remove duplicates given that the information is now relational

  # First level stratification employed
  ehrbsi <- ehrbsi[!duplicated(ehrbsi[, ..reporting_unit])]

  # Same PatientCounter sometimes occurs in different hospitals/labs -
  # i.e. make sure that patient counter is combined with the first level
  # stratification
  reporting_unit <- c(reporting_unit, "PatientCounter")
  patient <- patient[!duplicated(patient[, ..reporting_unit])]

  # There seems to be some cases where same IsolateId appears for
  # different patientcounters - make sure the id is combined with
  # higher level patient stratifier
  reporting_unit <- c(reporting_unit, "PatientCounter", "IsolateId")
  isolate <- isolate[!duplicated(isolate[, ..reporting_unit])]

  # res duplicates might have occurred if varying information have been
  # removed or moved to an upper level - thus checking that no full duplicates
  res <- res[!duplicated(res,
                         by = setdiff(names(res),
                                      c("RecordId")))]

  # 6. Remove needless variables as the information is now relational

  patient[, intersect(c("ReportingCountry", "HospitalId", "LaboratoryCode",
                        "DateUsedForStatistics"), names(patient)) := NULL]

  isolate[,intersect(c("ReportingCountry", "HospitalId", "LaboratoryCode",
             "PatientCounter"), names(isolate)) := NULL]

  res[,intersect(c("ReportingCountry", "HospitalId", "LaboratoryCode",
         "PatientCounter", "IsolateId", "DateUsedForStatistics",
         "Pathogen"), names(res)) := NULL]

  # 7. Perform remaining recodes and mappings

  # TODO: still needs coded value filtering the values for Antibiotic?
  # Ad hoc filtering out Tigecycline "TGC" as not recognised by TESSy
  if ("Antibiotic" %in% names(res)) {
    res <- res[!Antibiotic %in%c("TGC")]
  }

  # UNK and N/A into missing (NA), "O" to "OTH"
  # Performed for each dataset separately
  UNK_O_Handler(ehrbsi)
  UNK_O_Handler(patient)
  UNK_O_Handler(isolate)
  UNK_O_Handler(res)

  # Recode necessary TESSy-variables

  # Data source
  ehrbsi[,DataSource := paste(ReportingCountry,
                              "EHRBSI",
                              sep = "-")]

  # Subject
  ehrbsi[,Subject := "EHRBSI"]

  # Record type version
  ehrbsi[,RecordTypeVersion := 1]

  # Record type for each file
  ehrbsi[,RecordType := "EHRBSI"]
  patient[,RecordType := "EHRBSI$Patient"]
  isolate[,RecordType := "EHRBSI$Patient$Isolate"]
  res[,RecordType := "EHRBSI$Patient$Isolate$Res"]

  # Use the lookup to update the HospitalUnitType if HospitalUnitType is available
  if ("HospitalUnitType" %in% names(patient)) {

    # Get the specialty lookup table
    Specialty_Lookup <- copy(Specialty_Lookup)

    patient[
      Specialty_Lookup,
      on = .(HospitalUnitType = fromearsval),
      HospitalUnitType := i.ehrval
    ]
  }

  # 8. Rename variables to EHR-BSI

  # Get the name lookup table
  Name_Lookup <- copy(Name_Lookup)

  # Apply to each data.table
  setnames(patient, Name_Lookup$earsval, Name_Lookup$ehrval, skip_absent = TRUE)
  setnames(isolate, Name_Lookup$earsval, Name_Lookup$ehrval, skip_absent = TRUE)
  setnames(res, Name_Lookup$earsval, Name_Lookup$ehrval, skip_absent = TRUE)


  # 9. Write to CSV, if selected
  if(write_to_file == TRUE){
    # Check is the path to write in is given, default to working directory
    if(is.null(write_to_file_path)){
      write_to_file_path <- getwd()
    }

    # Write the files
    fwrite(ehrbsi,
           file = file.path(write_to_file_path,
                            "1.EHRBSI.csv"))
    fwrite(patient,
           file = file.path(write_to_file_path,
                            "2.EHRBSI$Patient.csv"))
    fwrite(isolate,
           file = file.path(write_to_file_path,
                            "3.EHRBSI$Patient$Isolate.csv"))
    fwrite(res,
           file = file.path(write_to_file_path,
                            "4.EHRBSI$Patient$Isolate$Res.csv"))
  }

  # 10. Return the data items in a list
  return(list(ehrbsi = ehrbsi, patient = patient, isolate = isolate, res = res))

  }
