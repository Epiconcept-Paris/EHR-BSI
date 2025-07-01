#' Function to convert data from EHR-BSI format to EARS-Net format
#'
#' Note: In 2025, changes with EpiPulse cases expected to the EARS-Net metadata, thus the conversion is an interim version.
#'
#' @param input_file Input CSV files in EHR-BSI format, please give a vector of names for all 4 files "EHRBSI", "Patient", "Isolate" and "Res", defaults to TESSy file names
#' @param input_file_path Path to the input files, defaults to the working directory
#' @param write_to_file Whether to write the output list into separate output CSV-files
#' @param write_to_file_path File path where to write the files, defaults to the working directory
#'
#' @return Returns a data.table in EARS-Net format
#' @export
#'
#' @examples
#' From_EHRBSI_Convertor(input_file_path = "C:/DUMMY_DATA/PATH")
From_EHRBSI_converter <- function(input_file = NULL,
                                  input_file_path = NULL,
                                  write_to_file = FALSE,
                                  write_to_file_path = NULL){

  # TODO: EHR-BSI datasets 1.EHRBSI.csv,
  # 2.EHRBSI$Patient.csv, 3.EHRBSI$Patient$Isolate.csv
  # and 4.EHRBSI$Patient$Isolate$Res.csv should be
  # available for the conversion to be meaningful

  # 0. Read in the EHR-BSI csv-files
  if(is.null(input_file_path)){
    input_file_path <- getwd()
  }

  if(is.null(input_file)){
    input_file <- c("1.EHRBSI.csv",
                    "2.EHRBSI$Patient.csv",
                    "3.EHRBSI$Patient$Isolate.csv",
                    "4.EHRBSI$Patient$Isolate$Res.csv")
  }

  # Read the highest level data
  ehrbsi <- fread(file.path(input_file_path,
                            input_file[1]),
                  drop = c("RecordType",
                           "RecordTypeVersion",
                           "Subject"))

  # Read the patient level data
  patient <- fread(file.path(input_file_path,
                             input_file[2]),
                   drop = c("RecordType"))

  # Read the isolate level data
  isolate <- fread(file.path(input_file_path,
                             input_file[3]),
                   drop = c("RecordType"))

  # Read the resistance level data
  res <- fread(file.path(input_file_path,
                         input_file[4]),
               drop = c("RecordType"))

  # 2. Combine the files into one EARS-Net flat file
  ears <- merge(ehrbsi,
                patient,
                by.x = "RecordId",
                by.y = "ParentId",
                all = TRUE)

  # TODO: Check the behaviour as patient-isolates should sometimes result
  # to same patient with multiple isolates (even in EARS-Net?)?
  ears <- merge(ears,
                isolate,
                by.x = "RecordId.y",
                by.y = "ParentId",
                all = TRUE)

  ears <- merge(ears,
                res,
                by.x = "RecordId.y.y",
                by.y = "ParentId",
                all = TRUE)


  # 3. Check RecordId uniqueness and remove duplicates
  if(!length(unique(ears$RecordId)) == nrow(ears)){
    ears$RecordId <- c(1:nrow(ears))
  }

  # Check if LaboratoryCode appears at two levels
  if("LaboratoryCode"%in%names(isolate) &
     "LaboratoryCode"%in%names(ehrbsi)){
    if(all(ears$LaboratoryCode.x == ears$LaboratoryCode.y)){
      ears[LaboratoryCode.y := NULL]
      setnames(ears, "LaboratoryCode.x", "LaboratoryCode")
    }else{
      # TODO: Allow to show the mismatch!
      stop("Mismatch in the LaboratoryCode between levels 1.EHRBSI.csv and 3.EHRBSI$Patient$Isolate.csv, please check!")
    }
  }

  # Remove old duplicated ids from different levels not needed
  # after the combination
  ears[,c("RecordId.y.y",
          "RecordId.y",
          "RecordId.x") := NULL]

  # Check and remove duplicates, if occurring
  # TODO: Needs to log these probably.
  ears <- ears[!duplicated(ears,
                           by = setdiff(names(ears),
                                        c("RecordId")))]

  # 4. Rename variables to EARS-Net

  # Get the name lookup table
  Name_Lookup <- copy(Name_Lookup)

  # Apply to the data.table
  setnames(ears, Name_Lookup$ehrval, Name_Lookup$earsval, skip_absent = TRUE)


  # 5. Perform remaining recodes and mappings

  # Recode necessary TESSy-variables

  # DataSource - assumes ReportingCountry-AMR, but might need to adapt
  ears[,DataSource := paste(ReportingCountry,
                            "AMR",
                            sep = "-")]

  # Subject
  ears[,Subject := "AMRTEST"]

  # Record type version - assumes 1 but might need to adapt
  ears[,RecordTypeVersion := 1]

  # Record type
  ears[,RecordType := "AMRTEST"]

  # Use the lookup to update the HospitalUnitType if HospitalUnitType is available
  if ("HospitalUnitType" %in% names(patient)) {

    # Get the specialty lookup table
    Specialty_Lookup <- copy(Specialty_Lookup)

    ears[
      Specialty_Lookup,
      on = .(HospitalUnitType = ehrval),
      HospitalUnitType := i.toearsval
    ]
  }

  # 6. Write to CSV, if selected
  if(write_to_file == TRUE){
    # Check is the path to write in is given, default to working directory
    if(is.null(write_to_file_path)){
      write_to_file_path <- getwd()
    }

    # Write the file
    fwrite(ears,
           file = file.path(write_to_file_path,
                            "1.AMRTEST.csv"))
  }

  # 6. Return the data.table
  return(data.table(ears))
}
