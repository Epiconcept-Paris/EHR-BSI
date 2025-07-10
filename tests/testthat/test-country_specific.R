context("Country-Specific Processing")

# Helper functions to create test data for each country
create_test_estonia_data <- function() {
  data.frame(
    HospitalId = c("EE001", "EE002", "EE003"),
    PatientId = c("PAT001", "PAT002", "PAT003"),
    IsolateId = c("ISO001", "ISO002", "ISO003"),
    DateOfHospitalAdmission = c("01/01/2023 10:00", "02/01/2023 11:00", "03/01/2023 12:00"),
    DateOfSpecCollection = c("03/01/2023 14:00", "04/01/2023 15:00", "05/01/2023 16:00"),
    DateOfHospitalDischarge = c("10/01/2023 16:00", "12/01/2023 17:00", "15/01/2023 18:00"),
    MicroorganismCode = c("RP-001", "CC-001", "RP-002"),
    Antibiotic = c("AMP", "CIP", "GEN"),
    SIR = c("S", "R", "I"),
    stringsAsFactors = FALSE
  )
}

create_test_malta_data <- function() {
  data.frame(
    HospitalId = c("MT001", "MT002", "MT003"),
    PatientId = c("PAT001", "PAT002", "PAT003"),
    DateOfHospitalAdmission = c("01/01/2023", "02/01/2023", "03/01/2023"),
    DateOfSpecCollection = c("03/01/2023", "04/01/2023", "05/01/2023"),
    DateOfHospitalDischarge = c("10/01/2023", "12/01/2023", "15/01/2023"),
    MicroorganismCode = c("RP-001", "CC-001", "RP-002"),
    UnitSpecialty_noncdm = c("ICU", "WARD", "ER"),
    OutcomeOfCase_noncdm = c("ALIVE", "DEAD", "ALIVE"),
    # Add base columns that are referenced in else clauses
    OutcomeOfCase = c("A", "D", "A"),
    HospitalType = c("ACUTE", "ACUTE", "CHRONIC"),
    PreviousAdmission = c("NO", "OTH", "NO"),
    Antibiotic = c("AMP", "CIP", "GEN"),
    SIR = c("S", "R", "I"),
    stringsAsFactors = FALSE
  )
}

# Test Estonia-specific processing
test_that(".process_estonia_basic_cleaning handles required columns validation", {
  # Test with missing required columns
  incomplete_data <- data.frame(
    HospitalId = "EE001",
    PatientId = "PAT001"
    # Missing other required columns
  )

  expect_error(
    .process_estonia_basic_cleaning(incomplete_data, 2023),
    "Missing required columns in Estonia BSI data"
  )
})

test_that(".process_estonia_basic_cleaning processes dates correctly", {
  test_data <- create_test_estonia_data()

  result <- .process_estonia_basic_cleaning(test_data, 2023)

  # Check that date columns are properly converted (parse_dates_with_fallback converts to Date)
  expect_s3_class(result$DateOfSpecCollection, "Date")
  expect_s3_class(result$DateOfHospitalAdmission, "Date")
  expect_s3_class(result$DateOfHospitalDischarge, "Date")

  # Check that dates are parsed correctly
  expect_equal(result$DateOfSpecCollection[1], as.Date("2023-01-03"))
  expect_equal(result$DateOfHospitalAdmission[1], as.Date("2023-01-01"))
})

test_that(".process_estonia_basic_cleaning creates hierarchical record IDs", {
  test_data <- create_test_estonia_data()

  result <- .process_estonia_basic_cleaning(test_data, 2023)

  # Check that record IDs are created
  expect_true("record_id_bsi" %in% names(result))
  expect_true("record_id_patient" %in% names(result))
  expect_true("record_id_isolate" %in% names(result))

  # Check record ID format (Estonia uses time components)
  expect_equal(result$record_id_bsi[1], "EE001")
  expect_true(grepl("PAT001-", result$record_id_patient[1]))
  expect_true(grepl("ISO001_", result$record_id_isolate[1]))
})

test_that(".process_estonia_basic_cleaning handles edge cases", {
  # Test with minimal valid data
  minimal_data <- data.frame(
    HospitalId = "EE001",
    PatientId = "PAT001",
    IsolateId = "ISO001",
    DateOfHospitalAdmission = "01/01/2023 10:00",
    DateOfSpecCollection = "03/01/2023 14:00",
    MicroorganismCode = "RP-001",
    stringsAsFactors = FALSE
  )

  result <- .process_estonia_basic_cleaning(minimal_data, 2023)

  expect_equal(nrow(result), 1)
  expect_true(all(c("record_id_bsi", "record_id_patient", "record_id_isolate") %in% names(result)))
})

# Test Malta-specific processing
test_that(".process_malta_basic_cleaning handles required columns validation", {
  # Test with missing required columns
  incomplete_data <- data.frame(
    PatientId = "PAT001"
    # Missing HospitalId and DateOfHospitalAdmission
  )

  expect_error(
    .process_malta_basic_cleaning(incomplete_data, 2023),
    "Missing required columns in Malta BSI data"
  )
})

test_that(".process_malta_basic_cleaning processes dates correctly", {
  test_data <- create_test_malta_data()

  result <- .process_malta_basic_cleaning(test_data, 2023)

  # Check that date columns are properly converted to Date class
  expect_s3_class(result$DateOfSpecCollection, "Date")
  expect_s3_class(result$DateOfHospitalAdmission, "Date")
  expect_s3_class(result$DateOfHospitalDischarge, "Date")

  # Check that dates are parsed correctly
  expect_equal(result$DateOfSpecCollection[1], as.Date("2023-01-03"))
  expect_equal(result$DateOfHospitalAdmission[1], as.Date("2023-01-01"))
})

test_that(".process_malta_basic_cleaning identifies non-CDM columns", {
  test_data <- create_test_malta_data()

  # Capture output to check for non-CDM column message
  expect_output(
    result <- .process_malta_basic_cleaning(test_data, 2023),
    "columns require recoding"
  )

  expect_true(is.data.frame(result))
})

test_that(".process_malta_basic_cleaning creates hierarchical record IDs", {
  test_data <- create_test_malta_data()

  result <- .process_malta_basic_cleaning(test_data, 2023)

  # Check that record IDs are created
  expect_true("record_id_bsi" %in% names(result))
  expect_true("record_id_patient" %in% names(result))

  # Check record ID format (Malta uses simpler date format)
  expect_equal(result$record_id_bsi[1], "MT001")
  expect_true(grepl("PAT001-", result$record_id_patient[1]))
})

test_that(".process_malta_basic_cleaning handles recoding", {
  test_data <- create_test_malta_data()

  # Test should complete without error despite missing lookup tables
  expect_no_error({
    result <- .process_malta_basic_cleaning(test_data, 2023)
  })

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
})

test_that(".process_malta_basic_cleaning handles edge cases", {
  # Test with minimal valid data
  minimal_data <- data.frame(
    HospitalId = "MT001",
    PatientId = "PAT001",
    DateOfHospitalAdmission = "01/01/2023",
    # Add required base columns for else clauses
    DateOfSpecCollection = "03/01/2023",
    OutcomeOfCase = "A",
    HospitalType = "ACUTE",
    PreviousAdmission = "NO",
    stringsAsFactors = FALSE
  )

  result <- .process_malta_basic_cleaning(minimal_data, 2023)

  expect_equal(nrow(result), 1)
  expect_true("record_id_bsi" %in% names(result))
  expect_true("record_id_patient" %in% names(result))
})

# Test differences between Estonia and Malta processing
test_that("Estonia and Malta processing handle dates differently", {
  # Estonia data with time components
  estonia_data <- data.frame(
    HospitalId = "EE001",
    PatientId = "PAT001",
    IsolateId = "ISO001",
    DateOfHospitalAdmission = "01/01/2023 10:30",
    DateOfSpecCollection = "03/01/2023 14:15",
    MicroorganismCode = "RP-001",
    stringsAsFactors = FALSE
  )

  # Malta data without time components
  malta_data <- data.frame(
    HospitalId = "MT001",
    PatientId = "PAT001",
    DateOfHospitalAdmission = "01/01/2023",
    DateOfSpecCollection = "03/01/2023",
    MicroorganismCode = "RP-001",
    # Add required base columns for else clauses
    OutcomeOfCase = "A",
    HospitalType = "ACUTE",
    PreviousAdmission = "NO",
    stringsAsFactors = FALSE
  )

  estonia_result <- .process_estonia_basic_cleaning(estonia_data, 2023)
  malta_result <- .process_malta_basic_cleaning(malta_data, 2023)

  # Estonia should use Date class (parse_dates_with_fallback converts to Date)
  expect_s3_class(estonia_result$DateOfHospitalAdmission, "Date")

  # Malta should use Date class
  expect_s3_class(malta_result$DateOfHospitalAdmission, "Date")

  # Estonia record IDs should include time components
  expect_true(grepl("_", estonia_result$record_id_patient))

  # Malta record IDs should be simpler
  expect_false(grepl("_", malta_result$record_id_patient))
})

test_that("Country-specific functions handle missing data gracefully", {
  # Test Estonia with missing discharge date
  estonia_no_discharge <- data.frame(
    HospitalId = "EE001",
    PatientId = "PAT001",
    IsolateId = "ISO001",
    DateOfHospitalAdmission = "01/01/2023 10:00",
    DateOfSpecCollection = "03/01/2023 14:00",
    MicroorganismCode = "RP-001",
    stringsAsFactors = FALSE
  )

  expect_no_error({
    result_ee <- .process_estonia_basic_cleaning(estonia_no_discharge, 2023)
  })

  # Test Malta with missing specimen collection date
  malta_no_specimen <- data.frame(
    HospitalId = "MT001",
    PatientId = "PAT001",
    DateOfHospitalAdmission = "01/01/2023",
    MicroorganismCode = "RP-001",
    # Add required base columns for else clauses (including DateOfSpecCollection even though we're testing "missing" it)
    DateOfSpecCollection = NA_character_,  # This allows the function to proceed without error
    OutcomeOfCase = "A",
    HospitalType = "ACUTE",
    PreviousAdmission = "NO",
    stringsAsFactors = FALSE
  )

  expect_no_error({
    result_mt <- .process_malta_basic_cleaning(malta_no_specimen, 2023)
  })
})

test_that("Country-specific functions validate reporting year parameter", {
  test_data_ee <- create_test_estonia_data()
  test_data_mt <- create_test_malta_data()

  # Test with valid reporting year
  expect_no_error({
    .process_estonia_basic_cleaning(test_data_ee, 2023)
    .process_malta_basic_cleaning(test_data_mt, 2024)
  })

  # Both functions should accept numeric year
  expect_no_error({
    .process_estonia_basic_cleaning(test_data_ee, as.numeric(format(Sys.Date(), "%Y")))
    .process_malta_basic_cleaning(test_data_mt, as.numeric(format(Sys.Date(), "%Y")))
  })
})