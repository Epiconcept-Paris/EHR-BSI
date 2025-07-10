context("recodingFuncs.R")

test_that("validate_required_columns: success case", {
  data <- data.frame(colA = 1:5, colB = letters[1:5])
  required <- c("colA", "colB")
  # No error should be thrown
  expect_silent(validate_required_columns(data, required))
})

test_that("validate_required_columns: error for one missing column", {
  data <- data.frame(colA = 1:5)
  required <- c("colA", "colB")
  expect_error(
    validate_required_columns(data, required, context_name = "my_data"),
    "Missing required columns in my_data: colB"
  )
})

test_that("validate_required_columns: error for multiple missing columns", {
  data <- data.frame(colC = 1:5)
  required <- c("colA", "colB")
  # The order of missing columns might vary, so check for both.
  expect_error(
    validate_required_columns(data, required),
    regexp = "Missing required columns in dataset: (colA, colB|colB, colA)"
  )
})

test_that("create_lookup_vector creates a correct lookup vector", {
  lookup_df <- data.frame(
    key = c("a", "b", "c"),
    value = c("Apple", "Banana", "Cherry"),
    stringsAsFactors = FALSE
  )
  expected_vector <- c(a = "Apple", b = "Banana", c = "Cherry")
  result_vector <- create_lookup_vector(lookup_df, value_col = "value", key_col = "key")
  expect_equal(result_vector, expected_vector)
})

test_that("create_lookup_vector handles empty data frame", {
  lookup_df <- data.frame(key = character(), value = character())
  expected_vector <- character(0)
  result_vector <- create_lookup_vector(lookup_df, value_col = "value", key_col = "key")
  expect_equal(result_vector, expected_vector)
})

test_that("create_lookup_vector handles NULL input", {
  expected_vector <- character(0)
  result_vector <- create_lookup_vector(NULL, value_col = "value", key_col = "key")
  expect_equal(result_vector, expected_vector)
})

test_that("recode_with_lookup recodes values correctly and keeps default", {
  test_data <- data.frame(col = c("a", "b", "d"), stringsAsFactors = FALSE)
  lookup <- c(a = "Apple", b = "Banana", c = "Cherry")
  
  result <- recode_with_lookup(test_data, "col", lookup)
  expected <- data.frame(col = c("Apple", "Banana", "d"), stringsAsFactors = FALSE)
  
  expect_equal(result, expected)
})

test_that("recode_with_lookup uses a fallback value", {
  test_data <- data.frame(col = c("a", "b", "d"), stringsAsFactors = FALSE)
  lookup <- c(a = "Apple", b = "Banana", c = "Cherry")
  
  result <- recode_with_lookup(test_data, "col", lookup, fallback_value = "Other")
  expected <- data.frame(col = c("Apple", "Banana", "Other"), stringsAsFactors = FALSE)
  
  expect_equal(result, expected)
})

test_that("recode_with_lookup handles an empty lookup vector", {
  test_data <- data.frame(col = c("a", "b", "d"), stringsAsFactors = FALSE)
  lookup <- character(0)
  
  result <- recode_with_lookup(test_data, "col", lookup)
  
  expect_equal(result, test_data)
})

test_that("recode_with_lookup handles a non-existent column", {
  test_data <- data.frame(col = c("a", "b", "d"), stringsAsFactors = FALSE)
  lookup <- c(a = "Apple", b = "Banana", c = "Cherry")
  
  result <- recode_with_lookup(test_data, "non_existent_col", lookup)
  
  expect_equal(result, test_data)
})

test_that("parse_dates_with_fallback uses fallback columns", {
  # This test covers the scenario where getAnyDictionaryValue doesn't exist.
  test_data <- data.frame(
    date1 = c("01/01/2023", "02/01/2023"),
    date2 = c("03/01/2023", "04/01/2023"),
    not_a_date = "text",
    stringsAsFactors = FALSE
  )
  
  result <- parse_dates_with_fallback(test_data, fallback_cols = c("date1", "date2"))
  
  expect_s3_class(result$date1, "Date")
  expect_s3_class(result$date2, "Date")
  expect_equal(result$date1[1], as.Date("2023-01-01"))
  expect_type(result$not_a_date, "character") # Should be untouched
})

test_that("parse_dates_with_fallback handles missing date columns gracefully", {
  test_data <- data.frame(not_a_date = "text")
  
  result <- parse_dates_with_fallback(test_data, fallback_cols = "a_date_col")
  
  expect_equal(result, test_data) # Data should be unchanged
})

test_that("finalize_table selects, arranges, and makes distinct", {
  test_data <- data.frame(
    A = c(1, 2, 1),
    B = c("x", "y", "x"),
    C = c(TRUE, FALSE, TRUE)
  )
  
  # Select and arrange, which should result in a distinct, ordered data frame
  result <- finalize_table(test_data, select_cols = c("C", "A"), arrange_cols = "A")
  
  # Expected result after selecting C and A, arranging by A, and taking distinct rows
  expected <- data.frame(C = c(TRUE, FALSE), A = c(1, 2))
  
  expect_equal(names(result), c("C", "A"))
  expect_equal(nrow(result), 2) # Check for distinctness
  expect_equal(result$A, c(1, 2)) # Check for arrangement
})

test_that("finalize_table handles non-existent columns gracefully", {
  test_data <- data.frame(A = 1, B = 2)
  
  # Non-existent select_cols are ignored
  result_select <- finalize_table(test_data, select_cols = c("A", "Z"))
  expect_equal(names(result_select), "A")
  
  # Non-existent arrange_cols are ignored
  result_arrange <- finalize_table(test_data, arrange_cols = c("B", "Z"))
  expect_equal(result_arrange, test_data) # arrange does not change order if only one row
})

test_that("finalize_table handles NULL for cols arguments", {
  test_data <- data.frame(B = c(2, 1, 2), A = c("y", "x", "y"))
  expected_data <- data.frame(B = c(2, 1), A = c("y", "x"))
  
  # Should just apply distinct
  result <- finalize_table(test_data, select_cols = NULL, arrange_cols = NULL)
  expect_equal(result, expected_data)
})

test_that("create_base_ehrbsi_table creates a correct base table", {
  source_data <- data.frame(
    my_id = "REC1",
    HospitalId = "HOS1",
    SomeOtherCol = "value"
  )
  
  result <- create_base_ehrbsi_table(
    data = source_data,
    country_code = "XX",
    reporting_year = 2023,
    episode_duration = 30,
    record_id_col = "my_id"
  )
  
  expect_equal(nrow(result), 1)
  expect_true("data.frame" %in% class(result))
  expect_equal(result$AggregationLevel, "HOSP")
  expect_equal(result$DataSource, "XX-EHRBSI")
  expect_equal(result$DateUsedForStatistics, 2023)
  expect_equal(result$EpisodeDuration, 30)
  expect_equal(result$HospitalId, "HOS1")
  expect_equal(result$RecordId, "REC1")
  expect_equal(result$ReportingCountry, "XX")
  # Check if other columns from source_data are preserved
  expect_true("SomeOtherCol" %in% names(result))
})

test_that("create_hierarchical_record_ids works with all optional columns", {
  source_data <- data.frame(
    HospitalId = "HOS1",
    PatientId = "PAT1",
    DateOfHospitalAdmission = as.Date("2023-01-15"),
    DateOfSpecCollection = as.Date("2023-01-20"),
    IsolateId = "ISO1",
    MicroorganismCode = "ORG1"
  )
  
  result <- create_hierarchical_record_ids(source_data)
  
  expect_equal(result$record_id_bsi, "HOS1")
  expect_equal(result$record_id_patient, "PAT1-15012023")
  expect_equal(result$record_id_isolate, "ISO1_ORG1")
})

test_that("create_hierarchical_record_ids works with missing optional columns", {
  source_data <- data.frame(
    HospitalId = "HOS1",
    PatientId = "PAT1"
  )
  
  result <- create_hierarchical_record_ids(source_data)
  
  expect_equal(result$record_id_bsi, "HOS1")
  expect_equal(result$record_id_patient, "PAT1")
  expect_false("record_id_isolate" %in% names(result))
})

test_that("create_hierarchical_record_ids handles only admission date", {
  source_data <- data.frame(
    HospitalId = "HOS1",
    PatientId = "PAT1",
    DateOfHospitalAdmission = as.Date("2023-01-15")
  )
  
  result <- create_hierarchical_record_ids(source_data)
  
  expect_equal(result$record_id_patient, "PAT1-15012023")
  expect_false("record_id_isolate" %in% names(result))
})

test_that("create_hierarchical_record_ids handles specimen date without isolate/organism", {
  source_data <- data.frame(
    HospitalId = "HOS1",
    PatientId = "PAT1",
    DateOfSpecCollection = as.Date("2023-01-20")
  )
  
  result <- create_hierarchical_record_ids(source_data)
  
  expect_equal(result$record_id_isolate, "PAT1-20012023")
})

test_that("create_standard_patient_table uses defaults and country overrides", {
  source_data <- data.frame(
    record_id_patient = "PAT1",
    record_id_bsi = "BSI1",
    patientType = "EMERGENCY" # This should be used over the default
  )
  
  country_overrides <- list(
    PatientSpecialty = "CARDIOLOGY",
    OutcomeOfCase = "DISCHARGED"
  )
  
  result <- create_standard_patient_table(
    source_data,
    country_defaults = country_overrides
  )
  
  expect_equal(result$RecordId, "PAT1")
  expect_equal(result$ParentId, "BSI1")
  expect_equal(result$patientType, "EMERGENCY") # Value from data
  expect_equal(result$PatientSpecialty, "CARDIOLOGY") # Value from override
  expect_equal(result$OutcomeOfCase, "DISCHARGED") # Value from override
  expect_equal(result$HospitalisationAdmissionCodeSystem, "ICD-10") # Value from default
})

test_that("create_standard_isolate_table uses defaults and country overrides", {
  source_data <- data.frame(
    record_id_isolate = "ISO1",
    PatientId = "PAT1",
    Specimen = "BLOOD" # This should be used over the default
  )
  
  country_overrides <- list(
    LaboratoryCode = "LAB_XYZ"
  )
  
  result <- create_standard_isolate_table(
    source_data,
    country_defaults = country_overrides
  )
  
  expect_equal(result$RecordId, "ISO1")
  expect_equal(result$ParentId, "PAT1")
  expect_equal(result$Specimen, "BLOOD") # Value from data
  expect_equal(result$LaboratoryCode, "LAB_XYZ") # Value from override
  expect_equal(result$MicroorganismCodeSystem, "SNOMED-CT") # Value from default
})

test_that("get_standard_table_columns returns correct columns for each table type", {
  expect_type(get_standard_table_columns("patient"), "character")
  expect_type(get_standard_table_columns("isolate"), "character")
  expect_type(get_standard_table_columns("res"), "character")
  expect_type(get_standard_table_columns("ehrbsi"), "character")
  
  # Check for a few key columns to ensure correctness without being too brittle
  expect_true("PatientId" %in% get_standard_table_columns("patient"))
  expect_true("MicroorganismCode" %in% get_standard_table_columns("isolate"))
  expect_true("Antibiotic" %in% get_standard_table_columns("res"))
  expect_true("HospitalId" %in% get_standard_table_columns("ehrbsi"))
})

test_that("get_standard_table_columns throws error for unknown table type", {
  expect_error(
    get_standard_table_columns("unknown_type"),
    "Unknown table_type: unknown_type. Must be one of: patient, isolate, res, ehrbsi"
  )
}) 