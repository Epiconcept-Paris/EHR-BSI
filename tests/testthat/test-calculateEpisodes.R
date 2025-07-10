context("calculateEpisodes.R")

test_that("calculateEpisodes runs without errors with typical data", {

  # Sample data setup
  patient_df <- data.frame(
    RecordId = c("ADM-001", "ADM-002", "ADM-003"),
    PatientId = c("PAT-1", "PAT-1", "PAT-2"),
    ParentId = c("HOSP-A", "HOSP-A", "HOSP-B"),
    DateOfHospitalAdmission = as.Date(c("2023-01-01", "2023-02-10", "2023-01-15")),
    DateOfHospitalDischarge = as.Date(c("2023-01-10", "2023-02-20", "2023-01-25")),
    stringsAsFactors = FALSE
  )

  isolate_df <- data.frame(
    RecordId = paste0("ISO-", 1:5),
    ParentId = c("PAT-1", "PAT-1", "PAT-1", "PAT-2", "PAT-2"),
    DateOfSpecCollection = as.Date(c("2023-01-03", "2023-01-12", "2023-02-12", "2023-01-16", "2023-01-20")),
    MicroorganismCode = c("RP-1", "CC-1", "RP-2", "RP-1", "CC-1"),
    stringsAsFactors = FALSE
  )

  commensal_df <- data.frame(
    SNOMED.Code = c("CC-1", "CC-2"),
    Organism = c("Commensal One", "Commensal Two"),
    stringsAsFactors = FALSE
  )

  # Run the function
  episodes <- calculateEpisodes(
    patient_df = patient_df,
    isolate_df = isolate_df,
    commensal_df = commensal_df,
    episodeDuration = 14
  )

  # Basic checks
  expect_true(is.data.frame(episodes))
  expect_gt(nrow(episodes), 0)

})

test_that("Rule 1: Recognised pathogen creates a BSI case", {
  patient_df <- data.frame(
    RecordId = "ADM-001", PatientId = "PAT-1", ParentId = "HOSP-A",
    DateOfHospitalAdmission = as.Date("2023-01-01"), DateOfHospitalDischarge = as.Date("2023-01-10"),
    stringsAsFactors = FALSE
  )
  isolate_df <- data.frame(
    RecordId = "ISO-1", ParentId = "PAT-1",
    DateOfSpecCollection = as.Date("2023-01-03"), MicroorganismCode = "RP-1",
    stringsAsFactors = FALSE
  )
  commensal_df <- data.frame(SNOMED.Code = "CC-1", Organism = "Commensal", stringsAsFactors = FALSE)

  episodes <- calculateEpisodes(patient_df, isolate_df, commensal_df)

  expect_equal(nrow(episodes), 1)
  expect_equal(episodes$MicroorganismCode, "RP-1")
})

test_that("Rule 2: Two concordant CCs create a BSI case", {
  patient_df <- data.frame(
    RecordId = "ADM-001", PatientId = "PAT-1", ParentId = "HOSP-A",
    DateOfHospitalAdmission = as.Date("2023-01-01"), DateOfHospitalDischarge = as.Date("2023-01-10"),
    stringsAsFactors = FALSE
  )
  isolate_df <- data.frame(
    RecordId = c("ISO-1", "ISO-2"), ParentId = c("PAT-1", "PAT-1"),
    DateOfSpecCollection = as.Date(c("2023-01-03", "2023-01-05")), # 2 days apart
    MicroorganismCode = c("CC-1", "CC-1"),
    stringsAsFactors = FALSE
  )
  commensal_df <- data.frame(SNOMED.Code = "CC-1", Organism = "Commensal", stringsAsFactors = FALSE)

  episodes <- calculateEpisodes(patient_df, isolate_df, commensal_df)

  expect_equal(nrow(episodes), 1)
  expect_equal(episodes$MicroorganismCode, "CC-1")
})

test_that("A single common commensal does NOT create a BSI case", {
  patient_df <- data.frame(
    RecordId = "ADM-001", PatientId = "PAT-1", ParentId = "HOSP-A",
    DateOfHospitalAdmission = as.Date("2023-01-01"), DateOfHospitalDischarge = as.Date("2023-01-10"),
    stringsAsFactors = FALSE
  )
  isolate_df <- data.frame(
    RecordId = "ISO-1", ParentId = "PAT-1",
    DateOfSpecCollection = as.Date("2023-01-03"), MicroorganismCode = "CC-1",
    stringsAsFactors = FALSE
  )
  commensal_df <- data.frame(SNOMED.Code = "CC-1", Organism = "Commensal", stringsAsFactors = FALSE)

  # Suppress the "no cases found" message
  suppressMessages(
    episodes <- calculateEpisodes(patient_df, isolate_df, commensal_df)
  )

  # This should return an empty or zero-row data frame for episodes.
  # The exact return value might vary, so we check for zero rows.
  expect_equal(nrow(episodes), 0)
})

test_that("Polymicrobial episode: isolates within 3 days are grouped", {
  patient_df <- data.frame(
    RecordId = "ADM-001", PatientId = "PAT-1", ParentId = "HOSP-A",
    DateOfHospitalAdmission = as.Date("2023-01-01"), DateOfHospitalDischarge = as.Date("2023-01-30"),
    stringsAsFactors = FALSE
  )
  isolate_df <- data.frame(
    RecordId = c("ISO-1", "ISO-2"), ParentId = c("PAT-1", "PAT-1"),
    DateOfSpecCollection = as.Date(c("2023-01-03", "2023-01-05")), # 2 days apart
    MicroorganismCode = c("RP-1", "RP-2"),
    stringsAsFactors = FALSE
  )
  commensal_df <- data.frame(SNOMED.Code = "CC-1", Organism = "Commensal", stringsAsFactors = FALSE)

  episodes <- calculateEpisodes(patient_df, isolate_df, commensal_df, episodeDuration = 14)

  expect_equal(nrow(episodes), 1)
  expect_true(episodes$Polymicrobial)
})

test_that("New episode started for different organism after 3 days", {
  patient_df <- data.frame(
    RecordId = "ADM-001", PatientId = "PAT-1", ParentId = "HOSP-A",
    DateOfHospitalAdmission = as.Date("2023-01-01"), DateOfHospitalDischarge = as.Date("2023-01-30"),
    stringsAsFactors = FALSE
  )
  isolate_df <- data.frame(
    RecordId = c("ISO-1", "ISO-2"), ParentId = c("PAT-1", "PAT-1"),
    DateOfSpecCollection = as.Date(c("2023-01-03", "2023-01-10")), # 7 days apart
    MicroorganismCode = c("RP-1", "RP-2"),
    stringsAsFactors = FALSE
  )
  commensal_df <- data.frame(SNOMED.Code = "CC-1", Organism = "Commensal", stringsAsFactors = FALSE)

  episodes <- calculateEpisodes(patient_df, isolate_df, commensal_df, episodeDuration = 14)

  expect_equal(nrow(episodes), 2)
  expect_false(all(episodes$Polymicrobial))
})

test_that("aggregateEpisodes correctly counts and aggregates episodes", {
  # Sample episodes data (output from calculateEpisodes)
  eps_df <- data.frame(
    HospitalId = c("HOSP-A", "HOSP-A", "HOSP-B", "HOSP-B", "HOSP-A"),
    EpisodeClass = c("CA", "HO-HA", "CA", "IMP-HA", "HO-HA"),
    EpisodeId = paste0("EPI-", 1:5),
    stringsAsFactors = FALSE
  )

  # Sample ehrbsi data
  ehrbsi <- data.frame(
    RecordId = c("HOSP-A", "HOSP-B"),
    RecordType = "EHRBSI",
    RecordTypeVersion = NA_character_,
    Subject = "EHRBSI",
    Status = "New/Update",
    DataSource = "XX-EHRBSI",
    ReportingCountry = "XX",
    DateUsedForStatistics = 2023,
    HospitalId = c("HOSP-A", "HOSP-B"),
    LaboratoryCode = NA_character_,
    GeoLocation = NA_character_,
    HospitalSize = NA_real_,
    HospitalType = NA_character_,
    ESurvBSI = NA,
    AggregationLevel = "HOSP",
    EpisodeDuration = 14,
    ClinicalTerminology = NA_character_,
    ClinicalTerminologySpec = NA_character_,
    MicrobiologicalTerminology = "SNOMED-CT",
    MicrobiologicalTerminologySpec = NA_character_,
    NumberOfBloodCultureSets = NA_real_,
    NumberOfHospitalDischarges = NA_real_,
    NumberOfHospitalPatientDays = NA_real_,
    ProportionPopulationCovered = NA_real_,
    NumberOfCABSIs = 0,
    NumberOfTotalBSIs = 0,
    NumberOfHOHABSIs = 0,
    NumberOfImportedHABSIs = 0,
    stringsAsFactors = FALSE
  )

  # Aggregate the episodes
  aggregated_df <- aggregateEpisodes(eps_df, ehrbsi)

  # Check HOSP-A
  hosp_a_counts <- aggregated_df[aggregated_df$RecordId == "HOSP-A", ]
  expect_equal(hosp_a_counts$NumberOfCABSIs, 1)
  expect_equal(hosp_a_counts$NumberOfHOHABSIs, 2)
  expect_equal(hosp_a_counts$NumberOfImportedHABSIs, 0)
  expect_equal(hosp_a_counts$NumberOfTotalBSIs, 3)

  # Check HOSP-B
  hosp_b_counts <- aggregated_df[aggregated_df$RecordId == "HOSP-B", ]
  expect_equal(hosp_b_counts$NumberOfCABSIs, 1)
  expect_equal(hosp_b_counts$NumberOfHOHABSIs, 0)
  expect_equal(hosp_b_counts$NumberOfImportedHABSIs, 1)
  expect_equal(hosp_b_counts$NumberOfTotalBSIs, 2)
})