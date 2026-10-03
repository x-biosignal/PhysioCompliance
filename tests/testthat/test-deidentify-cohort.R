# De-identification of a PhysioCohort (multi-subject container).

test_that("deidentify handles a PhysioCohort: subjects + subject-level colData", {
  s1 <- make_deidentification_experiment()
  s2 <- make_deidentification_experiment()
  coh <- PhysioCore::PhysioCohort(
    subjects = list(subjA = s1, subjB = s2),
    colData = S4Vectors::DataFrame(
      subject_id = c("subjA", "subjB"),
      patient_name = c("Yamada Taro", "Suzuki Hanako"),   # subject-level PII
      age = c(34, 71)))
  policy <- safeHarborPolicy(free_text = "drop", biometric_data = "drop")

  result <- deidentify(coh, policy)
  expect_s4_class(result, "PhysioCohort")
  methods::validObject(result)                            # invariant preserved

  # the merged report lives in the cohort metadata slot
  report <- result@metadata[["deidentification"]]
  expect_s3_class(report, "deidentification_report")

  # subject-level PII (a name) is scrubbed from the cohort colData
  expect_false("patient_name" %in% names(result@colData))

  # both subjects were de-identified end-to-end (each PE lost its PII)
  expect_length(result@subjects, 2L)
  pe1 <- result@subjects[[1]]@sessions[[1]]
  expect_false("patient_name" %in% names(SummarizedExperiment::colData(pe1)))

  # idempotence guard: re-running a de-identified cohort errors
  expect_error(deidentify(result, policy), "already has")
})
