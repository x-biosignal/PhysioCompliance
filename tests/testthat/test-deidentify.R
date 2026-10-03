test_that("Safe Harbor candidate policy handles all 18 configured categories", {
  original <- make_deidentification_experiment()
  raw_before <- serialize(
    SummarizedExperiment::assay(original, "raw"),
    NULL,
    version = 3L,
    xdr = TRUE
  )
  policy <- safeHarborPolicy(
    free_text = "drop",
    biometric_data = "drop"
  )
  result <- deidentify(original, policy)
  report <- S4Vectors::metadata(result)$deidentification

  expect_s4_class(result, "PhysioExperiment")
  expect_s3_class(report, "deidentification_report")
  expect_setequal(unique(report$field_actions$category), c(
    "names", "substate_geography", "dates_ages", "telephone", "fax",
    "email", "ssn", "medical_record", "health_plan", "account",
    "certificate_license", "vehicle", "device", "url", "ip_address",
    "biometric", "full_face_image", "other_unique"
  ))
  expect_identical(
    serialize(
      SummarizedExperiment::assay(result, "raw"),
      NULL,
      version = 3L,
      xdr = TRUE
    ),
    raw_before
  )
  expect_identical(SummarizedExperiment::assayNames(result), "raw")
  expect_false("patient_name" %in% names(SummarizedExperiment::colData(result)))
  expect_identical(
    SummarizedExperiment::colData(result)$age,
    c("89", "90_or_older", "90_or_older")
  )
  expect_identical(S4Vectors::metadata(result)$birth_date, "2000")
  expect_false("city" %in% names(S4Vectors::metadata(result)))
  expect_false("comments" %in% names(S4Vectors::metadata(result)))
  expect_false("photo" %in% names(S4Vectors::metadata(result)))

  serialized_report <- serialize(report, NULL, version = 3L, xdr = TRUE)
  expect_false(raw_contains(
    serialized_report,
    charToRaw("participant-raw")
  ))
  expect_false(raw_contains(
    serialized_report,
    charToRaw("Example City")
  ))
  printed <- paste(capture.output(print(report)), collapse = "\n")
  expect_match(printed, "policy=safe_harbor_candidate", fixed = TRUE)
  expect_false(grepl("participant-raw", printed, fixed = TRUE))

  audit <- auditDeidentification(result, policy)
  expect_identical(audit$status, "manual_review")
  expect_false(any(audit$findings$severity == "error"))
  expect_true("BIOMETRIC_REVIEW" %in% audit$findings$rule_id)
})

test_that("residual fields produce stable category-specific rule IDs", {
  policy <- safeHarborPolicy(
    free_text = "drop",
    biometric_data = "drop"
  )
  clean <- deidentify(make_deidentification_experiment(), policy)
  cases <- list(
    names = list(alias = "patient_name", value = "remaining",
                 rule = "DIRECT_IDENTIFIER"),
    substate_geography = list(alias = "city", value = "remaining",
                              rule = "SUBSTATE_GEOGRAPHY"),
    dates_ages = list(alias = "age", value = 97,
                      rule = "AGE_OVER_89"),
    telephone = list(alias = "phone", value = "remaining",
                     rule = "DIRECT_IDENTIFIER"),
    fax = list(alias = "fax", value = "remaining",
               rule = "DIRECT_IDENTIFIER"),
    email = list(alias = "email", value = "remaining",
                 rule = "DIRECT_IDENTIFIER"),
    ssn = list(alias = "ssn", value = "remaining",
               rule = "DIRECT_IDENTIFIER"),
    medical_record = list(alias = "mrn", value = "remaining",
                          rule = "DIRECT_IDENTIFIER"),
    health_plan = list(alias = "health_plan_id", value = "remaining",
                       rule = "DIRECT_IDENTIFIER"),
    account = list(alias = "account_number", value = "remaining",
                   rule = "DIRECT_IDENTIFIER"),
    certificate_license = list(alias = "license_number", value = "remaining",
                               rule = "DIRECT_IDENTIFIER"),
    vehicle = list(alias = "vehicle_id", value = "remaining",
                   rule = "DIRECT_IDENTIFIER"),
    device = list(alias = "device_id", value = "remaining",
                  rule = "DIRECT_IDENTIFIER"),
    url = list(alias = "url", value = "remaining",
               rule = "DIRECT_IDENTIFIER"),
    ip_address = list(alias = "ip_address", value = "remaining",
                      rule = "DIRECT_IDENTIFIER"),
    biometric = list(alias = "fingerprint", value = "remaining",
                     rule = "BIOMETRIC_REVIEW"),
    full_face_image = list(alias = "photo", value = "remaining",
                           rule = "IMAGE_REVIEW"),
    other_unique = list(alias = "participant_id", value = "remaining",
                        rule = "DIRECT_IDENTIFIER")
  )

  for (category in names(cases)) {
    changed <- clean
    metadata <- S4Vectors::metadata(changed)
    metadata[[cases[[category]]$alias]] <- cases[[category]]$value
    S4Vectors::metadata(changed) <- metadata
    audit <- auditDeidentification(changed, policy)
    matching <- audit$findings[
      audit$findings$category == category &
        audit$findings$rule_id == cases[[category]]$rule,
      ,
      drop = FALSE
    ]
    expect_gt(nrow(matching), 0L)
  }
})

test_that("dates, ages, factors, and explicit removal policies are safe", {
  x <- PhysioCore::PhysioExperiment(
    assays = list(raw = matrix(1:4, 2)),
    colData = S4Vectors::DataFrame(
      age = factor(c("89", "90")),
      admission_date = factor(c("2024-02-29", NA))
    ),
    samplingRate = 10
  )
  year <- deidentify(
    x,
    safeHarborPolicy(free_text = "drop", biometric_data = "drop")
  )
  expect_identical(
    SummarizedExperiment::colData(year)$age,
    c("89", "90_or_older")
  )
  expect_identical(
    SummarizedExperiment::colData(year)$admission_date,
    c("2024", NA_character_)
  )

  removed <- deidentify(
    x,
    safeHarborPolicy(
      date_action = "remove",
      free_text = "drop",
      biometric_data = "drop"
    )
  )
  expect_false(
    "admission_date" %in% names(SummarizedExperiment::colData(removed))
  )

  bad <- x
  SummarizedExperiment::colData(bad)$admission_date <-
    factor(c("not-a-date", "2024-01-01"))
  expect_error(
    deidentify(
      bad,
      safeHarborPolicy(free_text = "drop", biometric_data = "drop")
    ),
    "invalid person-related date"
  )
  SummarizedExperiment::colData(bad)$admission_date <-
    factor(c("2024-02-30", "2024-01-01"))
  expect_error(
    deidentify(
      bad,
      safeHarborPolicy(free_text = "drop", biometric_data = "drop")
    ),
    "invalid person-related date"
  )
})

test_that("pseudonymized policy replaces subject IDs and shifts dates", {
  key <- as.raw(1:32)
  bundle <- pseudonymize("source-subject", key, "study-a")
  x <- PhysioCore::PhysioExperiment(
    assays = list(raw = matrix(1:4, 2)),
    metadata = list(
      subject_id = "source-subject",
      measurement_date = as.Date("2024-02-29")
    ),
    samplingRate = 10
  )
  policy <- pseudonymizedPolicy(free_text = "drop")
  result <- deidentify(
    x,
    policy,
    subject_id = "source-subject",
    pseudonymization = bundle,
    date_key = key
  )
  metadata <- S4Vectors::metadata(result)

  expect_identical(metadata$subject_id, bundle$values)
  expect_s3_class(metadata$measurement_date, "Date")
  expect_false(identical(
    metadata$measurement_date,
    as.Date("2024-02-29")
  ))
  expect_identical(
    dateShift(
      metadata$measurement_date,
      "source-subject",
      key,
      inverse = TRUE
    ),
    as.Date("2024-02-29")
  )
  report <- metadata$deidentification
  expect_identical(report$policy_label, "pseudonymized")
  expect_identical(report$pseudonym_algorithm, "hmac_sha256")
  expect_identical(report$key_fingerprint, bundle$key_fingerprint)
  expect_false(any(names(metadata) %in% c("encrypted_map", "key")))

  audit <- auditDeidentification(result, policy)
  expect_false(any(audit$findings$severity == "error"))

  changed <- result
  S4Vectors::metadata(changed)$subject_id <- "not-a-token"
  expect_true("PSEUDONYM_LINK" %in%
    auditDeidentification(changed, policy)$findings$rule_id)
})

test_that("initialized audit chains are validated and linked", {
  x <- initializeAuditTrail(
    make_deidentification_experiment(),
    actor = "operator",
    timestamp = fixed_time()
  )
  policy <- safeHarborPolicy(
    free_text = "drop",
    biometric_data = "drop"
  )
  result <- deidentify(x, policy, audit_actor = "privacy-officer")
  state <- compliance_state(result)

  expect_true(verifyAuditTrail(result)$valid)
  expect_identical(
    state$audit[[length(state$audit)]]$action,
    "physio_compliance.deidentify"
  )
  expect_identical(
    state$audit[[length(state$audit)]]$details$policy_digest,
    S4Vectors::metadata(result)$deidentification$policy_digest
  )
  event_text <- paste(capture.output(str(
    state$audit[[length(state$audit)]]
  )), collapse = "\n")
  expect_false(grepl("participant-raw|Example City", event_text))
  expect_false(any(
    auditDeidentification(result, policy)$findings$rule_id == "AUDIT_LINK"
  ))

  expect_error(
    deidentify(
      initializeAuditTrail(
        make_deidentification_experiment(),
        actor = "operator",
        timestamp = fixed_time()
      ),
      policy
    ),
    "audit_actor"
  )
  tampered <- x
  state <- compliance_state(tampered)
  state$audit[[1L]]$entry_hash <- strrep("0", 64)
  tampered <- set_compliance_state(tampered, state)
  expect_error(
    deidentify(tampered, policy, audit_actor = "privacy-officer"),
    "invalid"
  )
})

test_that("multi-rate and longitudinal classes preserve structure and order", {
  make_child <- function(id) {
    PhysioCore::PhysioExperiment(
      assays = list(raw = matrix(1:4, 2)),
      metadata = list(patient_id = id),
      samplingRate = 10
    )
  }
  policy <- safeHarborPolicy(
    free_text = "drop",
    biometric_data = "drop"
  )
  multi <- PhysioCore::MultiRatePhysioExperiment(
    slow = initializeAuditTrail(
      make_child("a"), actor = "operator", timestamp = fixed_time()
    ),
    fast = initializeAuditTrail(
      make_child("a"), actor = "operator", timestamp = fixed_time()
    )
  )
  multi_result <- deidentify(
    multi, policy, audit_actor = "privacy-officer"
  )
  expect_s4_class(multi_result, "MultiRatePhysioExperiment")
  expect_identical(names(multi_result@streams), c("slow", "fast"))
  expect_s3_class(
    attr(multi_result, "deidentification"),
    "deidentification_report"
  )
  expect_true(all(vapply(
    as.list(multi_result@streams),
    function(stream) {
      !("patient_id" %in% names(S4Vectors::metadata(stream)))
    },
    logical(1)
  )))
  expect_false(any(
    auditDeidentification(multi_result, policy)$findings$rule_id ==
      "AUDIT_LINK"
  ))

  longitudinal <- PhysioCore::PhysioLongitudinal(
    baseline = make_child("a"),
    followup = make_child("a"),
    design = S4Vectors::DataFrame(
      session_id = c("baseline", "followup"),
      visit_label = c("baseline", "followup"),
      days_from_baseline = c(0, 30),
      condition = c("usual", "usual")
    ),
    subject = S4Vectors::DataFrame(id = "a", dx = "stroke")
  )
  long_result <- deidentify(longitudinal, policy)
  expect_s4_class(long_result, "PhysioLongitudinal")
  expect_identical(names(long_result@sessions), c("baseline", "followup"))
  expect_identical(as.character(long_result@design$session_id),
                   c("baseline", "followup"))
  expect_false("id" %in% names(long_result@subject))
  expect_s3_class(
    attr(long_result, "deidentification"),
    "deidentification_report"
  )
  audit <- auditDeidentification(long_result, policy)
  expect_false(any(audit$findings$severity == "error"))
})

test_that("policies and traversal reject ambiguity and unsupported state", {
  expect_error(
    safeHarborPolicy(additional_fields = list(unknown = "field")),
    "unknown category"
  )
  expect_error(
    safeHarborPolicy(additional_fields = list(names = "patient_name")),
    "unique after normalization"
  )
  expect_error(
    safeHarborPolicy(additional_fields = list(
      names = "site-field",
      email = "site_field"
    )),
    "unique after normalization"
  )
  malformed_policy <- safeHarborPolicy()
  malformed_policy$action[[1L]] <- "retain"
  expect_error(
    deidentify(make_deidentification_experiment(), malformed_policy),
    "not a supported"
  )

  x <- PhysioCore::PhysioExperiment(
    assays = list(raw = matrix(1:4, 2)),
    metadata = list(unsafe = new.env(parent = emptyenv())),
    samplingRate = 10
  )
  expect_error(
    deidentify(
      x,
      safeHarborPolicy(free_text = "drop", biometric_data = "drop")
    ),
    "x@metadata\\$unsafe"
  )
  S4Vectors::metadata(x)$unsafe <- stats::as.formula("value ~ subject")
  expect_error(
    deidentify(
      x,
      safeHarborPolicy(free_text = "drop", biometric_data = "drop")
    ),
    "unsupported object graph"
  )

  clean <- deidentify(
    make_deidentification_experiment(),
    safeHarborPolicy(free_text = "drop", biometric_data = "drop")
  )
  expect_error(
    deidentify(
      clean,
      safeHarborPolicy(free_text = "drop", biometric_data = "drop")
    ),
    "already has"
  )
})

test_that("free-text and biometric handling require explicit choices", {
  x <- make_deidentification_experiment()
  expect_error(deidentify(x), "biometric|free text")
  expect_error(
    deidentify(
      x,
      safeHarborPolicy(free_text = "drop", biometric_data = "error")
    ),
    "biometric"
  )

  key <- raw(32)
  bundle <- pseudonymize("subject", key, "study")
  y <- PhysioCore::PhysioExperiment(
    assays = list(raw = matrix(1:4, 2)),
    metadata = list(subject_id = "subject", comments = "review me"),
    samplingRate = 10
  )
  retained <- deidentify(
    y,
    pseudonymizedPolicy(free_text = "retain"),
    pseudonymization = bundle
  )
  expect_identical(S4Vectors::metadata(retained)$comments, "review me")
  expect_true(any(grepl(
    "Retained free text",
    S4Vectors::metadata(retained)$deidentification$manual_review$reason,
    fixed = TRUE
  )))
})
