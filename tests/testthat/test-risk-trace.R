test_that("complete traceability graph is deterministic and evidence-backed", {
  root <- local_tempdir()
  fixture <- ws1037_trace_fixture(root, n = 12L)
  trace <- ws1037_make_trace(fixture, root)
  verification <- validateTraceability(trace)
  expect_s3_class(trace, "traceability_matrix")
  expect_s3_class(verification, "traceability_verification")
  expect_true(verification$valid)
  expect_true(verification$complete)
  expect_true(verification$evidence_checked)
  expect_equal(nrow(trace$paths), 12L)
  expect_true(all(trace$paths$path_status == "complete"))
  expect_identical(as.data.frame(trace), trace$paths)
  expect_identical(as.data.frame(verification), verification$issues)

  set.seed(37)
  shuffled <- fixture
  for (name in c("requirements", "risks", "controls", "tests", "links")) {
    shuffled[[name]] <- shuffled[[name]][
      sample(nrow(shuffled[[name]])), , drop = FALSE
    ]
  }
  shuffled$risk_matrix <- riskMatrix(
    fixture$risk_matrix$severity[sample(4), ],
    fixture$risk_matrix$probability[sample(5), ],
    fixture$risk_matrix$decisions[sample(20), ],
    fixture$risk_matrix$matrix_id,
    fixture$risk_matrix$version,
    fixture$risk_matrix$rationale
  )
  trace_shuffled <- ws1037_make_trace(shuffled, root)
  expect_identical(trace$content_hash, trace_shuffled$content_hash)
  expect_identical(trace$paths, trace_shuffled$paths)
  expect_match(paste(capture.output(print(trace)), collapse = "\n"),
               "paths: 12")
  expect_match(
    paste(capture.output(print(verification)), collapse = "\n"),
    "traceability complete: TRUE"
  )
})

test_that("traceability constructor rejects policy and graph invention", {
  root <- local_tempdir()
  fixture <- ws1037_trace_fixture(root)
  risks <- fixture$risks
  risks$initial_decision <- "not_evaluated"
  risks$residual_decision <- "not_evaluated"
  risks$status <- "controlled"
  risks$acceptance_rationale <- NA_character_
  no_matrix <- traceabilityMatrix(
    fixture$requirements, risks, fixture$controls, fixture$tests,
    fixture$links, evidence_root = root
  )
  expect_null(no_matrix$risk_matrix)
  expect_true(all(no_matrix$risks$initial_decision == "not_evaluated"))
  expect_true(all(no_matrix$risks$residual_decision == "not_evaluated"))

  conflicting <- fixture$risks
  conflicting$residual_decision[[1]] <- "review_required"
  expect_error(
    traceabilityMatrix(
      fixture$requirements, conflicting, fixture$controls, fixture$tests,
      fixture$links, risk_matrix = fixture$risk_matrix
    ),
    "conflicts"
  )
  accepted_without_matrix <- risks
  accepted_without_matrix$status[[1]] <- "accepted"
  expect_error(
    traceabilityMatrix(
      fixture$requirements, accepted_without_matrix, fixture$controls,
      fixture$tests, fixture$links
    ),
    "Accepted or closed"
  )
  dangling <- fixture$links
  dangling$target_id[[1]] <- "RSK999"
  expect_error(
    traceabilityMatrix(
      fixture$requirements, fixture$risks, fixture$controls, fixture$tests,
      dangling, risk_matrix = fixture$risk_matrix
    ),
    "dangling"
  )
  duplicate <- rbind(fixture$links, fixture$links[1, ])
  expect_error(
    traceabilityMatrix(
      fixture$requirements, fixture$risks, fixture$controls, fixture$tests,
      duplicate, risk_matrix = fixture$risk_matrix
    ),
    "duplicated"
  )
})

test_that("traceability verification reports stable completeness rules", {
  root <- local_tempdir()
  fixture <- ws1037_trace_fixture(root)
  trace <- ws1037_make_trace(fixture, root)

  tampered <- trace
  tampered$links <- tampered$links[
    !(tampered$links$source_type == "control" &
        tampered$links$source_id == "CTL1"),
    ,
    drop = FALSE
  ]
  rownames(tampered$links) <- NULL
  tampered$paths <- PhysioCompliance:::.pc_trace_paths(
    tampered$requirements, tampered$risks, tampered$controls,
    tampered$tests, tampered$links
  )
  tampered$content_hash <- PhysioCompliance:::.pc_hash(
    PhysioCompliance:::.pc_trace_content(tampered)
  )
  finding <- validateTraceability(tampered, verify_evidence = FALSE)
  expect_false(finding$complete)
  expect_true("CONTROL_UNVERIFIED" %in% finding$issues$rule_id)

  changed_hash <- trace
  changed_hash$requirements$title[[1]] <- "changed"
  finding <- validateTraceability(changed_hash, verify_evidence = FALSE)
  expect_false(finding$valid)
  expect_true("CONTENT_HASH" %in% finding$issues$rule_id)

  failed <- trace
  failed$tests$status[[1]] <- "fail"
  failed$content_hash <- PhysioCompliance:::.pc_hash(
    PhysioCompliance:::.pc_trace_content(failed)
  )
  finding <- validateTraceability(failed, verify_evidence = FALSE)
  expect_true("FAILED_TEST" %in% finding$issues$rule_id)
  expect_false(finding$complete)

  forged_paths <- trace
  forged_paths$paths$path_status[[1]] <- "forged"
  forged_paths$content_hash <- PhysioCompliance:::.pc_hash(
    PhysioCompliance:::.pc_trace_content(forged_paths)
  )
  finding <- validateTraceability(forged_paths, verify_evidence = FALSE)
  expect_false(finding$valid)
  expect_true("TRACE_SCHEMA" %in% finding$issues$rule_id)

  forged_level <- trace
  forged_level$risks$initial_severity[[1]] <- "S999"
  forged_level$content_hash <- PhysioCompliance:::.pc_hash(
    PhysioCompliance:::.pc_trace_content(forged_level)
  )
  finding <- validateTraceability(forged_level, verify_evidence = FALSE)
  expect_false(finding$valid)
  expect_true("RISK_MATRIX_LEVEL" %in% finding$issues$rule_id)

  forged_decision <- trace
  forged_decision$risks$initial_decision[[1]] <- "acceptable"
  forged_decision$content_hash <- PhysioCompliance:::.pc_hash(
    PhysioCompliance:::.pc_trace_content(forged_decision)
  )
  finding <- validateTraceability(forged_decision, verify_evidence = FALSE)
  expect_false(finding$valid)
  expect_true("RISK_DECISION" %in% finding$issues$rule_id)

  malformed_status <- trace
  malformed_status$risks$status[[1]] <- NA_character_
  malformed_status$content_hash <- PhysioCompliance:::.pc_hash(
    PhysioCompliance:::.pc_trace_content(malformed_status)
  )
  finding <- expect_no_error(
    validateTraceability(malformed_status, verify_evidence = FALSE)
  )
  expect_false(finding$valid)
  expect_true("TRACE_SCHEMA" %in% finding$issues$rule_id)

  malformed_root <- trace
  malformed_root$evidence_root$path_hash <- "not-a-hash"
  malformed_root$content_hash <- PhysioCompliance:::.pc_hash(
    PhysioCompliance:::.pc_trace_content(malformed_root)
  )
  finding <- expect_no_error(validateTraceability(malformed_root))
  expect_false(finding$valid)
  expect_true("TRACE_SCHEMA" %in% finding$issues$rule_id)
})

test_that("objective evidence detects mutation and path escape", {
  root <- local_tempdir()
  fixture <- ws1037_trace_fixture(root)
  trace <- ws1037_make_trace(fixture, root)
  writeBin(charToRaw("mutated\n"), fixture$evidence[[1]])
  finding <- validateTraceability(trace)
  expect_false(finding$valid)
  expect_true("EVIDENCE_HASH" %in% finding$issues$rule_id)

  fixture <- ws1037_trace_fixture(file.path(root, "fresh"))
  escaped <- fixture
  escaped$tests$evidence_uri[[1]] <- "../outside.txt"
  trace <- ws1037_make_trace(escaped, file.path(root, "fresh"))
  finding <- validateTraceability(trace)
  expect_true("EVIDENCE_PATH_ESCAPE" %in% finding$issues$rule_id)
  expect_false(finding$valid)

  missing_root <- file.path(root, "missing-root")
  fixture <- ws1037_trace_fixture(missing_root)
  trace <- ws1037_make_trace(fixture, missing_root)
  unlink(missing_root, recursive = TRUE)
  finding <- expect_no_error(validateTraceability(trace))
  expect_true("EVIDENCE_MISSING" %in% finding$issues$rule_id)
  expect_false(finding$complete)
})

test_that("derivation cycles and factors are rejected", {
  root <- local_tempdir()
  fixture <- ws1037_trace_fixture(root)
  extra <- data.frame(
    source_type = c("requirement", "requirement"),
    source_id = c("REQ1", "REQ2"),
    target_type = c("requirement", "requirement"),
    target_id = c("REQ2", "REQ1"),
    link_type = "derives",
    rationale = "derivation",
    stringsAsFactors = FALSE
  )
  expect_error(
    traceabilityMatrix(
      fixture$requirements, fixture$risks, fixture$controls, fixture$tests,
      rbind(fixture$links, extra), risk_matrix = fixture$risk_matrix
    ),
    "cycle"
  )
  factored <- fixture$requirements
  factored$status <- factor(factored$status)
  expect_error(
    traceabilityMatrix(
      factored, fixture$risks, fixture$controls, fixture$tests,
      fixture$links, risk_matrix = fixture$risk_matrix
    ),
    "plain data frame"
  )
})

test_that("empty canonical graph has a stable zero-row path schema", {
  root <- local_tempdir()
  fixture <- ws1037_trace_fixture(root, n = 1L)
  empty <- lapply(
    fixture[c("requirements", "risks", "controls", "tests", "links")],
    function(table) table[0, , drop = FALSE]
  )
  trace <- traceabilityMatrix(
    empty$requirements, empty$risks, empty$controls, empty$tests,
    empty$links
  )
  expect_identical(
    names(trace$paths),
    c("requirement_id", "risk_id", "control_id", "test_id", "path_status")
  )
  expect_equal(nrow(trace$paths), 0L)
  finding <- validateTraceability(trace)
  expect_true(finding$valid)
  expect_true(finding$complete)
})

test_that("evidence roots and path components reject symbolic links", {
  skip_on_os("windows")
  parent <- local_tempdir()
  real <- file.path(parent, "real")
  linked <- file.path(parent, "linked")
  dir.create(real)
  skip_if_not(file.symlink(real, linked), "symbolic links unavailable")
  fixture <- ws1037_trace_fixture(real, n = 1L)
  expect_error(
    ws1037_make_trace(fixture, linked),
    "symbolic link"
  )
})
