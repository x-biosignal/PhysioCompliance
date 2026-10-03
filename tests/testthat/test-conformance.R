test_that("conformanceCriteria validates project thresholds", {
  criteria <- conformanceCriteria(0.6, 0.85)
  expect_s3_class(criteria, "conformance_criteria")
  expect_identical(criteria$criterion_id, PhysioCompliance:::.pc_criteria_ids)
  expect_true(all(criteria$policy_source == "physio-conformance-v1"))
  expect_equal(
    criteria$threshold[criteria$criterion_id == "EXAMPLE_COVERAGE"],
    0.6
  )
  expect_error(conformanceCriteria(-0.1), "\\[0, 1\\]")
  expect_error(conformanceCriteria(coverage_floor = Inf), "\\[0, 1\\]")

  root <- local_tempdir()
  ws1037_write_package(file.path(root, "physio-ecosystem", "PkgGood"), "PkgGood")
  forged <- criteria
  forged$policy_source[[1]] <- "invented-policy"
  expect_error(
    conformanceCheck(
      root, criteria = forged,
      checked_at = as.POSIXct("2026-07-28 03:04:05", tz = "UTC")
    ),
    "returned by conformanceCriteria"
  )
})

test_that("conformanceCheck produces a deterministic static truth table", {
  root <- local_tempdir()
  ecosystem <- file.path(root, "physio-ecosystem")
  good <- file.path(ecosystem, "PkgGood")
  strings <- file.path(ecosystem, "PkgStrings")
  ws1037_write_package(good, "PkgGood", documented = TRUE, calls = TRUE)
  ws1037_write_package(
    strings, "PkgStrings", documented = FALSE, calls = FALSE
  )
  fixed_time <- as.POSIXct("2026-07-28 03:04:05", tz = "UTC")
  first <- conformanceCheck(root, checked_at = fixed_time)
  expect_equal(nrow(first$packages), 2L)
  expect_equal(
    nrow(first$results),
    nrow(first$packages) * nrow(first$criteria)
  )
  expect_identical(first$packages$package, c("PkgGood", "PkgStrings"))

  reviewed <- expand.grid(
    package = first$packages$package,
    criterion_id = c(
      "REFERENCE_PRESENT", "PROVENANCE_READINESS", "AUDIT_READINESS",
      "DEIDENTIFICATION_READINESS"
    ),
    stringsAsFactors = FALSE
  )
  reviewed$applicable <- TRUE
  reviewed$rationale <- "Reviewed project applicability"
  reviewed$reviewer <- "reviewer-01"
  reviewed$reviewed_on <- "2026-07-28"
  coverage <- data.frame(
    package = first$packages$package,
    line_coverage = c(0.95, 0.10),
    tool = "covr",
    tool_version = "test",
    run_id = c("COV-GOOD", "COV-STALE"),
    executed_at = "2026-07-28T03:04:05.000000Z",
    source_hash = c(
      first$packages$source_hash[first$packages$package == "PkgGood"],
      paste(rep("0", 64), collapse = "")
    ),
    stringsAsFactors = FALSE
  )
  external <- data.frame(
    package = "PkgGood",
    tool = "R_CMD_check",
    tool_version = "4.5",
    run_id = "CHECK-GOOD",
    executed_at = "2026-07-28T03:04:05.000000Z",
    source_hash = first$packages$source_hash[
      first$packages$package == "PkgGood"
    ],
    errors = 0L,
    warnings = 0L,
    notes = 0L,
    status = "pass",
    evidence_uri = "checks/PkgGood.log",
    evidence_sha256 = paste(rep("1", 64), collapse = ""),
    stringsAsFactors = FALSE
  )
  report <- conformanceCheck(
    root,
    criteria = conformanceCriteria(0.5, 0.8),
    coverage = coverage,
    external_results = external,
    applicability = reviewed,
    checked_at = fixed_time
  )
  good_results <- report$results[report$results$package == "PkgGood", ]
  strings_results <- report$results[
    report$results$package == "PkgStrings", ]
  expect_true(all(
    good_results$status %in% c("PASS", "NOT_APPLICABLE")
  ))
  expect_identical(
    strings_results$status[
      strings_results$criterion_id == "TEST_COVERAGE"
    ],
    "NOT_EVALUATED"
  )
  expect_identical(
    strings_results$status[
      strings_results$criterion_id == "AUDIT_READINESS"
    ],
    "FAIL"
  )
  expect_identical(
    strings_results$status[
      strings_results$criterion_id == "NAMESPACE_DOCUMENTED"
    ],
    "FAIL"
  )
  repeated <- conformanceCheck(
    root,
    criteria = conformanceCriteria(0.5, 0.8),
    coverage = coverage[sample(nrow(coverage)), ],
    external_results = external,
    applicability = reviewed[sample(nrow(reviewed)), ],
    checked_at = fixed_time
  )
  expect_identical(report$report_hash, repeated$report_hash)
  expect_identical(report$results, repeated$results)
  expect_match(paste(capture.output(print(report)), collapse = "\n"),
               "engineering readiness check")
})

test_that("malformed package does not execute or stop other package results", {
  root <- local_tempdir()
  ecosystem <- file.path(root, "physio-ecosystem")
  good <- file.path(ecosystem, "PkgGood")
  broken <- file.path(ecosystem, "PkgBroken")
  ws1037_write_package(good, "PkgGood")
  ws1037_write_package(
    broken, "PkgBroken", documented = TRUE, calls = FALSE, malformed = TRUE
  )
  writeLines(
    "stop('package source must never execute')",
    file.path(broken, "R", "startup.R")
  )
  report <- conformanceCheck(
    root,
    checked_at = as.POSIXct("2026-07-28 03:04:05", tz = "UTC")
  )
  expect_equal(length(unique(report$results$package)), 2L)
  broken_tests <- report$results[
    report$results$package == "PkgBroken" &
      report$results$criterion_id == "TESTS_PRESENT",
  ]
  expect_identical(broken_tests$status, "FAIL")
  good_description <- report$results[
    report$results$package == "PkgGood" &
      report$results$criterion_id == "DESCRIPTION_VALID",
  ]
  expect_identical(good_description$status, "PASS")
})

test_that("quoted calls and strings are not counted as operation evidence", {
  root <- local_tempdir()
  package_path <- file.path(root, "physio-ecosystem", "PkgQuoted")
  ws1037_write_package(
    package_path, "PkgQuoted", documented = TRUE, calls = FALSE
  )
  writeLines(
    c(
      "quotedAudit <- function(x) {",
      "  marker <- 'initializeAuditTrail(x)'",
      "  palette <- grDevices::colorRampPalette(c('black', 'white'))(2)",
      "  quote(initializeAuditTrail(x))",
      "}"
    ),
    file.path(package_path, "R", "quoted.R")
  )
  report <- conformanceCheck(
    root,
    checked_at = as.POSIXct("2026-07-28 03:04:05", tz = "UTC")
  )
  result <- report$results[
    report$results$criterion_id == "AUDIT_READINESS", , drop = FALSE
  ]
  expect_identical(result$status, "FAIL")
})

test_that("report writers are deterministic, typed, and reject tampering", {
  root <- local_tempdir()
  package_path <- file.path(root, "physio-ecosystem", "PkgGood")
  ws1037_write_package(package_path, "PkgGood")
  report <- conformanceCheck(
    root,
    checked_at = as.POSIXct("2026-07-28 03:04:05", tz = "UTC")
  )
  out <- local_tempdir()
  json_path <- file.path(out, "report.json")
  csv_path <- file.path(out, "report.csv")
  md_path <- file.path(out, "report.md")
  first <- writeConformanceReport(report, json_path, "json")
  csv_files <- writeConformanceReport(report, csv_path, "csv")
  writeConformanceReport(report, md_path, "markdown")
  parsed <- jsonlite::fromJSON(first, simplifyVector = FALSE)
  expect_identical(parsed$schema_version, "1")
  expect_identical(parsed$report_hash, report$report_hash)
  expect_equal(length(parsed$results), nrow(report$results))
  expect_equal(length(csv_files), 2L)
  expect_true(all(file.exists(c(first, csv_files, md_path))))
  json_bytes <- readBin(first, "raw", n = file.info(first)$size)
  unlink(first)
  writeConformanceReport(report, json_path, "json")
  expect_identical(
    json_bytes,
    readBin(json_path, "raw", n = file.info(json_path)$size)
  )
  markdown <- paste(readLines(md_path, warn = FALSE), collapse = "\n")
  expect_match(markdown, "Engineering Readiness Check")
  expect_false(grepl("syntheticAnalysis <-", markdown, fixed = TRUE))
  expect_error(
    writeConformanceReport(report, json_path, "json"),
    "exists"
  )
  tampered <- report
  tampered$results$message[[1]] <- "changed"
  expect_error(
    writeConformanceReport(
      tampered, file.path(out, "tampered.json"), "json"
    ),
    "hash"
  )
})

test_that("reviewed applicability requires accountable rationale", {
  root <- local_tempdir()
  package_path <- file.path(root, "physio-ecosystem", "PkgGood")
  ws1037_write_package(package_path, "PkgGood")
  bad <- data.frame(
    package = "PkgGood",
    criterion_id = "REFERENCE_PRESENT",
    applicable = FALSE,
    rationale = "",
    reviewer = "reviewer",
    reviewed_on = "2026-07-28",
    stringsAsFactors = FALSE
  )
  expect_error(
    conformanceCheck(
      root,
      applicability = bad,
      checked_at = as.POSIXct("2026-07-28 03:04:05", tz = "UTC")
    ),
    "invalid reviewed"
  )
})

test_that("external evidence and report destinations reject path escape", {
  root <- local_tempdir()
  package_path <- file.path(root, "physio-ecosystem", "PkgGood")
  ws1037_write_package(package_path, "PkgGood")
  baseline <- conformanceCheck(
    root,
    checked_at = as.POSIXct("2026-07-28 03:04:05", tz = "UTC")
  )
  external <- data.frame(
    package = "PkgGood",
    tool = "R_CMD_check",
    tool_version = "4.5",
    run_id = "CHECK-ESCAPE",
    executed_at = "2026-07-28T03:04:05.000000Z",
    source_hash = baseline$packages$source_hash,
    errors = 0L,
    warnings = 0L,
    notes = 0L,
    status = "pass",
    evidence_uri = "../escape.log",
    evidence_sha256 = paste(rep("1", 64), collapse = ""),
    stringsAsFactors = FALSE
  )
  expect_error(
    conformanceCheck(
      root, external_results = external,
      checked_at = as.POSIXct("2026-07-28 03:04:05", tz = "UTC")
    ),
    "invalid execution evidence"
  )

  skip_on_os("windows")
  real <- local_tempdir()
  linked <- file.path(dirname(real), paste0(basename(real), "-link"))
  skip_if_not(file.symlink(real, linked), "symbolic links unavailable")
  expect_error(
    conformanceCheck(
      linked,
      checked_at = as.POSIXct("2026-07-28 03:04:05", tz = "UTC")
    ),
    "symbolic link"
  )
  expect_error(
    writeConformanceReport(baseline, file.path(linked, "report.json")),
    "non-symlink"
  )
})
