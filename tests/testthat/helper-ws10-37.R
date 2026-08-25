local_tempdir <- function() {
  path <- tempfile("physio-compliance-test-")
  dir.create(path)
  path
}

ws1037_risk_matrix <- function() {
  severity <- data.frame(
    level_id = c("S1", "S2", "S3", "S4"),
    rank = 1:4,
    label = c("Minimal", "Minor", "Serious", "Critical"),
    definition = paste("Project severity", 1:4),
    stringsAsFactors = FALSE
  )
  probability <- data.frame(
    level_id = paste0("P", 1:5),
    rank = 1:5,
    label = c("Rare", "Unlikely", "Possible", "Likely", "Frequent"),
    definition = paste("Project probability", 1:5),
    stringsAsFactors = FALSE
  )
  decisions <- expand.grid(
    severity_id = severity$level_id,
    probability_id = probability$level_id,
    stringsAsFactors = FALSE
  )
  score <- match(decisions$severity_id, severity$level_id) +
    match(decisions$probability_id, probability$level_id)
  decisions$decision <- ifelse(
    score <= 4, "acceptable",
    ifelse(score <= 6, "review_required", "unacceptable")
  )
  decisions$rationale <- paste0("POLICY-", seq_len(nrow(decisions)))
  riskMatrix(
    severity, probability, decisions,
    matrix_id = "RM01", version = "1.0",
    rationale = "Project-owned decision policy"
  )
}

ws1037_trace_fixture <- function(evidence_root, n = 4L) {
  dir.create(evidence_root, recursive = TRUE, showWarnings = FALSE)
  evidence <- file.path(evidence_root, paste0("evidence-", seq_len(n), ".txt"))
  for (i in seq_along(evidence)) {
    writeBin(charToRaw(paste0("objective evidence ", i, "\n")), evidence[[i]])
  }
  hashes <- vapply(
    evidence,
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE
  )
  requirements <- data.frame(
    requirement_id = paste0("REQ", seq_len(n)),
    title = paste("Requirement", seq_len(n)),
    description = paste("Project requirement", seq_len(n)),
    source = "project",
    source_version = "1",
    software_safety_class = "unclassified",
    status = "implemented",
    stringsAsFactors = FALSE
  )
  risks <- data.frame(
    risk_id = paste0("RSK", seq_len(n)),
    hazard = paste("Hazard", seq_len(n)),
    foreseeable_sequence = paste("Sequence", seq_len(n)),
    hazardous_situation = paste("Situation", seq_len(n)),
    harm = paste("Harm", seq_len(n)),
    initial_severity = "S4",
    initial_probability = "P5",
    initial_decision = "unacceptable",
    residual_severity = "S1",
    residual_probability = "P1",
    residual_decision = "acceptable",
    benefit_risk_required = FALSE,
    acceptance_rationale = paste("Project review", seq_len(n)),
    status = "accepted",
    stringsAsFactors = FALSE
  )
  controls <- data.frame(
    control_id = paste0("CTL", seq_len(n)),
    control_type = "inherent_safety",
    description = paste("Control", seq_len(n)),
    implementation_status = "verified",
    implementation_evidence = NA_character_,
    evidence_sha256 = NA_character_,
    stringsAsFactors = FALSE
  )
  tests <- data.frame(
    test_id = paste0("TST", seq_len(n)),
    level = rep(c("unit", "integration"), length.out = n),
    description = paste("Test", seq_len(n)),
    expected_result = "Project criterion met",
    status = "pass",
    evidence_uri = basename(evidence),
    evidence_sha256 = hashes,
    executed_at = sprintf(
      "2026-07-28T03:04:%02d.000000Z", seq_len(n)
    ),
    executor = "operator-01",
    stringsAsFactors = FALSE
  )
  links <- rbind(
    data.frame(
      source_type = "requirement",
      source_id = requirements$requirement_id,
      target_type = "risk",
      target_id = risks$risk_id,
      link_type = "traces_to",
      rationale = "Requirement-risk trace",
      stringsAsFactors = FALSE
    ),
    data.frame(
      source_type = "risk",
      source_id = risks$risk_id,
      target_type = "control",
      target_id = controls$control_id,
      link_type = "mitigates",
      rationale = "Risk-control trace",
      stringsAsFactors = FALSE
    ),
    data.frame(
      source_type = "control",
      source_id = controls$control_id,
      target_type = "test",
      target_id = tests$test_id,
      link_type = "verifies",
      rationale = "Control-test trace",
      stringsAsFactors = FALSE
    )
  )
  list(
    requirements = requirements,
    risks = risks,
    controls = controls,
    tests = tests,
    links = links,
    evidence = evidence,
    risk_matrix = ws1037_risk_matrix()
  )
}

ws1037_make_trace <- function(fixture, evidence_root) {
  traceabilityMatrix(
    fixture$requirements,
    fixture$risks,
    fixture$controls,
    fixture$tests,
    fixture$links,
    evidence_root = evidence_root,
    risk_matrix = fixture$risk_matrix
  )
}

ws1037_write_package <- function(
    path, package, documented = TRUE, calls = TRUE, malformed = FALSE) {
  dir.create(file.path(path, "R"), recursive = TRUE)
  dir.create(file.path(path, "man"), recursive = TRUE)
  dir.create(file.path(path, "tests", "testthat"), recursive = TRUE)
  writeLines(
    c(
      paste0("Package: ", package),
      "Version: 0.1.0",
      "Title: Synthetic Physiological Analysis Package",
      "Description: Reads participant files and returns a PhysioExperiment.",
      "License: MIT",
      "Authors@R: person('Yusuke', 'Matsui', role=c('aut','cre'), email='mail.to.matsui@gmail.com')",
      "Imports: PhysioCore, PhysioCompliance"
    ),
    file.path(path, "DESCRIPTION")
  )
  writeLines("export(syntheticAnalysis)", file.path(path, "NAMESPACE"))
  body <- if (calls) {
    c(
      "syntheticAnalysis <- function(x) {",
      "  x <- PhysioCore::recordProvenance(x)",
      "  x <- PhysioCompliance::initializeAuditTrail(x, actor='operator')",
      "  PhysioCompliance::deidentify(x, list())",
      "}"
    )
  } else {
    c(
      "syntheticAnalysis <- function(x) {",
      "  marker <- 'initializeAuditTrail deidentify recordProvenance'",
      "  x",
      "}"
    )
  }
  writeLines(body, file.path(path, "R", "synthetic.R"))
  if (documented) {
    writeLines(
      c(
        "\\name{syntheticAnalysis}",
        "\\alias{syntheticAnalysis}",
        "\\title{Synthetic analysis}",
        "\\description{Synthetic analysis for a test package.}",
        "\\usage{syntheticAnalysis(x)}",
        "\\arguments{\\item{x}{An object.}}",
        "\\value{An object.}",
        "\\references{Project-authored synthetic reference.}",
        "\\examples{syntheticAnalysis(NULL)}"
      ),
      file.path(path, "man", "syntheticAnalysis.Rd")
    )
  }
  test_text <- if (malformed) {
    "test_that('broken', {"
  } else {
    "test_that('synthetic', { expect_true(TRUE) })"
  }
  writeLines(
    test_text,
    file.path(path, "tests", "testthat", "test-synthetic.R")
  )
}
