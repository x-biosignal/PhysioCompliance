# Evidence, privacy, and readiness controls with PhysioCompliance

PhysioCompliance supplies deterministic technical controls for
physiological records: tamper-evident audit trails, externally
authenticated electronic signatures, explicit de-identification and
keyed pseudonymization, and project-owned risk, traceability and
engineering-readiness records.

These are **technical controls only**. The package does not certify that
an installation, study, organization, or submission complies with 21 CFR
Part 11, HIPAA, GDPR, IEC 62304, ISO 14971, or any other framework, and
it does not decide device status, risk acceptability, or release.
Everything below runs on synthetic data and writes only into
[`tempdir()`](https://rdrr.io/r/base/tempfile.html).

``` r

library(PhysioCompliance)
```

## 1. A tamper-evident audit trail

Initialize a versioned SHA-256 audit chain over a record, then append
events. Verification reports tampering without modifying the object.

``` r

x <- PhysioExperiment::PhysioExperiment(
  assays = list(raw = matrix(as.double(1:12), nrow = 4)),
  samplingRate = 100
)
x <- initializeAuditTrail(x, actor = "operator-01")
x <- appendAuditEvent(
  x,
  action = "quality_control",
  actor = "operator-01",
  reason = "reviewed acquisition quality"
)
verifyAuditTrail(x)
#> <compliance_verification> valid; 2 events; 0 signatures
as.data.frame(auditTrail(x))[, c("sequence", "actor", "action")]
#>   sequence       actor                 action
#> 1        1 operator-01 initialize_audit_trail
#> 2        2 operator-01        quality_control
```

## 2. An externally authenticated signature

Signing and verification are delegated to caller-supplied callbacks. The
package never accepts passwords, private keys, or reusable secrets. The
illustrative callback below simply hashes the challenge; a real
deployment authenticates the signer and uses its own credential service,
which determines cryptographic strength and non-repudiation.

``` r

credential <- list(id = "credential-01", algorithm = "demo-sha256",
                   fingerprint = "01:23:45:67")
sign_bytes <- function(challenge, credential) {
  digest::digest(c(challenge, serialize(credential, NULL, version = 3)),
                 algo = "sha256", serialize = FALSE, raw = TRUE)
}
x <- eSign(x, signer = "reviewer-02", meaning = "reviewed",
           credential = credential, sign = sign_bytes)

verifyESignatures(x, verify = function(challenge, signature, credential) {
  identical(signature, sign_bytes(challenge, credential))
})$valid
#> [1] TRUE
```

## 3. De-identification

[`safeHarborPolicy()`](https://x-biosignal.github.io/PhysioCompliance/reference/safeHarborPolicy.md)
builds a conservative field policy. The `safe_harbor_candidate` label is
a configured-field result, not a legal determination; the applicable
organization still owes the no-actual-knowledge condition and a manual
review of free text, images, and signals.

``` r

rec <- PhysioExperiment::PhysioExperiment(
  assays = list(raw = matrix(as.double(1:6), nrow = 3)),
  colData = S4Vectors::DataFrame(patient_name = c("A", "B")),
  samplingRate = 100
)
clean <- deidentify(rec, safeHarborPolicy())
auditDeidentification(clean)$status
#> [1] "manual_review"
```

## 4. Keyed pseudonymization

Keys are caller-owned raw vectors of at least 32 random bytes. The
returned object carries tokens and an authenticated encrypted map, never
the plaintext map or the key.

``` r

key <- openssl::rand_bytes(32)
p <- pseudonymize(c("subject-a", "subject-b", "subject-a"), key,
                  namespace = "study-example")
p$values
#> [1] "psn_5bff675b24d76d671bdfed35eb892389"
#> [2] "psn_10cdec92619262df174236b4e33fb551"
#> [3] "psn_5bff675b24d76d671bdfed35eb892389"
identical(reidentify(p, key), c("subject-a", "subject-b", "subject-a"))
#> [1] TRUE
```

Date shifting applies one deterministic per-subject offset while
preserving the exact elapsed intervals between visits:

``` r

visits <- as.Date(c("2026-01-05", "2026-02-02", "2026-03-09"))
shifted <- dateShift(visits, subject_id = "subject-a", key = key)
all(diff(shifted) == diff(visits))
#> [1] TRUE
```

## 5. Project-owned risk and traceability

[`riskMatrix()`](https://x-biosignal.github.io/PhysioCompliance/reference/riskMatrix.md)
records one explicit decision for every severity/probability pair; it
never multiplies ranks or supplies an acceptability threshold.

``` r

severity <- data.frame(
  level_id = c("S1", "S2"), rank = 1:2,
  label = c("Minor", "Serious"),
  definition = c("Limited impact", "Significant impact"),
  stringsAsFactors = FALSE
)
probability <- data.frame(
  level_id = c("P1", "P2"), rank = 1:2,
  label = c("Unlikely", "Likely"),
  definition = c("Rarely occurs", "Often occurs"),
  stringsAsFactors = FALSE
)
decisions <- expand.grid(
  severity_id = severity$level_id, probability_id = probability$level_id,
  stringsAsFactors = FALSE
)
decisions$decision <- c("acceptable", "review_required",
                        "review_required", "unacceptable")
decisions$rationale <- paste0("POLICY-", seq_len(nrow(decisions)))
rm <- riskMatrix(severity, probability, decisions,
                 matrix_id = "RM01", version = "1.0",
                 rationale = "Project-owned decision policy")
```

[`traceabilityMatrix()`](https://x-biosignal.github.io/PhysioCompliance/reference/traceabilityMatrix.md)
normalizes a requirement-risk-control-test graph with objective-evidence
hashes, and
[`validateTraceability()`](https://x-biosignal.github.io/PhysioCompliance/reference/validateTraceability.md)
reports orphans, uncontrolled risks, untested controls, and
evidence-hash mismatches.

``` r

evidence_root <- file.path(tempdir(), "pc-evidence")
dir.create(evidence_root, showWarnings = FALSE)
evidence_file <- file.path(evidence_root, "evidence-1.txt")
writeBin(charToRaw("objective evidence 1\n"), evidence_file)
evidence_hash <- digest::digest(evidence_file, algo = "sha256", file = TRUE)

requirements <- data.frame(
  requirement_id = "REQ1", title = "Requirement 1",
  description = "Project requirement 1", source = "project",
  source_version = "1", software_safety_class = "unclassified",
  status = "implemented", stringsAsFactors = FALSE
)
risks <- data.frame(
  risk_id = "RSK1", hazard = "Hazard 1", foreseeable_sequence = "Sequence 1",
  hazardous_situation = "Situation 1", harm = "Harm 1",
  initial_severity = "S2", initial_probability = "P2",
  initial_decision = "unacceptable",
  residual_severity = "S1", residual_probability = "P1",
  residual_decision = "acceptable",
  benefit_risk_required = FALSE, acceptance_rationale = "Project review 1",
  status = "accepted", stringsAsFactors = FALSE
)
controls <- data.frame(
  control_id = "CTL1", control_type = "inherent_safety",
  description = "Control 1", implementation_status = "verified",
  implementation_evidence = NA_character_, evidence_sha256 = NA_character_,
  stringsAsFactors = FALSE
)
tests <- data.frame(
  test_id = "TST1", level = "unit", description = "Test 1",
  expected_result = "Project criterion met", status = "pass",
  evidence_uri = basename(evidence_file), evidence_sha256 = evidence_hash,
  executed_at = "2026-07-28T03:04:01.000000Z", executor = "operator-01",
  stringsAsFactors = FALSE
)
links <- rbind(
  data.frame(source_type = "requirement", source_id = "REQ1",
             target_type = "risk", target_id = "RSK1",
             link_type = "traces_to", rationale = "Requirement-risk trace",
             stringsAsFactors = FALSE),
  data.frame(source_type = "risk", source_id = "RSK1",
             target_type = "control", target_id = "CTL1",
             link_type = "mitigates", rationale = "Risk-control trace",
             stringsAsFactors = FALSE),
  data.frame(source_type = "control", source_id = "CTL1",
             target_type = "test", target_id = "TST1",
             link_type = "verifies", rationale = "Control-test trace",
             stringsAsFactors = FALSE)
)

trace <- traceabilityMatrix(requirements, risks, controls, tests, links,
                            evidence_root = evidence_root, risk_matrix = rm)
validateTraceability(trace)$valid
#> [1] TRUE
```

## 6. Engineering-readiness check

[`conformanceCheck()`](https://x-biosignal.github.io/PhysioCompliance/reference/conformanceCheck.md)
statically parses package metadata and source. It does not load
namespaces, run tests, or launch external tools; missing execution
evidence is `NOT_EVALUATED`, never a pass. An engineering-readiness
report is not a regulatory conformity or release conclusion.

``` r

pkg <- file.path(tempdir(), "SyntheticPkg")
dir.create(file.path(pkg, "R"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(pkg, "man"), showWarnings = FALSE)
dir.create(file.path(pkg, "tests", "testthat"), recursive = TRUE,
           showWarnings = FALSE)
writeLines(c(
  "Package: SyntheticPkg", "Version: 0.1.0",
  "Title: Synthetic Example Package",
  "Description: A synthetic package used only to illustrate the readiness check.",
  "License: MIT",
  "Authors@R: person('Pat', 'Example', role = c('aut', 'cre'), email = 'pat@example.org')"
), file.path(pkg, "DESCRIPTION"))
writeLines("export(syntheticAnalysis)", file.path(pkg, "NAMESPACE"))
writeLines("syntheticAnalysis <- function(x) x",
           file.path(pkg, "R", "synthetic.R"))
writeLines(c(
  "\\name{syntheticAnalysis}", "\\alias{syntheticAnalysis}",
  "\\title{Synthetic analysis}", "\\description{Synthetic analysis.}",
  "\\usage{syntheticAnalysis(x)}",
  "\\arguments{\\item{x}{An object.}}", "\\value{An object.}",
  "\\examples{syntheticAnalysis(NULL)}"
), file.path(pkg, "man", "syntheticAnalysis.Rd"))
writeLines("test_that('synthetic', { expect_true(TRUE) })",
           file.path(pkg, "tests", "testthat", "test-synthetic.R"))

report <- conformanceCheck(pkg)
report
#> <conformance_report>
#>   engineering readiness check
#>   root id: SyntheticPkg 
#>   checked at: 2026-10-03T13:15:21.005252Z 
#>   packages: 1 
#>   PASS: 4 
#>   FAIL: 0 
#>   NOT_EVALUATED: 2 
#>   report hash: 43c36601b9e206b47c2a0c18d1a23d02ca8632bb774978150223ae0df9390104 
#>   This report records project engineering-readiness evidence; it is not a conformity conclusion.
out <- writeConformanceReport(report, file.path(tempdir(), "conformance.json"))
basename(out)
#> [1] "conformance.json"
```

## Where to go next

[`?PhysioCompliance`](https://x-biosignal.github.io/PhysioCompliance/reference/PhysioCompliance-package.md)
lists every entry point. Audit-trail detection is in-object only: to
detect whole-object rollback, retain each verified `head_hash` in a
validated external append-only store and compare it on retrieval.
