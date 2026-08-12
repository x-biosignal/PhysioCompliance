# PhysioCompliance

PhysioCompliance provides:

- tamper-evident audit trails and externally authenticated signatures;
- conservative field-based de-identification with residual scanning;
- keyed pseudonymization and deterministic within-subject date shifting;
- structured EDF/BDF, BrainVision, SNIRF, and DICOM-adjacent header
  scrubbing;
- plan-first data-subject export and erasure helpers;
- original project-owned lifecycle and risk-document templates;
- normalized requirement-risk-control-test traceability with evidence
  hashes;
- read-only ecosystem engineering-readiness checks.

The package supplies technical controls. It does not certify that an
installation, study, organization, or submission complies with 21 CFR
Part 11 or ALCOA+, and it does not determine HIPAA or GDPR compliance.
Regulated use also requires validated systems, access controls, identity
proofing, credential lifecycle management, policies, training,
retention, and other operational controls.

The lifecycle and risk functions are documentation and
evidence-management aids. They do not determine whether software is a
medical device, assign a software safety class, decide whether risk is
acceptable, authorize release, or establish conformity with IEC 62304,
ISO 14971, an FDA guidance, a quality-system regulation, or a
Bioconductor policy. The bundled templates use original neutral headings
and blank project schemas; they do not reproduce licensed standard
clauses.

Signing and verification are performed by caller-supplied callbacks.
Passwords, private keys, API tokens, and reusable secrets must remain in
the deployment’s credential service and are never accepted for storage
by this package.

An internally consistent earlier copy cannot be distinguished from the
record as it existed at that time. To detect whole-object rollback or
removal of the current tail event, retain each verified `head_hash` in a
validated external append-only store and compare it during retrieval.
The in-object chain detects edits, insertions, removals before the head,
reordering, and current-record changes; it is not an external timestamp
or trusted archive.

## Installation

``` r

install.packages(
  "PhysioCompliance",
  repos = c(
    "https://x-biosignal.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

## Basic use

``` r

library(PhysioCore)
library(PhysioCompliance)

x <- PhysioExperiment(
  assays = list(raw = matrix(1:12, nrow = 4)),
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
```

Electronic-signature callbacks must authenticate and authorize the
signer outside R. Their algorithm and credential service determine
cryptographic strength, identity assurance, and non-repudiation
properties.

## De-identification

[`safeHarborPolicy()`](https://x-biosignal.github.io/PhysioCompliance/reference/safeHarborPolicy.md)
covers the 18 configured identifier categories and labels its output
`safe_harbor_candidate`. That label is deliberately narrower than a
legal conclusion: the applicable organization must still establish the
required no-actual-knowledge condition and review unclassified metadata,
free text, images, and potentially identifying physiological signals.

``` r

policy <- safeHarborPolicy(
  free_text = "drop",
  biometric_data = "drop"
)
deidentified <- deidentify(x, policy)
auditDeidentification(deidentified, policy)
```

The report stores field paths, categories, actions, and counts. It never
stores removed values, value-derived hashes, keys, or a
re-identification map. Assays are not inspected to infer that their
signal content is non-identifying; retained signal data remain a
manual-review item.

`MultiRatePhysioExperiment` and `PhysioLongitudinal` have no metadata
slot. Each child `PhysioExperiment` stores its own report and audit
link; a serializable aggregate report is attached to the outer object as
a `deidentification` attribute. Subject/design changes on the outer
longitudinal container cannot be linked to a child audit event and are
reported for manual review.

## Pseudonymization

Keys are caller-owned raw vectors of at least 32 cryptographically
random bytes. Character passwords are rejected. Independent HMAC-derived
subkeys are used for tokens, map encryption, and map authentication. The
protected map uses an AES-256-GCM ciphertext plus an independently
derived HMAC-SHA-256 envelope authentication tag.

``` r

key <- openssl::rand_bytes(32)
mapping <- pseudonymize(
  c("subject-a", "subject-b"),
  key,
  namespace = "deployment-specific-study"
)
identical(
  reidentify(mapping, key),
  c("subject-a", "subject-b")
)
```

The encrypted map remains sensitive additional information. Store it
separately from pseudonymized records with deployment access controls
and an explicit key rotation, backup, and recovery policy. Pseudonymized
data remain personal data when the additional information can restore
identity. Full date shifting is a pseudonymization operation and is not
emitted by
[`safeHarborPolicy()`](https://x-biosignal.github.io/PhysioCompliance/reference/safeHarborPolicy.md).

## Structured Headers

[`headerScrub()`](https://x-biosignal.github.io/PhysioCompliance/reference/headerScrub.md)
accepts a parsed named list or data-frame header. It does not parse or
rewrite EDF/BDF, BrainVision, SNIRF, or DICOM files. DICOM-adjacent mode
replaces UIDs consistently within one call, but it does not inspect
pixel data, private attributes, overlays, or burned-in annotations and
does not produce a DICOM PS3.15 conformance statement. Those locations
always remain manual-review items.

## Data-Subject Workflows

[`dataSubjectExport()`](https://x-biosignal.github.io/PhysioCompliance/reference/dataSubjectExport.md)
and
[`dataSubjectErase()`](https://x-biosignal.github.io/PhysioCompliance/reference/dataSubjectErase.md)
keep storage and identity matching in caller-supplied callbacks. Erasure
defaults to `mode = "plan"` and invokes the authorization callback
exactly once only in apply mode. Apply mode changes the returned
in-memory named list; it does not delete files, databases, backups,
replicas, caches, or remote systems.

Identity verification, controller decisions, legal bases, response
content, exceptions, retention requirements, secure delivery, and
operational deletion remain outside the package.

## Interpretation Baseline

Version 0.3.0 pins its field policy and tests to the bundled version-1
alias dictionary and the following public references. The package does
not fetch regulatory or format content at runtime and does not reproduce
standards text.

- HHS OCR, *Guidance Regarding Methods for De-identification of
  Protected Health Information in Accordance with the HIPAA Privacy
  Rule*:
  <https://www.hhs.gov/hipaa/for-professionals/special-topics/de-identification/>
- Regulation (EU) 2016/679, including Articles 4(5), 15, and 17:
  <https://eur-lex.europa.eu/eli/reg/2016/679/oj>
- DICOM PS3.15, Security and System Management Profiles:
  <https://dicom.nema.org/medical/dicom/current/output/html/part15.html>
- MNE `mne.io.anonymize_info()` behavior used only as an optional
  comparison:
  <https://mne.tools/stable/generated/mne.io.anonymize_info.html>

## Lifecycle, Risk, and Traceability

[`lifecycleTemplate()`](https://x-biosignal.github.io/PhysioCompliance/reference/lifecycleTemplate.md)
and
[`riskManagementTemplate()`](https://x-biosignal.github.io/PhysioCompliance/reference/riskManagementTemplate.md)
render deterministic project records after verifying every shipped
template hash. The default software safety class is `unclassified`;
A/B/C values are recorded only when supplied by the project and remain
pending review.

[`riskMatrix()`](https://x-biosignal.github.io/PhysioCompliance/reference/riskMatrix.md)
requires one explicit caller decision for every severity and probability
pair. It never multiplies ordinal ranks or supplies a universal
acceptable-risk threshold.
[`traceabilityMatrix()`](https://x-biosignal.github.io/PhysioCompliance/reference/traceabilityMatrix.md)
normalizes stable IDs and directed links, while
[`validateTraceability()`](https://x-biosignal.github.io/PhysioCompliance/reference/validateTraceability.md)
reports orphan requirements, uncontrolled risks, untested controls,
failed tests, path escapes, and objective-evidence hash mismatches
without editing the graph.

``` r

sources <- standardsSources(as_of = as.Date("2026-07-28"))
sources[, c("source_id", "edition", "review_status")]
```

## Engineering Readiness

[`conformanceCheck()`](https://x-biosignal.github.io/PhysioCompliance/reference/conformanceCheck.md)
uses [`read.dcf()`](https://rdrr.io/r/base/dcf.html), R/Rd parsers, and
raw-byte source fingerprints. It does not load package namespaces, run
examples or tests, execute startup files, fetch URLs, or invoke R CMD
check, BiocCheck, or `covr`. Coverage and external results are accepted
only as explicit caller-supplied evidence tied to the exact source hash.
Missing or stale execution evidence is `NOT_EVALUATED`, never a pass.

[`writeConformanceReport()`](https://x-biosignal.github.io/PhysioCompliance/reference/writeConformanceReport.md)
writes deterministic JSON, CSV, or Markdown. Reports contain project
criteria, statuses, run identifiers, and relative evidence paths, not
source or evidence contents. An engineering-readiness report is not a
regulatory conformity or release conclusion.
