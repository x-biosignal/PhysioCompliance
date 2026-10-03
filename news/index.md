# Changelog

## PhysioCompliance 0.4.2

### Documentation

- A vignette carries one task end to end on synthetic or bundled data,
  offline, and is built and run by `R CMD check`.
- Runnable `@examples` added or corrected across 20 help pages. Each
  runs offline in seconds, writes nothing outside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html), and is executed
  by `R CMD check`; anything needing a device, a download or an optional
  backend is fenced with the reason stated.
- The README’s quick start runs as written: it attaches the package,
  builds its own inputs, and uses only hard dependencies.

## PhysioCompliance 0.4.1

- De-identification and data-subject handling accept the canonical
  `MultiPhysioExperiment`. They previously tested for
  `MultiRatePhysioExperiment` only, which would have rejected a
  container built by the current constructor.

## PhysioCompliance 0.4.0

- [`deidentify()`](https://x-biosignal.github.io/PhysioCompliance/reference/deidentify.md)
  now handles `PhysioCohort` (the multi-subject container): it
  de-identifies each subject timeline and the subject-level `colData`
  (where cohort PII concentrates), and stores the merged report in the
  cohort `metadata` slot. The structural `subject_id` linkage key is
  replaced with a stable positional pseudonym so the container stays
  valid and internally consistent while the real identifier is removed.
  Previously
  [`deidentify()`](https://x-biosignal.github.io/PhysioCompliance/reference/deidentify.md)
  aborted on a `PhysioCohort`.

## PhysioCompliance 0.3.0

- Add deterministic lifecycle and risk-management template rendering
  with bundled source-edition metadata and verified template
  inventories.
- Add project-owned risk matrices and normalized
  requirement-risk-control-test traceability with objective-evidence
  hashing.
- Add read-only ecosystem engineering-readiness checks and deterministic
  JSON, CSV, and Markdown reports.

## PhysioCompliance 0.2.0

- Add conservative Safe Harbor candidate policies and structured
  de-identification auditing.
- Add keyed pseudonymization, authenticated encrypted re-identification
  maps, and deterministic within-subject date shifting.
- Add structured header scrubbing and plan-first data-subject workflow
  helpers.

## PhysioCompliance 0.1.0

- Add deterministic SHA-256 audit chains over complete
  `PhysioExperiment` records.
- Add externally authenticated electronic signatures bound to record
  state and audit-chain heads.
- Add structured verification reports for audit and signature failures.
