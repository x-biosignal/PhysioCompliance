# De-identify PhysioCore records

Applies an explicit field policy while preserving the input S4 class.
Assays are never inspected for identifying signal content; retained
assays are therefore listed for manual review. Reports contain paths and
counts, never removed values, keys, or re-identification material.

## Usage

``` r
deidentify(
  x,
  policy = safeHarborPolicy(),
  subject_id = NULL,
  pseudonymization = NULL,
  date_key = NULL,
  audit_actor = NULL,
  audit_reason = "record de-identified"
)
```

## Arguments

- x:

  A `PhysioExperiment`, `MultiRatePhysioExperiment`,
  `PhysioLongitudinal`, or `PhysioCohort` (a multi-subject container
  whose subject-level `colData` and every subject timeline are
  de-identified; unlike MultiRate/Longitudinal it has a `metadata` slot,
  so its merged report is stored there).

- policy:

  A policy from
  [`safeHarborPolicy()`](https://x-biosignal.github.io/PhysioCompliance/reference/safeHarborPolicy.md)
  or
  [`pseudonymizedPolicy()`](https://x-biosignal.github.io/PhysioCompliance/reference/safeHarborPolicy.md).

- subject_id:

  A scalar or field-aligned subject identifier used only for keyed date
  shifting.

- pseudonymization:

  A protected result from
  [`pseudonymize()`](https://x-biosignal.github.io/PhysioCompliance/reference/pseudonymize.md)
  used to replace direct identifiers.

- date_key:

  A caller-owned raw key of at least 32 bytes.

- audit_actor:

  Responsible actor when an input audit trail is initialized.

- audit_reason:

  Non-empty reason stored in linked audit events.

## Value

A modified object of the same S4 class.

## Details

`MultiRatePhysioExperiment` and `PhysioLongitudinal` do not provide a
metadata slot. Their aggregate report is stored as a serializable
`deidentification` attribute, while every child `PhysioExperiment` keeps
its report in `metadata()`. Existing audit trails are linked per child.
