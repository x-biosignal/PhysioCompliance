# Audit a stored de-identification transformation

Rescans supported metadata and validates the stored report. A passing
result means that this checker found no configured residual identifier;
it is not a HIPAA, GDPR, Safe Harbor, or file-format compliance
determination.

## Usage

``` r
auditDeidentification(x, policy = NULL)
```

## Arguments

- x:

  A supported de-identified PhysioCore record.

- policy:

  Optional effective policy. When omitted, a built-in policy matching
  the stored label is used.

## Value

A list containing status, findings, severity counts, and check time.

## Examples

``` r
x <- PhysioExperiment::PhysioExperiment(
  assays = list(raw = matrix(1:6, nrow = 3)),
  colData = S4Vectors::DataFrame(patient_name = c("A", "B")),
  samplingRate = 100
)
clean <- deidentify(x, safeHarborPolicy())
# A pass means no configured residual identifier was found; it is not a
# HIPAA, GDPR, or Safe Harbor determination.
auditDeidentification(clean)$status
#> [1] "manual_review"
```
