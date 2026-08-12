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
