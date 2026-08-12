# Initialize a tamper-evident audit trail

Initializes a versioned SHA-256 audit chain over the complete
`PhysioExperiment` record. The genesis event snapshots the existing
PhysioCore provenance log. Existing compliance metadata is never
replaced.

## Usage

``` r
initializeAuditTrail(
  x,
  actor,
  reason = "audit trail initialized",
  timestamp = Sys.time()
)
```

## Arguments

- x:

  A
  [PhysioCore::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioCore/reference/PhysioExperiment.html)
  object.

- actor:

  Non-empty identifier for the responsible actor.

- reason:

  Non-empty reason for initialization.

- timestamp:

  One finite `POSIXct` value.

## Value

A modified copy of `x` with a genesis audit event.

## Examples

``` r
x <- PhysioCore::PhysioExperiment(
  assays = list(raw = matrix(1:6, nrow = 3)),
  samplingRate = 100
)
x <- initializeAuditTrail(x, actor = "operator-01")
verifyAuditTrail(x)
#> <compliance_verification> valid; 1 event; 0 signatures
```
