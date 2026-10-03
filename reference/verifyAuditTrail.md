# Verify a compliance audit trail

Verification reports tampering and malformed storage without modifying,
truncating, repairing, or rehashing the supplied object.

## Usage

``` r
verifyAuditTrail(x)
```

## Arguments

- x:

  A
  [PhysioExperiment::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/PhysioExperiment.html)
  object.

## Value

A `compliance_verification` object containing validity, deterministic
issues, the verified head hash, current record hash, and event counts.

## Details

A self-contained object cannot distinguish an internally consistent
earlier copy from the record as it existed at that time. Detect
whole-object rollback or removal of the current tail event by retaining
each verified `head_hash` in a validated external append-only store and
comparing it on retrieval.

## Examples

``` r
x <- PhysioExperiment::PhysioExperiment(
  assays = list(raw = matrix(1:6, nrow = 3)),
  samplingRate = 100
)
x <- initializeAuditTrail(x, actor = "operator-01")
verifyAuditTrail(x)
#> <compliance_verification> valid; 1 event; 0 signatures
```
