# Extract the compliance audit trail

Extract the compliance audit trail

## Usage

``` r
auditTrail(x)
```

## Arguments

- x:

  A
  [PhysioExperiment::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/PhysioExperiment.html)
  object.

## Value

An `audit_trail` data frame with a `details` list-column. An
uninitialized object returns a zero-row trail.

## Examples

``` r
x <- PhysioExperiment::PhysioExperiment(
  assays = list(raw = matrix(1:6, nrow = 3)),
  samplingRate = 100
)
x <- initializeAuditTrail(x, actor = "operator-01")
auditTrail(x)
#> <audit_trail> 1 event
#>  sequence                   timestamp       actor                 action
#>         1 2026-10-03T13:15:14.042502Z operator-01 initialize_audit_trail
#>                   reason
#>  audit trail initialized
#>                                                       record_hash
#>  2426dc5f51bb6e484e0cd143aec1431ecc216cb7465d975cbb4f85c6462a1333
#>                                                     previous_hash
#>  0000000000000000000000000000000000000000000000000000000000000000
#>                                                        entry_hash
#>  490c19dd3f091069cfea9cb4d651132f2e1aa6c5435868b000f24215089c8769
#>  hash_algorithm serialization_version  details
#>          sha256                     3 <list:2>
```
