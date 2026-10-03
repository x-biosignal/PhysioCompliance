# Append an audit event

Appends an event to a valid audit chain. A changed record may be
appended when its historical chain remains valid; the new event then
establishes the changed record as the current audited state.

## Usage

``` r
appendAuditEvent(
  x,
  action,
  actor,
  reason,
  details = list(),
  timestamp = Sys.time()
)
```

## Arguments

- x:

  An initialized
  [PhysioExperiment::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/PhysioExperiment.html)
  object.

- action:

  Non-empty domain action. Names beginning with `physio_compliance.` are
  reserved.

- actor:

  Non-empty identifier for the responsible actor.

- reason:

  Non-empty reason for the action.

- details:

  A recursively named plain list containing supported atomic values.
  Named lists are sorted before hashing.

- timestamp:

  One finite `POSIXct` value.

## Value

A modified copy of `x` with one additional event.

## Examples

``` r
x <- PhysioExperiment::PhysioExperiment(
  assays = list(raw = matrix(1:6, nrow = 3)),
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
```
