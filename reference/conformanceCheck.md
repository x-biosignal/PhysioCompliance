# Run a read-only ecosystem engineering-readiness audit

This function statically parses package metadata and source. It does not
load namespaces, execute package code, run tests, or launch external
tools.

## Usage

``` r
conformanceCheck(
  root,
  packages = NULL,
  criteria = conformanceCriteria(),
  coverage = NULL,
  external_results = NULL,
  applicability = NULL,
  checked_at = Sys.time()
)
```

## Arguments

- root:

  Repository or package root.

- packages:

  Optional exact package-name filter.

- criteria:

  Project criteria from
  [`conformanceCriteria()`](https://x-biosignal.github.io/PhysioCompliance/reference/conformanceCriteria.md).

- coverage:

  Caller-supplied coverage evidence or `NULL`.

- external_results:

  Caller-supplied external check evidence or `NULL`.

- applicability:

  Reviewed applicability decisions or `NULL`.

- checked_at:

  Fixed check timestamp.

## Value

A deterministic `conformance_report`.
