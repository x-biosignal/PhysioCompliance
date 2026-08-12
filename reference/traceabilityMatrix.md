# Construct a normalized requirement-risk-control-test traceability graph

Construct a normalized requirement-risk-control-test traceability graph

## Usage

``` r
traceabilityMatrix(
  requirements,
  risks,
  controls,
  tests,
  links,
  evidence_root = NULL,
  risk_matrix = NULL
)
```

## Arguments

- requirements, risks, controls, tests, links:

  Plain data frames using the schemas documented in
  `vignette("PhysioCompliance")` and the package reference.

- evidence_root:

  Optional evidence directory. Evidence URIs remain relative to this
  root.

- risk_matrix:

  Optional project-owned
  [`riskMatrix()`](https://x-biosignal.github.io/PhysioCompliance/reference/riskMatrix.md).

## Value

A deterministic `traceability_matrix` object.
