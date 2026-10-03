# Define PhysioExperiment project conformance criteria

Thresholds are project policies for an engineering-readiness check. They
are not regulatory thresholds or clauses of a standard.

## Usage

``` r
conformanceCriteria(example_floor = 0.5, coverage_floor = 0.8)
```

## Arguments

- example_floor:

  Minimum fraction of documented exports with examples.

- coverage_floor:

  Minimum caller-supplied line coverage.

## Value

A `conformance_criteria` data frame.

## Examples

``` r
criteria <- conformanceCriteria()
criteria[, c("criterion_id", "default_applicability", "threshold")]
#>                  criterion_id default_applicability threshold
#> 1           DESCRIPTION_VALID                  TRUE        NA
#> 2        NAMESPACE_DOCUMENTED                  TRUE        NA
#> 3            EXAMPLE_COVERAGE                  TRUE       0.5
#> 4           REFERENCE_PRESENT                 FALSE        NA
#> 5               TESTS_PRESENT                  TRUE       1.0
#> 6               TEST_COVERAGE                  TRUE       0.8
#> 7        PROVENANCE_READINESS                 FALSE       1.0
#> 8             AUDIT_READINESS                 FALSE       1.0
#> 9  DEIDENTIFICATION_READINESS                 FALSE       1.0
#> 10            EXTERNAL_CHECKS                  TRUE       1.0
```
