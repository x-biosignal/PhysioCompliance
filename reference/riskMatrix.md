# Define a project-owned risk decision matrix

The constructor records caller-supplied ordinal levels and one explicit
decision for every severity/probability pair. It does not multiply ranks
or infer a risk-acceptability policy.

## Usage

``` r
riskMatrix(severity, probability, decisions, matrix_id, version, rationale)
```

## Arguments

- severity:

  Severity levels with `level_id`, `rank`, `label`, and `definition`.

- probability:

  Probability levels with the same columns.

- decisions:

  One explicit decision for every Cartesian level pair.

- matrix_id, version, rationale:

  Non-empty project identifiers and policy rationale.

## Value

A deterministic `risk_matrix` object.
