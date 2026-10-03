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

## Examples

``` r
severity <- data.frame(
  level_id = c("S1", "S2"), rank = 1:2,
  label = c("Minor", "Serious"),
  definition = c("Limited impact", "Significant impact"),
  stringsAsFactors = FALSE
)
probability <- data.frame(
  level_id = c("P1", "P2"), rank = 1:2,
  label = c("Unlikely", "Likely"),
  definition = c("Rarely occurs", "Often occurs"),
  stringsAsFactors = FALSE
)
decisions <- expand.grid(
  severity_id = severity$level_id, probability_id = probability$level_id,
  stringsAsFactors = FALSE
)
# One explicit project decision for every pair; ranks are never multiplied
# and no universal acceptability threshold is supplied.
decisions$decision <- c("acceptable", "review_required",
                        "review_required", "unacceptable")
decisions$rationale <- paste0("POLICY-", seq_len(nrow(decisions)))
riskMatrix(severity, probability, decisions,
           matrix_id = "RM01", version = "1.0",
           rationale = "Project-owned decision policy")
#> <risk_matrix>
#>   matrix: RM01 
#>   version: 1.0 
#>   severity levels: 2 
#>   probability levels: 2 
#>   explicit decisions: 4 
#>   matrix hash: 6df531d07ce4581081cc782ae59a46e4243eefb0c4759731671998b852fa8220 
```
