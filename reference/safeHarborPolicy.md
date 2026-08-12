# Conservative de-identification policies

Constructs a versioned field policy. These policies are technical
controls, not legal determinations. A `safe_harbor_candidate` still
requires the no-actual-knowledge and organizational review required by
the applicable workflow. Pseudonymized data remain personal data when
separately held information permits re-identification.

## Usage

``` r
safeHarborPolicy(
  additional_fields = list(),
  date_action = c("year", "remove"),
  free_text = c("error", "drop"),
  biometric_data = c("error", "drop")
)

pseudonymizedPolicy(
  additional_fields = list(),
  free_text = c("error", "drop", "retain")
)
```

## Arguments

- additional_fields:

  A named plain list mapping canonical category IDs to
  deployment-specific aliases.

- date_action:

  Replace person-related dates with year-only values or remove them.

- free_text:

  Refuse, drop, or (for pseudonymized data only) retain detected free
  text.

- biometric_data:

  Refuse or drop detected biometric/image content.

## Value

A `deidentification_policy` data frame.
