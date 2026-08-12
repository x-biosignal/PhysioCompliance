# Shift dates consistently within subject

Derives one deterministic integer-day offset per subject using keyed
HMAC-SHA-256. Exact elapsed intervals are preserved. Full shifted dates
are pseudonymized personal data, not a HIPAA Safe Harbor output.

## Usage

``` r
dateShift(
  dates,
  subject_id,
  key,
  range_days = c(-3650L, 3650L),
  inverse = FALSE
)
```

## Arguments

- dates:

  A `Date` or `POSIXct` vector.

- subject_id:

  One subject ID or a vector aligned to `dates`.

- key:

  Raw vector of at least 32 cryptographically random bytes.

- range_days:

  Inclusive integer offset range.

- inverse:

  Apply the exact negative keyed offset.

## Value

A vector with the same length and date class.
