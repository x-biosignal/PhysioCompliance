# Read the bundled standards and guidance source metadata

Reads edition metadata recorded when this package was built. It performs
no network request and does not determine whether a source remains
applicable to a project.

## Usage

``` r
standardsSources(max_age_days = 365L, as_of = Sys.Date())
```

## Arguments

- max_age_days:

  Non-negative integer age after which source metadata is marked for
  review.

- as_of:

  Date used to calculate metadata age.

## Value

A `standards_sources` data frame.
