# Write a deterministic conformance report

Write a deterministic conformance report

## Usage

``` r
writeConformanceReport(
  x,
  path,
  format = c("json", "csv", "markdown"),
  overwrite = FALSE
)
```

## Arguments

- x:

  A `conformance_report`.

- path:

  Destination path.

- format:

  JSON, CSV, or Markdown output.

- overwrite:

  Whether to replace existing output atomically.

## Value

Normalized output path or paths, invisibly.
