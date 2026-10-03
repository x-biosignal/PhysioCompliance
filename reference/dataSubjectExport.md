# Prepare a data-subject access export

Selects records through a caller-owned identity matcher and returns deep
copies with a content manifest. Identity verification, the scope and
format of an Article 15 response, third-party rights, and secure
delivery remain controller responsibilities.

## Usage

``` r
dataSubjectExport(records, subject_id, locate)
```

## Arguments

- records:

  A named plain list of records.

- subject_id:

  One requested subject identifier.

- locate:

  A callback called exactly once per record. It returns one subject
  identifier or `NA`.

## Value

A `data_subject_export` object.

## Examples

``` r
records <- list(
  visit_01 = list(subject = "S1", note = "baseline"),
  visit_02 = list(subject = "S2", note = "baseline")
)
# `locate` is a caller-owned identity matcher called once per record.
dataSubjectExport(records, "S1", locate = function(record) record$subject)
#> <data_subject_export> records=1; manifest=1
```
