# Plan or apply a data-subject erasure

Builds a no-content plan before authorization. Applying the plan changes
only the returned in-memory collection; it does not delete files,
databases, backups, replicas, caches, or remote systems. Identity
verification, applicable legal bases, Article 17 exceptions, retention
obligations, and operational deletion remain controller
responsibilities.

## Usage

``` r
dataSubjectErase(
  records,
  subject_id,
  locate,
  authorize,
  retain = NULL,
  reason,
  mode = c("plan", "apply")
)
```

## Arguments

- records:

  A named plain list of records.

- subject_id:

  One requested subject identifier.

- locate:

  A callback called exactly once per record.

- authorize:

  A callback invoked exactly once in apply mode with the plan.

- retain:

  Optional callback returning a non-empty retention reason or
  `NULL`/`NA` for erasure.

- reason:

  Non-empty operational reason. It must not contain the subject
  identifier.

- mode:

  Build a no-op plan or apply an authorized plan.

## Value

A `data_subject_erasure` plan or applied result.
