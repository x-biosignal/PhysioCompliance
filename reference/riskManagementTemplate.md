# Render a project-owned risk-management template set

Render a project-owned risk-management template set

## Usage

``` r
riskManagementTemplate(
  out_dir,
  project,
  intended_use,
  owner,
  effective_date = Sys.Date(),
  risk_matrix = NULL,
  source_editions = standardsSources(),
  overwrite = FALSE
)
```

## Arguments

- out_dir:

  Destination child directory.

- project:

  Project name. Path separators are not allowed.

- intended_use:

  Intended-use statement supplied by the project.

- owner:

  Recorded document owner.

- effective_date:

  Effective date to render.

- risk_matrix:

  Optional project-owned
  [`riskMatrix()`](https://x-biosignal.github.io/PhysioCompliance/reference/riskMatrix.md)
  object to record.

- source_editions:

  Metadata returned by
  [`standardsSources()`](https://x-biosignal.github.io/PhysioCompliance/reference/standardsSources.md).

- overwrite:

  Whether to replace an existing destination atomically.

## Value

A `lifecycle_template` manifest.
