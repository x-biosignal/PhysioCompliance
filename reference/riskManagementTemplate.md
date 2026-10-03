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

## Examples

``` r
parent <- file.path(tempdir(), "pc-risk-demo")
dir.create(parent, showWarnings = FALSE)
tmpl <- riskManagementTemplate(
  out_dir = file.path(parent, "risk"),
  project = "Example Project",
  intended_use = "Research data processing",
  owner = "project-owner",
  effective_date = as.Date("2026-07-28")
)
tmpl
#> <lifecycle_template>
#>   template set: physio-lifecycle-v1 
#>   template type: risk_management 
#>   project: Example Project 
#>   recorded class: unclassified 
#>   classification review: pending 
#>   files: 7 
#>   content hash: 805b670a1f7d04d910f59e0f4bf53f4e6fdd2e495295236ba444af1819858ebd 
unlink(parent, recursive = TRUE)
```
