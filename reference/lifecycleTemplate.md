# Render a project-owned software lifecycle template set

Render a project-owned software lifecycle template set

## Usage

``` r
lifecycleTemplate(
  out_dir,
  project,
  intended_use,
  software_safety_class = c("unclassified", "A", "B", "C"),
  owner,
  effective_date = Sys.Date(),
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

- software_safety_class:

  Project-supplied class or `"unclassified"`.

- owner:

  Recorded document owner.

- effective_date:

  Effective date to render.

- source_editions:

  Metadata returned by
  [`standardsSources()`](https://x-biosignal.github.io/PhysioCompliance/reference/standardsSources.md).

- overwrite:

  Whether to replace an existing destination atomically.

## Value

A `lifecycle_template` manifest.

## Examples

``` r
parent <- file.path(tempdir(), "pc-lifecycle-demo")
dir.create(parent, showWarnings = FALSE)
tmpl <- lifecycleTemplate(
  out_dir = file.path(parent, "lifecycle"),
  project = "Example Project",
  intended_use = "Research data processing",
  owner = "project-owner",
  effective_date = as.Date("2026-07-28")
)
tmpl
#> <lifecycle_template>
#>   template set: physio-lifecycle-v1 
#>   template type: lifecycle 
#>   project: Example Project 
#>   recorded class: unclassified 
#>   classification review: pending 
#>   files: 14 
#>   content hash: 248ce0214b11c6c0e604d83227a118c8718bb8d9176b450a04b011117ffb995f 
unlink(parent, recursive = TRUE)
```
