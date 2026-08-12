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
