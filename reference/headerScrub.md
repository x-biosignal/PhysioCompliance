# Scrub a parsed physiological or imaging header

Scrubs a named structured header without parsing or rewriting a binary
file. The DICOM-adjacent mode implements a documented conservative
subset, not a DICOM PS3.15 conformance statement. DICOM private
attributes, pixels, overlays, and burned-in annotations always remain
manual-review items.

## Usage

``` r
headerScrub(
  header,
  format = c("auto", "edf", "brainvision", "snirf", "dicom"),
  subject_token = NULL,
  date_key = NULL,
  subject_id = NULL,
  free_text = c("error", "drop")
)
```

## Arguments

- header:

  A named plain list, data frame, or `DataFrame`.

- format:

  Explicit format, or `"auto"` when the object carries exactly one
  explicit supported format marker.

- subject_token:

  Optional safe replacement token.

- date_key:

  Caller-owned raw date-shifting key.

- subject_id:

  Subject identifier used only to derive a date offset.

- free_text:

  Refuse or drop detected free-text fields.

## Value

A `header_scrub` object containing the scrubbed header and reports.
