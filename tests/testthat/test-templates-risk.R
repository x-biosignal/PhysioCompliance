test_that("standardsSources is deterministic and reports review age", {
  sources <- standardsSources(
    max_age_days = 365L,
    as_of = as.Date("2026-07-28")
  )
  expect_s3_class(sources, "standards_sources")
  expect_identical(sources$source_id, sort(sources$source_id))
  expect_equal(nrow(sources), 7L)
  expect_true(all(sources$review_status == "current"))
  expect_true(all(startsWith(sources$url, "https://")))
  expect_error(
    standardsSources(as_of = as.Date("2026-07-27")),
    "future"
  )
  stale <- standardsSources(
    max_age_days = 0L,
    as_of = as.Date("2026-07-29")
  )
  expect_true(all(stale$review_status == "review_due"))
  expect_match(paste(capture.output(print(sources)), collapse = "\n"),
               "review due: 0")
})

test_that("lifecycle templates render byte-identically from verified inventory", {
  parent <- local_tempdir()
  first <- file.path(parent, "first")
  second <- file.path(parent, "second")
  sources <- standardsSources(as_of = as.Date("2026-07-28"))
  special_use <- "Use \\1 & $ value"
  args <- list(
    project = "Study-01",
    intended_use = special_use,
    software_safety_class = "B",
    owner = "Quality owner",
    effective_date = as.Date("2026-07-28"),
    source_editions = sources
  )
  a <- do.call(lifecycleTemplate, c(list(out_dir = first), args))
  b <- do.call(lifecycleTemplate, c(list(out_dir = second), args))
  expect_s3_class(a, "lifecycle_template")
  expect_identical(a$content_hash, b$content_hash)
  expect_identical(a$files, b$files)
  expect_equal(nrow(a$files), 14L)
  for (path in a$files$path) {
    expect_identical(
      readBin(file.path(first, path), "raw", n = file.info(file.path(first, path))$size),
      readBin(file.path(second, path), "raw", n = file.info(file.path(second, path))$size)
    )
  }
  rendered <- unlist(lapply(
    file.path(first, a$files$path),
    readLines,
    warn = FALSE
  ))
  expect_false(any(grepl("\\{\\{[A-Z][A-Z0-9_]*\\}\\}", rendered)))
  record <- jsonlite::fromJSON(file.path(first, "project-record.json"))
  expect_identical(record$classification_value, "B")
  expect_identical(record$classification_review_status, "pending")
  expect_identical(record$intended_use, special_use)
  requirements <- paste(
    readLines(file.path(first, "software-development-plan.md"), warn = FALSE),
    collapse = "\n"
  )
  expect_true(grepl(special_use, requirements, fixed = TRUE))
  expect_match(paste(capture.output(print(a)), collapse = "\n"),
               "classification review: pending")
})

test_that("Markdown templates begin with accountable document metadata", {
  inventory <- PhysioCompliance:::.pc_template_manifest()
  markdown <- inventory$manifest$files[
    tools::file_ext(inventory$manifest$files) == "md"
  ]
  first_lines <- vapply(
    file.path(inventory$root, markdown),
    function(path) readLines(path, n = 1L, warn = FALSE),
    character(1)
  )
  expect_true(length(first_lines) > 0L)
  expect_true(all(first_lines == "Document ID:"))
})

test_that("template rendering enforces path, collision, and overwrite guards", {
  parent <- local_tempdir()
  destination <- file.path(parent, "lifecycle")
  fixed <- list(
    out_dir = destination,
    project = "Study",
    intended_use = "Analysis",
    owner = "Owner",
    effective_date = as.Date("2026-07-28"),
    source_editions = standardsSources(as_of = as.Date("2026-07-28"))
  )
  first <- do.call(lifecycleTemplate, fixed)
  record <- jsonlite::fromJSON(file.path(destination, "project-record.json"))
  expect_identical(record$classification_value, "unclassified")
  expect_error(do.call(lifecycleTemplate, fixed), "already exists")
  replacement <- fixed
  replacement$intended_use <- "Changed analysis"
  replacement$overwrite <- TRUE
  second <- do.call(lifecycleTemplate, replacement)
  expect_false(identical(first$content_hash, second$content_hash))
  expect_error(
    lifecycleTemplate(
      file.path(parent, "bad"),
      project = "../escape",
      intended_use = "Analysis",
      owner = "Owner"
    ),
    "path separator"
  )
  expect_error(
    lifecycleTemplate(
      file.path(parent, "bad"),
      project = "Study",
      intended_use = "line one\nline two",
      owner = "Owner"
    ),
    "single-line"
  )
})

test_that("risk template records an optional project risk matrix", {
  parent <- local_tempdir()
  out <- file.path(parent, "risk")
  rendered <- riskManagementTemplate(
    out,
    project = "Study",
    intended_use = "Analysis",
    owner = "Risk owner",
    effective_date = as.Date("2026-07-28"),
    risk_matrix = ws1037_risk_matrix(),
    source_editions = standardsSources(as_of = as.Date("2026-07-28"))
  )
  expect_true(file.exists(file.path(out, "risk-matrix.json")))
  expect_true("risk-matrix.json" %in% rendered$files$path)
  matrix_json <- jsonlite::fromJSON(file.path(out, "risk-matrix.json"))
  expect_identical(matrix_json$matrix_id, "RM01")
  expect_match(matrix_json$matrix_hash, "^[0-9a-f]{64}$")
})

test_that("riskMatrix preserves explicit cells without an RPN", {
  matrix <- ws1037_risk_matrix()
  expect_s3_class(matrix, "risk_matrix")
  expect_equal(nrow(matrix$decisions), 20L)
  expect_false(any(grepl("rpn|product|score", names(matrix), ignore.case = TRUE)))
  shuffled <- riskMatrix(
    matrix$severity[sample(nrow(matrix$severity)), ],
    matrix$probability[sample(nrow(matrix$probability)), ],
    matrix$decisions[sample(nrow(matrix$decisions)), ],
    matrix$matrix_id,
    matrix$version,
    matrix$rationale
  )
  expect_identical(matrix$matrix_hash, shuffled$matrix_hash)
  expect_identical(matrix$decisions, shuffled$decisions)
  attribute_free <- as.data.frame(
    lapply(matrix$decisions, unname),
    stringsAsFactors = FALSE,
    optional = TRUE
  )
  names(attribute_free) <- names(matrix$decisions)
  rebuilt <- riskMatrix(
    matrix$severity, matrix$probability, attribute_free,
    matrix$matrix_id, matrix$version, matrix$rationale
  )
  expect_identical(matrix$matrix_hash, rebuilt$matrix_hash)
  expect_null(attr(matrix$decisions, "out.attrs", exact = TRUE))
  attributed <- matrix$decisions
  attr(attributed$decision, "label") <- "not canonical"
  expect_error(
    riskMatrix(
      matrix$severity, matrix$probability, attributed,
      matrix$matrix_id, matrix$version, matrix$rationale
    ),
    "plain data frame"
  )
  missing <- matrix$decisions[-1, ]
  expect_error(
    riskMatrix(
      matrix$severity, matrix$probability, missing,
      "RM02", "1", "Policy"
    ),
    "exactly one"
  )
  changed <- matrix$decisions
  changed$decision[[1]] <- "review_required"
  changed_matrix <- riskMatrix(
    matrix$severity, matrix$probability, changed,
    matrix$matrix_id, matrix$version, matrix$rationale
  )
  expect_false(identical(matrix$matrix_hash, changed_matrix$matrix_hash))
  forged <- matrix
  forged$decisions$decision[[1]] <- "invented"
  forged$matrix_hash <- PhysioCompliance:::.pc_hash(
    forged[names(forged) != "matrix_hash"]
  )
  expect_false(PhysioCompliance:::.pc_risk_matrix_valid(forged))
  fixture_root <- local_tempdir()
  fixture <- ws1037_trace_fixture(fixture_root, n = 1L)
  expect_error(
    traceabilityMatrix(
      fixture$requirements, fixture$risks, fixture$controls, fixture$tests,
      fixture$links, risk_matrix = forged
    ),
    "valid risk_matrix"
  )
  expect_match(paste(capture.output(print(matrix)), collapse = "\n"),
               "explicit decisions: 20")
})
