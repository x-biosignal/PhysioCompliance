make_subject_store <- function() {
  records <- lapply(seq_len(10), function(i) {
    list(
      owner = if (i <= 6L) "subject-a" else "subject-b",
      value = as.integer(i),
      retain = i == 2L
    )
  })
  names(records) <- sprintf("record-%02d", seq_along(records))
  records
}

test_that("data-subject export selects exact records and deep copies them", {
  records <- make_subject_store()
  calls <- 0L
  result <- dataSubjectExport(
    records,
    "subject-a",
    function(record) {
      calls <<- calls + 1L
      record$owner
    }
  )

  expect_s3_class(result, "data_subject_export")
  expect_identical(calls, 10L)
  expect_length(result$records, 6L)
  expect_identical(names(result$records), names(records)[1:6])
  expect_equal(nrow(result$manifest), 6L)
  expect_true(all(grepl("^[0-9a-f]{64}$", result$manifest$sha256)))
  expect_true(all(result$manifest$byte_size > 0))
  expect_false(any(vapply(
    result$records,
    function(record) identical(record$owner, "subject-b"),
    logical(1)
  )))

  result$records[[1L]]$value <- 999L
  expect_identical(records[[1L]]$value, 1L)
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "records=6", fixed = TRUE)
  expect_false(grepl("subject-a", printed, fixed = TRUE))
})

test_that("erasure planning is a no-op and callbacks have exact cardinality", {
  records <- make_subject_store()
  before <- serialize(records, NULL, version = 3L, xdr = TRUE)
  locate_calls <- 0L
  retain_calls <- 0L
  authorize_calls <- 0L

  plan <- dataSubjectErase(
    records,
    "subject-a",
    locate = function(record) {
      locate_calls <<- locate_calls + 1L
      record$owner
    },
    authorize = function(plan) {
      authorize_calls <<- authorize_calls + 1L
      TRUE
    },
    retain = function(record) {
      retain_calls <<- retain_calls + 1L
      if (record$retain) "statutory retention" else NA_character_
    },
    reason = "verified erasure request"
  )

  expect_s3_class(plan, "data_subject_erasure")
  expect_identical(plan$mode, "plan")
  expect_identical(locate_calls, 10L)
  expect_identical(retain_calls, 6L)
  expect_identical(authorize_calls, 0L)
  expect_equal(nrow(plan$plan), 6L)
  expect_identical(sum(plan$plan$decision == "retain"), 1L)
  expect_identical(
    serialize(records, NULL, version = 3L, xdr = TRUE),
    before
  )
  expect_match(
    paste(capture.output(print(plan)), collapse = "\n"),
    "authorized=no",
    fixed = TRUE
  )
})

test_that("authorization refusal and callback failures leave records unchanged", {
  records <- make_subject_store()
  before <- serialize(records, NULL, version = 3L, xdr = TRUE)
  authorize_calls <- 0L

  expect_error(
    dataSubjectErase(
      records,
      "subject-a",
      function(record) record$owner,
      function(plan) {
        authorize_calls <<- authorize_calls + 1L
        FALSE
      },
      reason = "verified erasure request",
      mode = "apply"
    ),
    "not authorized"
  )
  expect_identical(authorize_calls, 1L)
  expect_identical(
    serialize(records, NULL, version = 3L, xdr = TRUE),
    before
  )

  expect_error(
    dataSubjectErase(
      records,
      "subject-a",
      function(record) stop("sensitive callback message"),
      function(plan) TRUE,
      reason = "verified erasure request"
    ),
    "locate.*record-01"
  )
  expect_error(
    dataSubjectErase(
      records,
      "subject-a",
      function(record) record$owner,
      function(plan) stop("sensitive callback message"),
      reason = "verified erasure request",
      mode = "apply"
    ),
    "authorize.*failed"
  )
  expect_identical(
    serialize(records, NULL, version = 3L, xdr = TRUE),
    before
  )
})

test_that("authorized apply erases only non-retained matching records", {
  records <- make_subject_store()
  authorize_calls <- 0L
  result <- dataSubjectErase(
    records,
    "subject-a",
    function(record) record$owner,
    function(plan) {
      authorize_calls <<- authorize_calls + 1L
      TRUE
    },
    retain = function(record) {
      if (record$retain) "statutory retention" else NA_character_
    },
    reason = "verified erasure request",
    mode = "apply"
  )

  expect_identical(authorize_calls, 1L)
  expect_true(result$authorized)
  expect_identical(
    names(result$records),
    c("record-02", "record-07", "record-08", "record-09", "record-10")
  )
  expect_identical(sum(result$receipt$status == "erased"), 5L)
  expect_identical(sum(result$receipt$status == "retained"), 1L)
  expect_identical(
    result$receipt$retention_reason[
      result$receipt$status == "retained"
    ],
    "statutory retention"
  )
  expect_match(result$plan_digest, "^[0-9a-f]{64}$")
  receipt_text <- paste(capture.output(str(result$receipt)), collapse = "\n")
  expect_false(grepl("subject-a", receipt_text, fixed = TRUE))
  expect_match(
    paste(capture.output(print(result)), collapse = "\n"),
    "erased=5; retained=1",
    fixed = TRUE
  )
})

test_that("audited child records receive an erasure event before removal", {
  child <- initializeAuditTrail(
    PhysioCore::PhysioExperiment(
      assays = list(raw = matrix(1:4, 2)),
      samplingRate = 10
    ),
    actor = "operator",
    timestamp = fixed_time()
  )
  multi <- PhysioCore::MultiRatePhysioExperiment(
    first = child,
    second = child
  )
  linked <- PhysioCompliance:::.ds_append_erasure_event(
    multi,
    strrep("a", 64),
    "verified erasure request",
    fixed_time(1)
  )

  expect_match(linked$audit_head, "^[0-9a-f]{64}$")
  for (stream in as.list(linked$record@streams)) {
    state <- compliance_state(stream)
    event <- state$audit[[length(state$audit)]]
    expect_identical(
      event$action,
      "physio_compliance.data_subject_erasure"
    )
    expect_identical(event$details$plan_digest, strrep("a", 64))
    expect_false(grepl(
      "source-subject",
      paste(capture.output(str(event)), collapse = "\n"),
      fixed = TRUE
    ))
    expect_true(verifyAuditTrail(stream)$valid)
  }
})

test_that("workflow helpers reject identity leakage and mutable records", {
  records <- make_subject_store()
  names(records)[[1L]] <- "subject-a-record"
  expect_error(
    dataSubjectExport(records, "subject-a", function(record) record$owner),
    "Record names"
  )

  records <- make_subject_store()
  records[[1L]]$mutable <- new.env(parent = emptyenv())
  expect_error(
    dataSubjectExport(records, "subject-a", function(record) record$owner),
    "mutable or unsupported"
  )

  expect_error(
    dataSubjectErase(
      make_subject_store(),
      "subject-a",
      function(record) record$owner,
      function(plan) TRUE,
      reason = "remove subject-a",
      mode = "apply"
    ),
    "must not contain"
  )
  expect_error(
    dataSubjectErase(
      make_subject_store(),
      "subject-a",
      function(record) record$owner,
      function(plan) TRUE,
      retain = function(record) "subject-a legal hold",
      reason = "verified request"
    ),
    "retention reason"
  )
  expect_error(
    dataSubjectExport(
      make_subject_store(),
      "subject-a",
      function(record) c(record$owner, record$owner)
    ),
    "invalid result"
  )
})
