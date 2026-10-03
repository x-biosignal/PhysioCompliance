test_that("initialization snapshots provenance and returns a valid chain", {
  provenance <- lapply(1:3, function(i) {
    list(activity = paste0("step-", i), value = as.integer(i))
  })
  x <- make_compliance_experiment(provenance)
  original <- x

  x <- initializeAuditTrail(
    x,
    actor = "operator-01",
    timestamp = fixed_time()
  )
  state <- compliance_state(x)

  expect_null(compliance_state(original))
  expect_identical(names(state), c("schema_version", "audit", "signatures"))
  expect_identical(state$schema_version, "1")
  expect_length(state$audit, 1L)
  expect_identical(
    names(state$audit[[1L]]),
    c(
      "sequence", "timestamp", "actor", "action", "reason", "details",
      "record_hash", "previous_hash", "hash_algorithm",
      "serialization_version", "entry_hash"
    )
  )
  expect_identical(state$audit[[1L]]$sequence, 1L)
  expect_identical(state$audit[[1L]]$previous_hash, strrep("0", 64))
  expect_identical(state$audit[[1L]]$details$provenance_entries, 3L)
  expected_provenance_hash <- digest::digest(
    serialize(provenance, NULL, ascii = FALSE, version = 3L, xdr = TRUE),
    algo = "sha256",
    serialize = FALSE
  )
  expect_identical(
    state$audit[[1L]]$details$provenance_snapshot,
    expected_provenance_hash
  )

  result <- verifyAuditTrail(x)
  expect_s3_class(result, "compliance_verification")
  expect_true(result$valid)
  expect_identical(result, verifyAuditTrail(x))
  expect_error(
    initializeAuditTrail(x, actor = "operator-01"),
    "already initialized"
  )
})

test_that("audit extraction has fixed columns and safe printing", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  x <- appendAuditEvent(
    x,
    action = "review",
    actor = "reviewer-01",
    reason = "quality review",
    details = list(z = 2L, a = list(y = TRUE, x = "ok")),
    timestamp = fixed_time(1)
  )
  trail <- auditTrail(x)

  expect_s3_class(trail, "audit_trail")
  expect_identical(
    names(trail),
    c(
      "sequence", "timestamp", "actor", "action", "reason", "record_hash",
      "previous_hash", "entry_hash", "hash_algorithm",
      "serialization_version", "details"
    )
  )
  expect_identical(names(trail$details[[2L]]), c("a", "z"))
  expect_identical(names(trail$details[[2L]]$a), c("x", "y"))
  expect_match(paste(capture.output(print(trail)), collapse = "\n"),
               "<audit_trail> 2 events", fixed = TRUE)
  expect_match(
    paste(capture.output(print(verifyAuditTrail(x))), collapse = "\n"),
    "<compliance_verification> valid; 2 events; 0 signatures",
    fixed = TRUE
  )
  expect_identical(as.data.frame(verifyAuditTrail(x)), verifyAuditTrail(x)$issues)
})

test_that("uninitialized and malformed storage are distinguished", {
  x <- make_compliance_experiment()
  expect_equal(nrow(auditTrail(x)), 0L)
  result <- verifyAuditTrail(x)
  expect_false(result$valid)
  expect_identical(result$issues$rule_id, "AUDIT_NOT_INITIALIZED")

  metadata <- S4Vectors::metadata(x)
  metadata$physio_compliance <- list(
    schema_version = "99",
    audit = list(),
    signatures = list()
  )
  S4Vectors::metadata(x) <- metadata
  result <- verifyAuditTrail(x)
  expect_false(result$valid)
  expect_true(all(c("EVENT_SCHEMA", "SCHEMA_UNSUPPORTED") %in%
                    result$issues$rule_id))
  expect_error(auditTrail(x), "malformed|no genesis")
})

test_that("chain verification detects retroactive event edits", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  for (i in 1:5) {
    x <- appendAuditEvent(
      x,
      action = paste0("step-", i),
      actor = "operator-01",
      reason = paste("reason", i),
      details = list(index = as.integer(i)),
      timestamp = fixed_time(i)
    )
  }
  expect_true(verifyAuditTrail(x)$valid)

  mutate_event <- function(x, index, field, value) {
    state <- compliance_state(x)
    state$audit[[index]][[field]] <- value
    set_compliance_state(x, state)
  }

  changed <- list(
    actor = mutate_event(x, 3, "actor", "other"),
    reason = mutate_event(x, 3, "reason", "other"),
    timestamp = mutate_event(
      x, 3, "timestamp", "2026-01-02T03:04:00.000000Z"
    ),
    details = mutate_event(x, 3, "details", list(index = 99L)),
    record_hash = mutate_event(x, 3, "record_hash", strrep("a", 64)),
    entry_hash = mutate_event(x, 3, "entry_hash", strrep("b", 64)),
    sequence = mutate_event(x, 3, "sequence", 20L),
    previous_hash = mutate_event(x, 3, "previous_hash", strrep("c", 64))
  )
  expect_true(all(vapply(changed, function(value) {
    !verifyAuditTrail(value)$valid
  }, logical(1))))
  expect_true("TIMESTAMP_ORDER" %in%
                verifyAuditTrail(changed$timestamp)$issues$rule_id)
  expect_true("SEQUENCE_BREAK" %in%
                verifyAuditTrail(changed$sequence)$issues$rule_id)
  expect_true("PREVIOUS_HASH" %in%
                verifyAuditTrail(changed$previous_hash)$issues$rule_id)
  expect_true(all(vapply(
    changed[c("actor", "reason", "details", "record_hash", "entry_hash")],
    function(value) "ENTRY_HASH" %in% verifyAuditTrail(value)$issues$rule_id,
    logical(1)
  )))

  state <- compliance_state(x)
  deleted <- state
  deleted$audit[[3]] <- NULL
  duplicated <- state
  duplicated$audit <- append(duplicated$audit, duplicated$audit[3], after = 3)
  reordered <- state
  reordered$audit[c(3, 4)] <- reordered$audit[c(4, 3)]
  for (candidate in list(deleted, duplicated, reordered)) {
    expect_false(verifyAuditTrail(set_compliance_state(x, candidate))$valid)
  }
})

test_that("record evolution requires an explicit new event", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  original_head <- verifyAuditTrail(x)$head_hash
  SummarizedExperiment::assay(x, "raw")[1, 1] <- 999
  result <- verifyAuditTrail(x)
  expect_false(result$valid)
  expect_identical(result$issues$rule_id, "RECORD_HASH")

  x <- appendAuditEvent(
    x,
    action = "correct_value",
    actor = "operator-01",
    reason = "source correction",
    details = list(cell = c(1L, 1L)),
    timestamp = fixed_time(1)
  )
  expect_true(verifyAuditTrail(x)$valid)
  expect_identical(compliance_state(x)$audit[[2]]$previous_hash, original_head)

  state <- compliance_state(x)
  state$audit[[1]]$actor <- "tampered"
  bad <- set_compliance_state(x, state)
  SummarizedExperiment::assay(bad, "raw")[1, 2] <- 888
  expect_error(appendAuditEvent(
    bad,
    action = "change",
    actor = "operator-01",
    reason = "attempt",
    timestamp = fixed_time(2)
  ), "invalid")
})

test_that("an external head distinguishes an internally valid rollback", {
  earlier <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  current <- appendAuditEvent(
    earlier,
    action = "review",
    actor = "operator-01",
    reason = "completed review",
    timestamp = fixed_time(1)
  )
  retained_head <- verifyAuditTrail(current)$head_hash

  expect_true(verifyAuditTrail(earlier)$valid)
  expect_false(identical(verifyAuditTrail(earlier)$head_hash, retained_head))
  expect_identical(verifyAuditTrail(current)$head_hash, retained_head)
})

test_that("record hashing covers assays metadata events and provenance", {
  x <- initializeAuditTrail(
    make_compliance_experiment(list(list(activity = "import"))),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  mutations <- list(
    assay = function(value) {
      SummarizedExperiment::assay(value, "filtered")[1, 1] <- -1
      value
    },
    coldata = function(value) {
      SummarizedExperiment::colData(value)$quality[1] <- "bad"
      value
    },
    events = function(value) {
      S4Vectors::metadata(value)$events$label[1] <- "changed"
      value
    },
    provenance = function(value) {
      S4Vectors::metadata(value)$provenance[[1]]$activity <- "changed"
      value
    }
  )
  for (mutate in mutations) {
    result <- verifyAuditTrail(mutate(x))
    expect_false(result$valid)
    expect_true("RECORD_HASH" %in% result$issues$rule_id)
  }
})

test_that("audit inputs reject ambiguity and unsupported details", {
  x <- make_compliance_experiment()
  expect_error(initializeAuditTrail(x, actor = ""), "actor")
  expect_error(initializeAuditTrail(x, actor = "a", timestamp = NA), "POSIXct")
  x <- initializeAuditTrail(x, actor = "a", timestamp = fixed_time())

  bad_details <- list(
    list(1L),
    structure(list(a = 1L, b = 2L), names = c("a", "a")),
    list(value = NaN),
    list(value = Inf),
    list(value = factor("a")),
    list(value = new.env()),
    list(value = function() NULL),
    list(value = stats::as.formula("y ~ x"))
  )
  for (details in bad_details) {
    expect_error(appendAuditEvent(
      x,
      action = "review",
      actor = "a",
      reason = "test",
      details = details,
      timestamp = fixed_time(1)
    ))
  }
  expect_error(appendAuditEvent(
    x,
    action = "physio_compliance.test",
    actor = "a",
    reason = "test",
    timestamp = fixed_time(1)
  ), "reserved")
  expect_error(appendAuditEvent(
    x,
    action = "review",
    actor = "a",
    reason = "test",
    timestamp = fixed_time(-1)
  ), "precede")

  tied <- appendAuditEvent(
    x,
    action = "review",
    actor = "a",
    reason = "timestamp tie",
    timestamp = fixed_time()
  )
  expect_true(verifyAuditTrail(tied)$valid)
})
