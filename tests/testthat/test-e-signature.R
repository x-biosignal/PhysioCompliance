test_that("electronic signatures bind record head identity and meaning", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  calls <- 0L
  signer <- function(challenge, credential) {
    calls <<- calls + 1L
    test_signer(challenge, credential)
  }
  x <- eSign(
    x,
    signer = "reviewer-01",
    meaning = "reviewed and approved",
    credential = test_credential(),
    sign = signer,
    timestamp = fixed_time(1)
  )
  x <- eSign(
    x,
    signer = "reviewer-01",
    meaning = "approved for release",
    credential = test_credential(),
    sign = signer,
    timestamp = fixed_time(2)
  )
  expect_identical(calls, 2L)

  state <- compliance_state(x)
  expect_length(state$signatures, 2L)
  expect_identical(
    names(state$signatures[[1]]),
    c(
      "signature_id", "challenge_version", "record_hash", "audit_head_hash",
      "signer", "meaning", "timestamp", "credential_id",
      "credential_algorithm", "credential_fingerprint",
      "credential_public_data", "challenge_hash", "signature", "signature_hash"
    )
  )
  expect_identical(state$signatures[[1]]$signer, "reviewer-01")
  expect_identical(state$signatures[[1]]$meaning, "reviewed and approved")
  expect_identical(state$signatures[[1]]$credential_id, "credential-01")

  verify_calls <- 0L
  verifier <- function(challenge, signature, credential) {
    verify_calls <<- verify_calls + 1L
    test_verifier(challenge, signature, credential)
  }
  result <- verifyESignatures(x, verifier)
  expect_true(result$valid)
  expect_identical(verify_calls, 2L)
  expect_true(verifyAuditTrail(x)$valid)
})

test_that("sign callback failures are not swallowed or retried", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  calls <- 0L
  failing <- function(challenge, credential) {
    calls <<- calls + 1L
    stop("provider rejected authentication")
  }
  error <- expect_error(eSign(
    x,
    signer = "reviewer-01",
    meaning = "approve",
    credential = test_credential(),
    sign = failing,
    timestamp = fixed_time(1)
  ), "external signing callback failed")
  expect_false(grepl("provider rejected", conditionMessage(error)))
  expect_identical(calls, 1L)
  expect_length(compliance_state(x)$signatures, 0L)

  calls <- 0L
  wrong_type <- function(challenge, credential) {
    calls <<- calls + 1L
    "not raw"
  }
  expect_error(eSign(
    x,
    signer = "reviewer-01",
    meaning = "approve",
    credential = test_credential(),
    sign = wrong_type,
    timestamp = fixed_time(1)
  ), "non-empty raw")
  expect_identical(calls, 1L)
})

test_that("secret-like credential fields are rejected recursively", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  credentials <- list(
    c(test_credential(), list(password = "value")),
    c(test_credential(), list(private_key = charToRaw("value"))),
    c(test_credential(), list(apiToken = "value")),
    test_credential(list(provider = "test", nested = list(secret = "value")))
  )
  for (credential in credentials) {
    expect_error(eSign(
      x,
      signer = "reviewer-01",
      meaning = "approve",
      credential = credential,
      sign = test_signer,
      timestamp = fixed_time(1)
    ), "secret-like")
  }
  expect_length(compliance_state(x)$signatures, 0L)
})

test_that("signature tampering is detected without mutating the object", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  x <- eSign(
    x,
    signer = "reviewer-01",
    meaning = "approve",
    credential = test_credential(),
    sign = test_signer,
    timestamp = fixed_time(1)
  )
  original <- serialize(x, NULL, version = 3, xdr = TRUE)

  change_signature <- function(x, field, value) {
    state <- compliance_state(x)
    state$signatures[[1]][[field]] <- value
    set_compliance_state(x, state)
  }
  cases <- list(
    signer = change_signature(x, "signer", "other"),
    meaning = change_signature(x, "meaning", "reject"),
    timestamp = change_signature(
      x, "timestamp", "2026-01-02T03:04:07.000000Z"
    ),
    credential_id = change_signature(x, "credential_id", "other-id"),
    credential_algorithm = change_signature(
      x, "credential_algorithm", "other-algorithm"
    ),
    credential_fingerprint = change_signature(
      x, "credential_fingerprint", "other-fingerprint"
    ),
    public_data = change_signature(
      x, "credential_public_data", list(provider = "changed")
    ),
    record_hash = change_signature(x, "record_hash", strrep("a", 64)),
    audit_head = change_signature(x, "audit_head_hash", strrep("b", 64)),
    challenge_hash = change_signature(x, "challenge_hash", strrep("c", 64)),
    signature_hash = change_signature(x, "signature_hash", strrep("d", 64))
  )
  state <- compliance_state(x)
  raw_changed <- state
  raw_changed$signatures[[1]]$signature[[1]] <-
    as.raw(bitwXor(as.integer(raw_changed$signatures[[1]]$signature[[1]]), 1L))
  cases$signature <- set_compliance_state(x, raw_changed)

  for (candidate in cases) {
    expect_false(verifyESignatures(candidate, test_verifier)$valid)
  }
  expect_true("SIGNATURE_HASH" %in%
                verifyESignatures(cases$signature, test_verifier)$issues$rule_id)
  expect_true("CHALLENGE_HASH" %in%
                verifyESignatures(cases$meaning, test_verifier)$issues$rule_id)
  expect_true("SIGNATURE_INVALID" %in%
                verifyESignatures(cases$public_data, test_verifier)$issues$rule_id)
  expect_identical(serialize(x, NULL, version = 3, xdr = TRUE), original)
})

test_that("removed duplicated and unlinked signatures fail", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  x <- eSign(
    x,
    signer = "reviewer-01",
    meaning = "approve",
    credential = test_credential(),
    sign = test_signer,
    timestamp = fixed_time(1)
  )
  state <- compliance_state(x)

  removed_signature <- state
  removed_signature$signatures <- list()
  expect_true("SIGNATURE_LINK" %in% verifyAuditTrail(
    set_compliance_state(x, removed_signature)
  )$issues$rule_id)

  removed_event <- state
  removed_event$audit[[2]] <- NULL
  expect_true("SIGNATURE_LINK" %in% verifyAuditTrail(
    set_compliance_state(x, removed_event)
  )$issues$rule_id)

  duplicate <- state
  duplicate$signatures[[2]] <- duplicate$signatures[[1]]
  duplicate_x <- set_compliance_state(x, duplicate)
  result <- verifyESignatures(duplicate_x, test_verifier)
  expect_false(result$valid)
  expect_true("SIGNATURE_REPLAY" %in% result$issues$rule_id)
})

test_that("verification callback errors and invalid returns are issues", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  x <- eSign(
    x,
    signer = "reviewer-01",
    meaning = "approve",
    credential = test_credential(),
    sign = test_signer,
    timestamp = fixed_time(1)
  )
  callbacks <- list(
    function(...) stop("provider detail must not escape"),
    function(...) NA,
    function(...) c(TRUE, TRUE),
    function(...) "yes",
    function(...) FALSE
  )
  for (callback in callbacks) {
    result <- verifyESignatures(x, callback)
    expect_false(result$valid)
    expect_true("SIGNATURE_INVALID" %in% result$issues$rule_id)
    expect_false(any(grepl("provider detail", result$issues$message)))
  }
})

test_that("signing requires a current valid audited state", {
  x <- initializeAuditTrail(
    make_compliance_experiment(),
    actor = "operator-01",
    timestamp = fixed_time()
  )
  SummarizedExperiment::assay(x, "raw")[1, 1] <- 999
  expect_error(eSign(
    x,
    signer = "reviewer-01",
    meaning = "approve",
    credential = test_credential(),
    sign = test_signer,
    timestamp = fixed_time(1)
  ), "valid before signing")

  x <- appendAuditEvent(
    x,
    action = "correct_value",
    actor = "operator-01",
    reason = "source correction",
    timestamp = fixed_time(1)
  )
  expect_error(eSign(
    x,
    signer = "reviewer-01",
    meaning = "approve",
    credential = test_credential(),
    sign = test_signer,
    timestamp = fixed_time()
  ), "precede")
})
