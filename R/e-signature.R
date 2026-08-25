.pc_challenge_fields <- c(
  "challenge_version", "record_hash", "audit_head_hash", "signer", "meaning",
  "timestamp", "credential_id", "credential_algorithm",
  "credential_fingerprint"
)

.pc_secret_name <- function(name) {
  normalized <- gsub("[^a-z0-9]", "", tolower(name))
  grepl("password|secret|privatekey|token|apikey", normalized)
}

.pc_reject_secret_names <- function(value, path = "credential") {
  if (!is.null(names(value)) && any(.pc_secret_name(names(value)))) {
    .pc_abort(sprintf("`%s` contains a secret-like field name.", path))
  }
  if (is.list(value)) {
    for (i in seq_along(value)) {
      child <- if (is.null(names(value))) as.character(i) else names(value)[[i]]
      .pc_reject_secret_names(value[[i]], paste0(path, "$", child))
    }
  }
  invisible(NULL)
}

.pc_credential <- function(credential) {
  if (!is.list(credential) || !identical(class(credential), "list")) {
    .pc_abort("`credential` must be a plain named list.")
  }
  .pc_reject_secret_names(credential)
  required <- c("id", "algorithm", "fingerprint")
  allowed <- c(required, "public_data")
  if (is.null(names(credential)) || anyNA(names(credential)) ||
      anyDuplicated(names(credential)) ||
      !setequal(names(credential), intersect(allowed, names(credential))) ||
      !all(required %in% names(credential)) ||
      length(credential) != length(unique(names(credential)))) {
    .pc_abort(
      "`credential` must contain id, algorithm, fingerprint, and optional public_data."
    )
  }
  unknown <- setdiff(names(credential), allowed)
  if (length(unknown)) {
    .pc_abort("`credential` contains unsupported fields.")
  }
  out <- list(
    id = .pc_assert_string(credential$id, "credential$id"),
    algorithm = .pc_assert_string(
      credential$algorithm, "credential$algorithm"
    ),
    fingerprint = .pc_assert_string(
      credential$fingerprint, "credential$fingerprint"
    ),
    public_data = if ("public_data" %in% names(credential)) {
      .pc_canonicalize(
        credential$public_data,
        path = "credential$public_data",
        named_list = TRUE
      )
    } else {
      NULL
    }
  )
  out
}

.pc_make_challenge <- function(record_hash, audit_head_hash, signer, meaning,
                               timestamp, credential) {
  list(
    challenge_version = "1",
    record_hash = record_hash,
    audit_head_hash = audit_head_hash,
    signer = signer,
    meaning = meaning,
    timestamp = timestamp,
    credential_id = credential$id,
    credential_algorithm = credential$algorithm,
    credential_fingerprint = credential$fingerprint
  )
}

.pc_signature_schema_valid <- function(signature) {
  if (!is.list(signature) || !identical(class(signature), "list") ||
      !identical(names(signature), .pc_signature_fields)) {
    return(FALSE)
  }
  scalar <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) &&
      nzchar(trimws(value))
  }
  hashes <- c(
    "signature_id", "record_hash", "audit_head_hash", "challenge_hash",
    "signature_hash"
  )
  if (!all(vapply(signature[hashes], .pc_is_hash, logical(1))) ||
      !identical(signature$challenge_version, "1") ||
      !all(vapply(
        signature[c(
          "signer", "meaning", "credential_id", "credential_algorithm",
          "credential_fingerprint"
        )],
        scalar,
        logical(1)
      )) ||
      !.pc_is_timestamp(signature$timestamp) ||
      !is.raw(signature$signature) || !length(signature$signature)) {
    return(FALSE)
  }
  public_data <- tryCatch(
    .pc_canonicalize(
      signature$credential_public_data,
      path = "credential_public_data",
      named_list = TRUE
    ),
    error = function(e) structure(FALSE, class = "pc_invalid")
  )
  !inherits(public_data, "pc_invalid") &&
    identical(public_data, signature$credential_public_data) &&
    !tryCatch(
      {
        .pc_reject_secret_names(public_data, "credential_public_data")
        FALSE
      },
      error = function(e) TRUE
    )
}

.pc_signature_credential <- function(signature) {
  list(
    id = signature$credential_id,
    algorithm = signature$credential_algorithm,
    fingerprint = signature$credential_fingerprint,
    public_data = signature$credential_public_data
  )
}

.pc_add_signature_issue <- function(issues, sequence, rule_id, message) {
  issues[[length(issues) + 1L]] <- data.frame(
    component = "signature",
    sequence = as.integer(sequence),
    rule_id = rule_id,
    message = message,
    stringsAsFactors = FALSE
  )
  issues
}

#' Add an externally authenticated electronic signature
#'
#' The callback is responsible for authenticating and authorizing the signer.
#' It is called exactly once with canonical challenge bytes and a sanitized
#' public credential descriptor. The package never accepts passwords, private
#' keys, API tokens, or reusable secrets.
#'
#' The callback's algorithm and credential service determine cryptographic
#' strength, identity assurance, and non-repudiation properties. This function
#' is a technical control and does not itself establish regulatory compliance.
#'
#' @param x An initialized, currently valid [PhysioCore::PhysioExperiment].
#' @param signer Non-empty signer identity.
#' @param meaning Non-empty signature meaning.
#' @param credential Named list containing `id`, `algorithm`, `fingerprint`, and
#'   optional non-secret `public_data`.
#' @param sign Function called once as `sign(challenge_raw, credential)`. It
#'   must return a non-empty raw vector.
#' @param timestamp One finite `POSIXct` value that does not precede the audit
#'   head.
#'
#' @return A modified copy of `x` with one signature and one cross-linked audit
#'   event.
#' @export
eSign <- function(
    x,
    signer,
    meaning,
    credential,
    sign,
    timestamp = Sys.time()) {
  .pc_assert_experiment(x)
  signer <- .pc_assert_string(signer, "signer")
  meaning <- .pc_assert_string(meaning, "meaning")
  credential <- .pc_credential(credential)
  if (!is.function(sign)) {
    .pc_abort("`sign` must be a function.")
  }
  timestamp_text <- .pc_timestamp(timestamp)
  audit_result <- verifyAuditTrail(x)
  if (!audit_result$valid) {
    .pc_abort("The audit trail must be valid before signing.")
  }
  state <- .pc_state(x)
  head_event <- state$audit[[length(state$audit)]]
  if (.pc_timestamp_number(timestamp_text) <
      .pc_timestamp_number(head_event$timestamp)) {
    .pc_abort("`timestamp` cannot precede the audit head.")
  }

  challenge <- .pc_make_challenge(
    record_hash = audit_result$record_hash,
    audit_head_hash = audit_result$head_hash,
    signer = signer,
    meaning = meaning,
    timestamp = timestamp_text,
    credential = credential
  )
  challenge_raw <- .pc_serialize(challenge)
  challenge_hash <- .pc_hash_raw(challenge_raw)
  if (challenge_hash %in% vapply(
    state$signatures,
    function(value) {
      if (is.list(value) && is.character(value$signature_id) &&
          length(value$signature_id) == 1L) value$signature_id else NA_character_
    },
    character(1)
  )) {
    .pc_abort("The electronic-signature challenge has already been used.")
  }

  signature_raw <- tryCatch(
    sign(challenge_raw, credential),
    error = function(e) {
      .pc_abort("The external signing callback failed.")
    }
  )
  if (!is.raw(signature_raw) || !length(signature_raw)) {
    .pc_abort("`sign` must return a non-empty raw vector.")
  }
  signature <- list(
    signature_id = challenge_hash,
    challenge_version = "1",
    record_hash = audit_result$record_hash,
    audit_head_hash = audit_result$head_hash,
    signer = signer,
    meaning = meaning,
    timestamp = timestamp_text,
    credential_id = credential$id,
    credential_algorithm = credential$algorithm,
    credential_fingerprint = credential$fingerprint,
    credential_public_data = credential$public_data,
    challenge_hash = challenge_hash,
    signature = signature_raw,
    signature_hash = .pc_hash_raw(signature_raw)
  )
  state$signatures[[length(state$signatures) + 1L]] <- signature

  .pc_append_event_to_state(
    x = x,
    state = state,
    action = "physio_compliance.electronic_signature",
    actor = signer,
    reason = meaning,
    details = .pc_signature_event_details(signature),
    timestamp = timestamp
  )
}

#' Verify stored electronic signatures
#'
#' Reconstructs each canonical challenge and delegates cryptographic and
#' credential verification to a caller-supplied callback. Provider errors are
#' reported without storing or exposing provider diagnostics.
#'
#' @param x A [PhysioCore::PhysioExperiment] object.
#' @param verify Function called once for each structurally valid signature as
#'   `verify(challenge_raw, signature_raw, credential)`. It must return one
#'   non-missing logical value.
#'
#' @return A `compliance_verification` object containing audit and signature
#'   issues.
#' @export
verifyESignatures <- function(x, verify) {
  .pc_assert_experiment(x)
  if (!is.function(verify)) {
    .pc_abort("`verify` must be a function.")
  }
  base <- verifyAuditTrail(x)
  state <- .pc_state(x)
  if (is.null(state) || !is.list(state) || !is.list(state$signatures)) {
    return(base)
  }
  issues <- lapply(seq_len(nrow(base$issues)), function(i) {
    base$issues[i, , drop = FALSE]
  })
  signatures <- state$signatures
  ids <- vapply(signatures, function(signature) {
    if (is.list(signature) && is.character(signature$signature_id) &&
        length(signature$signature_id) == 1L && !is.na(signature$signature_id)) {
      signature$signature_id
    } else {
      NA_character_
    }
  }, character(1))

  for (i in seq_along(signatures)) {
    signature <- signatures[[i]]
    if (!.pc_signature_schema_valid(signature)) {
      issues <- .pc_add_signature_issue(
        issues, i, "SIGNATURE_SCHEMA",
        "The electronic-signature record does not match the required schema."
      )
      next
    }
    if (sum(!is.na(ids) & ids == signature$signature_id) > 1L) {
      issues <- .pc_add_signature_issue(
        issues, i, "SIGNATURE_REPLAY",
        "The electronic-signature identifier is duplicated."
      )
    }
    if (!identical(.pc_hash_raw(signature$signature),
                   signature$signature_hash)) {
      issues <- .pc_add_signature_issue(
        issues, i, "SIGNATURE_HASH",
        "The signature bytes do not match the stored signature hash."
      )
    }

    credential <- .pc_signature_credential(signature)
    challenge <- .pc_make_challenge(
      record_hash = signature$record_hash,
      audit_head_hash = signature$audit_head_hash,
      signer = signature$signer,
      meaning = signature$meaning,
      timestamp = signature$timestamp,
      credential = credential
    )
    challenge_raw <- .pc_serialize(challenge)
    challenge_hash <- .pc_hash_raw(challenge_raw)
    if (!identical(challenge_hash, signature$challenge_hash) ||
        !identical(challenge_hash, signature$signature_id)) {
      issues <- .pc_add_signature_issue(
        issues, i, "CHALLENGE_HASH",
        "The reconstructed challenge does not match its stored hash."
      )
    }

    event_index <- which(vapply(state$audit, function(event) {
      is.list(event) &&
        identical(event$action, "physio_compliance.electronic_signature") &&
        identical(event$details[["signature_id"]], signature$signature_id)
    }, logical(1)))
    if (length(event_index) == 1L &&
        !identical(
          state$audit[[event_index]]$details[["credential_fingerprint"]],
          signature$credential_fingerprint
        )) {
      issues <- .pc_add_signature_issue(
        issues, i, "CREDENTIAL_MISMATCH",
        "The signature credential does not match its audit event."
      )
    }

    callback_result <- tryCatch(
      verify(challenge_raw, signature$signature, credential),
      error = function(e) NA
    )
    if (!is.logical(callback_result) || length(callback_result) != 1L ||
        is.na(callback_result) || !callback_result) {
      issues <- .pc_add_signature_issue(
        issues, i, "SIGNATURE_INVALID",
        "The external verifier rejected the electronic signature."
      )
    }
  }

  issue_frame <- if (length(issues)) do.call(rbind, issues) else .pc_empty_issues()
  .pc_verification(
    issue_frame,
    base$head_hash,
    base$record_hash,
    base$n_events,
    base$n_signatures
  )
}
