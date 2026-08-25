.pc_event_fields <- c(
  "sequence", "timestamp", "actor", "action", "reason", "details",
  "record_hash", "previous_hash", "hash_algorithm",
  "serialization_version", "entry_hash"
)

.pc_event_hash_fields <- .pc_event_fields[-length(.pc_event_fields)]

.pc_signature_fields <- c(
  "signature_id", "challenge_version", "record_hash", "audit_head_hash",
  "signer", "meaning", "timestamp", "credential_id",
  "credential_algorithm", "credential_fingerprint",
  "credential_public_data", "challenge_hash", "signature", "signature_hash"
)

.pc_abort <- function(message) {
  stop(message, call. = FALSE)
}

.pc_assert_experiment <- function(x) {
  if (!methods::is(x, "PhysioExperiment")) {
    .pc_abort("`x` must inherit from PhysioExperiment.")
  }
  invisible(x)
}

.pc_assert_string <- function(value, name) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    .pc_abort(sprintf("`%s` must be one non-empty character value.", name))
  }
  value
}

.pc_timestamp <- function(value, name = "timestamp") {
  if (!inherits(value, "POSIXct") || length(value) != 1L ||
      is.na(value) || !is.finite(as.numeric(value))) {
    .pc_abort(sprintf("`%s` must be one finite POSIXct value.", name))
  }
  format(value, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC", usetz = FALSE)
}

.pc_is_timestamp <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl(
        "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}\\.[0-9]{6}Z$",
        value
      )) {
    return(FALSE)
  }
  base <- substr(value, 1L, 19L)
  parsed <- suppressWarnings(strptime(
    base,
    format = "%Y-%m-%dT%H:%M:%S",
    tz = "UTC"
  ))
  !is.na(parsed) &&
    identical(format(parsed, "%Y-%m-%dT%H:%M:%S", tz = "UTC"), base)
}

.pc_timestamp_number <- function(value) {
  base <- as.POSIXct(
    strptime(substr(value, 1L, 19L), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  )
  as.numeric(base) + as.numeric(paste0("0.", substr(value, 21L, 26L)))
}

.pc_serialize <- function(value) {
  serialize(value, NULL, ascii = FALSE, version = 3L, xdr = TRUE)
}

.pc_hash_raw <- function(value) {
  digest::digest(value, algo = "sha256", serialize = FALSE)
}

.pc_hash <- function(value) {
  .pc_hash_raw(.pc_serialize(value))
}

.pc_is_hash <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) &&
    grepl("^[0-9a-f]{64}$", value)
}

.pc_validate_names <- function(value, path, required = FALSE) {
  nms <- names(value)
  if (is.null(nms)) {
    if (required && length(value)) {
      .pc_abort(sprintf("`%s` must be named.", path))
    }
    return(invisible(NULL))
  }
  if (anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms)) {
    .pc_abort(sprintf("`%s` contains missing, empty, or duplicated names.", path))
  }
  invisible(NULL)
}

.pc_canonicalize <- function(value, path = "details", named_list = TRUE) {
  forbidden <- is.environment(value) || is.function(value) ||
    inherits(value, "connection") || inherits(value, "formula") ||
    isS4(value) ||
    typeof(value) %in% c("externalptr", "weakref", "symbol", "language")
  if (forbidden) {
    .pc_abort(sprintf("`%s` contains an unsupported value.", path))
  }

  if (is.null(value)) {
    return(NULL)
  }

  if (is.list(value)) {
    if (!identical(class(value), "list")) {
      .pc_abort(sprintf("`%s` must contain plain lists and atomic vectors.", path))
    }
    .pc_validate_names(value, path, required = named_list)
    if (!length(value)) {
      return(list())
    }
    nms <- names(value)
    ord <- order(enc2utf8(nms), method = "radix")
    out <- lapply(ord, function(i) {
      .pc_canonicalize(
        value[[i]],
        path = paste0(path, "$", nms[[i]]),
        named_list = TRUE
      )
    })
    names(out) <- nms[ord]
    return(out)
  }

  if (!is.atomic(value) || typeof(value) %in% c("complex", "expression") ||
      !typeof(value) %in% c("logical", "integer", "double", "character", "raw")) {
    .pc_abort(sprintf("`%s` contains an unsupported value.", path))
  }
  if (is.object(value)) {
    .pc_abort(sprintf("`%s` contains a classed atomic value.", path))
  }
  attrs <- attributes(value)
  if (!is.null(attrs) && !identical(names(attrs), "names")) {
    .pc_abort(sprintf("`%s` contains unsupported attributes.", path))
  }
  .pc_validate_names(value, path, required = FALSE)
  if (is.double(value) &&
      any(is.nan(value) | (!is.na(value) & !is.finite(value)))) {
    .pc_abort(sprintf("`%s` contains a non-finite number.", path))
  }
  value
}

.pc_canonical_details <- function(details, name = "details") {
  if (!is.list(details) || !identical(class(details), "list")) {
    .pc_abort(sprintf("`%s` must be a plain named list.", name))
  }
  .pc_canonicalize(details, path = name, named_list = TRUE)
}

.pc_record_hash <- function(x) {
  copy <- x
  metadata <- S4Vectors::metadata(copy)
  metadata[["physio_compliance"]] <- NULL
  S4Vectors::metadata(copy) <- metadata
  .pc_hash(copy)
}

.pc_state <- function(x) {
  S4Vectors::metadata(x)[["physio_compliance"]]
}

.pc_set_state <- function(x, state) {
  metadata <- S4Vectors::metadata(x)
  metadata[["physio_compliance"]] <- state
  S4Vectors::metadata(x) <- metadata
  x
}

.pc_make_event <- function(sequence, timestamp, actor, action, reason, details,
                           record_hash, previous_hash) {
  fields <- list(
    sequence = as.integer(sequence),
    timestamp = timestamp,
    actor = actor,
    action = action,
    reason = reason,
    details = .pc_canonical_details(details),
    record_hash = record_hash,
    previous_hash = previous_hash,
    hash_algorithm = "sha256",
    serialization_version = 3L
  )
  fields$entry_hash <- .pc_hash(fields)
  fields
}

.pc_event_schema_valid <- function(event) {
  if (!is.list(event) || !identical(class(event), "list") ||
      !identical(names(event), .pc_event_fields)) {
    return(FALSE)
  }
  scalar_string <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) &&
      nzchar(trimws(value))
  }
  if (!is.integer(event$sequence) || length(event$sequence) != 1L ||
      is.na(event$sequence) || event$sequence < 1L ||
      !.pc_is_timestamp(event$timestamp) ||
      !scalar_string(event$actor) || !scalar_string(event$action) ||
      !scalar_string(event$reason) || !.pc_is_hash(event$record_hash) ||
      !.pc_is_hash(event$previous_hash) || !.pc_is_hash(event$entry_hash) ||
      !identical(event$hash_algorithm, "sha256") ||
      !identical(event$serialization_version, 3L)) {
    return(FALSE)
  }
  canonical <- tryCatch(
    .pc_canonical_details(event$details),
    error = function(e) NULL
  )
  !is.null(canonical) && identical(event$details, canonical)
}

.pc_signature_link_shape <- function(signature) {
  if (!is.list(signature) || !identical(class(signature), "list") ||
      !identical(names(signature), .pc_signature_fields)) {
    return(FALSE)
  }
  required_strings <- c(
    "signature_id", "record_hash", "audit_head_hash", "signer", "meaning",
    "timestamp", "credential_fingerprint", "challenge_hash", "signature_hash"
  )
  all(vapply(signature[required_strings], function(value) {
    is.character(value) && length(value) == 1L && !is.na(value)
  }, logical(1)))
}

.pc_signature_event_details <- function(signature) {
  .pc_canonical_details(list(
    signature_id = signature$signature_id,
    challenge_hash = signature$challenge_hash,
    signature_hash = signature$signature_hash,
    signer = signature$signer,
    meaning = signature$meaning,
    credential_fingerprint = signature$credential_fingerprint
  ))
}

.pc_empty_issues <- function() {
  data.frame(
    component = character(),
    sequence = integer(),
    rule_id = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

.pc_sort_issues <- function(issues) {
  if (!nrow(issues)) {
    return(.pc_empty_issues())
  }
  sequence_order <- ifelse(is.na(issues$sequence), .Machine$integer.max,
                           issues$sequence)
  ord <- order(
    issues$component,
    sequence_order,
    issues$rule_id,
    issues$message,
    method = "radix"
  )
  rownames(issues) <- NULL
  issues[ord, , drop = FALSE]
}

.pc_verification <- function(issues, head_hash, record_hash, n_events,
                             n_signatures) {
  issues <- .pc_sort_issues(issues)
  structure(
    list(
      valid = nrow(issues) == 0L,
      issues = issues,
      head_hash = head_hash,
      record_hash = record_hash,
      n_events = as.integer(n_events),
      n_signatures = as.integer(n_signatures)
    ),
    class = "compliance_verification"
  )
}

.pc_verify_audit <- function(x, compare_record = TRUE) {
  .pc_assert_experiment(x)
  current_hash <- .pc_record_hash(x)
  state <- .pc_state(x)
  issues <- list()
  chain_ok <- TRUE

  add_issue <- function(component, sequence, rule_id, message) {
    issues[[length(issues) + 1L]] <<- data.frame(
      component = component,
      sequence = as.integer(sequence),
      rule_id = rule_id,
      message = message,
      stringsAsFactors = FALSE
    )
  }

  if (is.null(state)) {
    add_issue(
      "audit", NA_integer_, "AUDIT_NOT_INITIALIZED",
      "The compliance audit trail is not initialized."
    )
    return(.pc_verification(
      do.call(rbind, issues), NA_character_, current_hash, 0L, 0L
    ))
  }

  if (!is.list(state) || !identical(class(state), "list")) {
    add_issue(
      "audit", NA_integer_, "EVENT_SCHEMA",
      "The compliance container must be a plain list."
    )
    return(.pc_verification(
      do.call(rbind, issues), NA_character_, current_hash, 0L, 0L
    ))
  }

  if (!identical(names(state), c("schema_version", "audit", "signatures"))) {
    add_issue(
      "audit", NA_integer_, "EVENT_SCHEMA",
      "The compliance container fields are invalid."
    )
    chain_ok <- FALSE
  }
  if (!identical(state[["schema_version"]], "1")) {
    add_issue(
      "audit", NA_integer_, "SCHEMA_UNSUPPORTED",
      "The compliance schema version is unsupported."
    )
    chain_ok <- FALSE
  }

  audit <- state[["audit"]]
  signatures <- state[["signatures"]]
  if (!is.list(audit) || !identical(class(audit), "list")) {
    add_issue(
      "audit", NA_integer_, "EVENT_SCHEMA",
      "The audit field must be a plain list."
    )
    audit <- list()
    chain_ok <- FALSE
  }
  if (!length(audit)) {
    add_issue(
      "audit", NA_integer_, "EVENT_SCHEMA",
      "An initialized audit trail must contain a genesis event."
    )
    chain_ok <- FALSE
  }
  if (!is.list(signatures) || !identical(class(signatures), "list")) {
    add_issue(
      "signature", NA_integer_, "SIGNATURE_LINK",
      "The signatures field must be a plain list."
    )
    signatures <- list()
  }

  previous_timestamp <- -Inf
  valid_event <- logical(length(audit))
  for (i in seq_along(audit)) {
    event <- audit[[i]]
    if (!.pc_event_schema_valid(event)) {
      add_issue(
        "audit", i, "EVENT_SCHEMA",
        "The audit event does not match the required schema."
      )
      chain_ok <- FALSE
      next
    }
    valid_event[[i]] <- TRUE
    if (!identical(event$sequence, as.integer(i))) {
      add_issue(
        "audit", i, "SEQUENCE_BREAK",
        "The audit sequence is not contiguous."
      )
      chain_ok <- FALSE
    }
    timestamp <- .pc_timestamp_number(event$timestamp)
    if (timestamp < previous_timestamp) {
      add_issue(
        "audit", i, "TIMESTAMP_ORDER",
        "The audit timestamp precedes the prior event."
      )
      chain_ok <- FALSE
    }
    previous_timestamp <- timestamp
    expected_previous <- if (i == 1L) {
      strrep("0", 64L)
    } else if (valid_event[[i - 1L]]) {
      audit[[i - 1L]]$entry_hash
    } else {
      NA_character_
    }
    if (!is.na(expected_previous) &&
        !identical(event$previous_hash, expected_previous)) {
      add_issue(
        "audit", i, "PREVIOUS_HASH",
        "The audit previous-hash link is invalid."
      )
      chain_ok <- FALSE
    }
    expected_entry <- .pc_hash(event[.pc_event_hash_fields])
    if (!identical(event$entry_hash, expected_entry)) {
      add_issue(
        "audit", i, "ENTRY_HASH",
        "The audit entry hash does not match its fields."
      )
      chain_ok <- FALSE
    }
  }

  if (compare_record && length(audit) && valid_event[[length(audit)]] &&
      !identical(audit[[length(audit)]]$record_hash, current_hash)) {
    add_issue(
      "record", length(audit), "RECORD_HASH",
      "The current record does not match the audit head."
    )
  }

  internal_event_index <- which(valid_event & vapply(
    audit,
    function(event) {
      is.list(event) &&
        identical(event$action, "physio_compliance.electronic_signature")
    },
    logical(1)
  ))
  event_ids <- vapply(internal_event_index, function(i) {
    value <- audit[[i]]$details[["signature_id"]]
    if (is.character(value) && length(value) == 1L && !is.na(value)) {
      value
    } else {
      NA_character_
    }
  }, character(1))

  signature_ids <- vapply(signatures, function(signature) {
    if (.pc_signature_link_shape(signature)) {
      signature$signature_id
    } else {
      NA_character_
    }
  }, character(1))

  for (j in seq_along(signatures)) {
    signature <- signatures[[j]]
    if (!.pc_signature_link_shape(signature) ||
        !.pc_is_hash(signature$signature_id)) {
      add_issue(
        "signature", j, "SIGNATURE_LINK",
        "A stored signature cannot be linked to an audit event."
      )
      next
    }
    matches <- which(!is.na(event_ids) & event_ids == signature$signature_id)
    if (length(matches) != 1L) {
      add_issue(
        "signature", j, "SIGNATURE_LINK",
        "A stored signature must have exactly one audit event."
      )
      next
    }
    event_index <- internal_event_index[[matches]]
    event <- audit[[event_index]]
    expected_details <- tryCatch(
      .pc_signature_event_details(signature),
      error = function(e) NULL
    )
    if (is.null(expected_details) ||
        !identical(event$details, expected_details) ||
        !identical(event$record_hash, signature$record_hash) ||
        !identical(event$previous_hash, signature$audit_head_hash) ||
        !identical(event$timestamp, signature$timestamp)) {
      add_issue(
        "signature", j, "SIGNATURE_LINK",
        "The stored signature and audit event disagree."
      )
    }
  }

  for (k in seq_along(internal_event_index)) {
    id <- event_ids[[k]]
    matches <- if (is.na(id)) integer() else {
      which(!is.na(signature_ids) & signature_ids == id)
    }
    if (length(matches) != 1L) {
      add_issue(
        "audit", internal_event_index[[k]], "SIGNATURE_LINK",
        "A signature audit event must have exactly one stored signature."
      )
    }
  }

  issue_frame <- if (length(issues)) do.call(rbind, issues) else .pc_empty_issues()
  head_hash <- if (chain_ok && length(audit) &&
                   valid_event[[length(audit)]]) {
    audit[[length(audit)]]$entry_hash
  } else {
    NA_character_
  }
  .pc_verification(
    issue_frame, head_hash, current_hash, length(audit), length(signatures)
  )
}

.pc_assert_appendable <- function(x) {
  result <- .pc_verify_audit(x, compare_record = FALSE)
  if (!result$valid) {
    rules <- unique(result$issues$rule_id)
    .pc_abort(sprintf(
      "The existing compliance chain is invalid (%s).",
      paste(rules, collapse = ", ")
    ))
  }
  result
}

.pc_append_event_to_state <- function(x, state, action, actor, reason, details,
                                      timestamp) {
  audit <- state$audit
  timestamp_text <- .pc_timestamp(timestamp)
  if (length(audit)) {
    prior_time <- .pc_timestamp_number(audit[[length(audit)]]$timestamp)
    if (.pc_timestamp_number(timestamp_text) < prior_time) {
      .pc_abort("`timestamp` cannot precede the audit head.")
    }
  }
  event <- .pc_make_event(
    sequence = length(audit) + 1L,
    timestamp = timestamp_text,
    actor = actor,
    action = action,
    reason = reason,
    details = details,
    record_hash = .pc_record_hash(x),
    previous_hash = audit[[length(audit)]]$entry_hash
  )
  state$audit[[length(audit) + 1L]] <- event
  .pc_set_state(x, state)
}

#' Initialize a tamper-evident audit trail
#'
#' Initializes a versioned SHA-256 audit chain over the complete
#' `PhysioExperiment` record. The genesis event snapshots the existing
#' PhysioCore provenance log. Existing compliance metadata is never replaced.
#'
#' @param x A [PhysioCore::PhysioExperiment] object.
#' @param actor Non-empty identifier for the responsible actor.
#' @param reason Non-empty reason for initialization.
#' @param timestamp One finite `POSIXct` value.
#'
#' @return A modified copy of `x` with a genesis audit event.
#' @export
#'
#' @examples
#' x <- PhysioCore::PhysioExperiment(
#'   assays = list(raw = matrix(1:6, nrow = 3)),
#'   samplingRate = 100
#' )
#' x <- initializeAuditTrail(x, actor = "operator-01")
#' verifyAuditTrail(x)
initializeAuditTrail <- function(
    x,
    actor,
    reason = "audit trail initialized",
    timestamp = Sys.time()) {
  .pc_assert_experiment(x)
  actor <- .pc_assert_string(actor, "actor")
  reason <- .pc_assert_string(reason, "reason")
  timestamp <- .pc_timestamp(timestamp)
  metadata <- S4Vectors::metadata(x)
  if ("physio_compliance" %in% names(metadata)) {
    .pc_abort("The compliance audit trail is already initialized.")
  }
  provenance <- metadata[["provenance"]]
  if (is.null(provenance)) {
    provenance <- list()
  }
  state <- list(
    schema_version = "1",
    audit = list(),
    signatures = list()
  )
  state$audit[[1L]] <- .pc_make_event(
    sequence = 1L,
    timestamp = timestamp,
    actor = actor,
    action = "initialize_audit_trail",
    reason = reason,
    details = list(
      provenance_entries = as.integer(length(provenance)),
      provenance_snapshot = .pc_hash(provenance)
    ),
    record_hash = .pc_record_hash(x),
    previous_hash = strrep("0", 64L)
  )
  .pc_set_state(x, state)
}

#' Append an audit event
#'
#' Appends an event to a valid audit chain. A changed record may be appended
#' when its historical chain remains valid; the new event then establishes the
#' changed record as the current audited state.
#'
#' @param x An initialized [PhysioCore::PhysioExperiment] object.
#' @param action Non-empty domain action. Names beginning with
#'   `physio_compliance.` are reserved.
#' @param actor Non-empty identifier for the responsible actor.
#' @param reason Non-empty reason for the action.
#' @param details A recursively named plain list containing supported atomic
#'   values. Named lists are sorted before hashing.
#' @param timestamp One finite `POSIXct` value.
#'
#' @return A modified copy of `x` with one additional event.
#' @export
appendAuditEvent <- function(
    x,
    action,
    actor,
    reason,
    details = list(),
    timestamp = Sys.time()) {
  .pc_assert_experiment(x)
  action <- .pc_assert_string(action, "action")
  actor <- .pc_assert_string(actor, "actor")
  reason <- .pc_assert_string(reason, "reason")
  .pc_canonical_details(details)
  if (startsWith(action, "physio_compliance.")) {
    .pc_abort("Actions beginning with `physio_compliance.` are reserved.")
  }
  .pc_assert_appendable(x)
  .pc_append_event_to_state(
    x = x,
    state = .pc_state(x),
    action = action,
    actor = actor,
    reason = reason,
    details = details,
    timestamp = timestamp
  )
}

.pc_empty_audit_trail <- function() {
  out <- data.frame(
    sequence = integer(),
    timestamp = character(),
    actor = character(),
    action = character(),
    reason = character(),
    record_hash = character(),
    previous_hash = character(),
    entry_hash = character(),
    hash_algorithm = character(),
    serialization_version = integer(),
    stringsAsFactors = FALSE
  )
  out$details <- I(list())
  class(out) <- c("audit_trail", "data.frame")
  out
}

#' Extract the compliance audit trail
#'
#' @param x A [PhysioCore::PhysioExperiment] object.
#'
#' @return An `audit_trail` data frame with a `details` list-column. An
#'   uninitialized object returns a zero-row trail.
#' @export
auditTrail <- function(x) {
  .pc_assert_experiment(x)
  state <- .pc_state(x)
  if (is.null(state)) {
    return(.pc_empty_audit_trail())
  }
  if (!is.list(state) || !identical(names(state),
                                    c("schema_version", "audit", "signatures")) ||
      !identical(state$schema_version, "1") || !is.list(state$audit) ||
      any(!vapply(state$audit, .pc_event_schema_valid, logical(1)))) {
    .pc_abort("The stored compliance audit trail is malformed.")
  }
  if (!length(state$audit)) {
    .pc_abort("The initialized audit trail has no genesis event.")
  }
  out <- data.frame(
    sequence = vapply(state$audit, `[[`, integer(1), "sequence"),
    timestamp = vapply(state$audit, `[[`, character(1), "timestamp"),
    actor = vapply(state$audit, `[[`, character(1), "actor"),
    action = vapply(state$audit, `[[`, character(1), "action"),
    reason = vapply(state$audit, `[[`, character(1), "reason"),
    record_hash = vapply(state$audit, `[[`, character(1), "record_hash"),
    previous_hash = vapply(state$audit, `[[`, character(1), "previous_hash"),
    entry_hash = vapply(state$audit, `[[`, character(1), "entry_hash"),
    hash_algorithm = vapply(
      state$audit, `[[`, character(1), "hash_algorithm"
    ),
    serialization_version = vapply(
      state$audit, `[[`, integer(1), "serialization_version"
    ),
    stringsAsFactors = FALSE
  )
  out$details <- I(lapply(state$audit, `[[`, "details"))
  class(out) <- c("audit_trail", "data.frame")
  out
}

#' Verify a compliance audit trail
#'
#' Verification reports tampering and malformed storage without modifying,
#' truncating, repairing, or rehashing the supplied object.
#'
#' A self-contained object cannot distinguish an internally consistent earlier
#' copy from the record as it existed at that time. Detect whole-object rollback
#' or removal of the current tail event by retaining each verified `head_hash`
#' in a validated external append-only store and comparing it on retrieval.
#'
#' @param x A [PhysioCore::PhysioExperiment] object.
#'
#' @return A `compliance_verification` object containing validity, deterministic
#'   issues, the verified head hash, current record hash, and event counts.
#' @export
verifyAuditTrail <- function(x) {
  .pc_verify_audit(x, compare_record = TRUE)
}

#' @export
as.data.frame.audit_trail <- function(x, ...) {
  out <- x
  class(out) <- "data.frame"
  out
}

#' @export
print.audit_trail <- function(x, ...) {
  cat(sprintf("<audit_trail> %d event%s\n", nrow(x),
              if (nrow(x) == 1L) "" else "s"))
  shown <- as.data.frame(x)
  shown$details <- vapply(
    shown$details,
    function(value) sprintf("<list:%d>", length(value)),
    character(1)
  )
  print(shown, row.names = FALSE, ...)
  invisible(x)
}

#' @export
as.data.frame.compliance_verification <- function(x, ...) {
  x$issues
}

#' @export
print.compliance_verification <- function(x, ...) {
  cat(sprintf(
    "<compliance_verification> %s; %d event%s; %d signature%s\n",
    if (isTRUE(x$valid)) "valid" else "invalid",
    x$n_events,
    if (x$n_events == 1L) "" else "s",
    x$n_signatures,
    if (x$n_signatures == 1L) "" else "s"
  ))
  if (nrow(x$issues)) {
    print(x$issues, row.names = FALSE, ...)
  }
  invisible(x)
}
