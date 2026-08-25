.ds_subject <- function(subject_id) {
  value <- .ps_identifiers(subject_id)
  if (length(value) != 1L || is.na(value)) {
    .pc_abort("`subject_id` must be one non-missing identifier.")
  }
  value
}

.ds_records <- function(records, subject_id) {
  if (!is.list(records) || !identical(class(records), "list")) {
    .pc_abort("`records` must be a plain named list.")
  }
  .pc_validate_names(records, "records", required = TRUE)
  if (length(records)) {
    for (name in names(records)) {
      .di_path_name(name, "records")
      if (grepl(subject_id, name, fixed = TRUE)) {
        .pc_abort(
          "Record names must not contain the requested subject identifier."
        )
      }
    }
  }
  records
}

.ds_safe_graph <- function(value, path) {
  if (.di_forbidden(value) || inherits(value, "refClass")) {
    .pc_abort(sprintf("`%s` contains mutable or unsupported state.", path))
  }
  if (isS4(value)) {
    for (slot in methods::slotNames(value)) {
      .ds_safe_graph(
        methods::slot(value, slot),
        paste0(path, "@", slot)
      )
    }
    return(invisible(value))
  }
  if (is.list(value)) {
    for (i in seq_along(value)) {
      child <- if (!is.null(names(value)) && nzchar(names(value)[[i]])) {
        .di_path_name(names(value)[[i]], path)
      } else {
        paste0(path, "[[", i, "]]")
      }
      .ds_safe_graph(value[[i]], child)
    }
  }
  invisible(value)
}

.ds_locate <- function(records, locate) {
  if (!is.function(locate)) {
    .pc_abort("`locate` must be a function.")
  }
  out <- rep(NA_character_, length(records))
  for (i in seq_along(records)) {
    value <- tryCatch(
      locate(records[[i]]),
      error = function(e) structure(FALSE, callback_error = TRUE)
    )
    if (isTRUE(attr(value, "callback_error", exact = TRUE))) {
      .pc_abort(sprintf(
        "The `locate` callback failed for `records$%s`.",
        names(records)[[i]]
      ))
    }
    if (length(value) != 1L || (!is.character(value) && !is.na(value))) {
      .pc_abort(sprintf(
        "The `locate` callback returned an invalid result for `records$%s`.",
        names(records)[[i]]
      ))
    }
    if (is.na(value)) {
      next
    }
    value <- .ps_utf8_scalar(value, "locate result")
    out[[i]] <- value
  }
  out
}

.ds_serialized <- function(record, path) {
  .ds_safe_graph(record, path)
  tryCatch(
    .pc_serialize(record),
    error = function(e) {
      .pc_abort(sprintf("`%s` could not be serialized safely.", path))
    }
  )
}

.ds_provenance_count <- function(record) {
  if (methods::is(record, "PhysioExperiment")) {
    provenance <- S4Vectors::metadata(record)[["provenance"]]
    return(as.integer(length(if (is.null(provenance)) list() else provenance)))
  }
  if (methods::is(record, "MultiRatePhysioExperiment")) {
    return(as.integer(sum(vapply(
      as.list(record@streams), .ds_provenance_count, integer(1)
    ))))
  }
  if (methods::is(record, "PhysioLongitudinal")) {
    return(as.integer(sum(vapply(
      as.list(record@sessions), .ds_provenance_count, integer(1)
    ))))
  }
  0L
}

.ds_audit_head <- function(record) {
  if (methods::is(record, "PhysioExperiment")) {
    state <- .pc_state(record)
    if (is.null(state) || !length(state$audit)) {
      return(NA_character_)
    }
    return(state$audit[[length(state$audit)]]$entry_hash)
  }
  children <- if (methods::is(record, "MultiRatePhysioExperiment")) {
    as.list(record@streams)
  } else if (methods::is(record, "PhysioLongitudinal")) {
    as.list(record@sessions)
  } else {
    list()
  }
  if (!length(children)) {
    return(NA_character_)
  }
  heads <- vapply(children, .ds_audit_head, character(1))
  if (all(is.na(heads))) {
    return(NA_character_)
  }
  .pc_hash(as.list(heads))
}

.ds_manifest_row <- function(record, name, serialized, timestamp) {
  data.frame(
    record_name = name,
    class = paste(class(record), collapse = "/"),
    byte_size = as.double(length(serialized)),
    sha256 = .pc_hash_raw(serialized),
    provenance_count = .ds_provenance_count(record),
    audit_head = .ds_audit_head(record),
    exported_at = timestamp,
    stringsAsFactors = FALSE
  )
}

.ds_empty_manifest <- function() {
  data.frame(
    record_name = character(), class = character(), byte_size = double(),
    sha256 = character(), provenance_count = integer(),
    audit_head = character(), exported_at = character(),
    stringsAsFactors = FALSE
  )
}

.ds_deep_copy <- function(serialized, path) {
  tryCatch(
    unserialize(serialized),
    error = function(e) {
      .pc_abort(sprintf("`%s` could not be copied safely.", path))
    }
  )
}

#' Prepare a data-subject access export
#'
#' Selects records through a caller-owned identity matcher and returns deep
#' copies with a content manifest. Identity verification, the scope and format
#' of an Article 15 response, third-party rights, and secure delivery remain
#' controller responsibilities.
#'
#' @param records A named plain list of records.
#' @param subject_id One requested subject identifier.
#' @param locate A callback called exactly once per record. It returns one
#'   subject identifier or `NA`.
#'
#' @return A `data_subject_export` object.
#' @export
dataSubjectExport <- function(records, subject_id, locate) {
  subject_id <- .ds_subject(subject_id)
  records <- .ds_records(records, subject_id)
  located <- .ds_locate(records, locate)
  selected <- which(!is.na(located) & located == subject_id)
  timestamp <- .pc_timestamp(Sys.time())
  copies <- list()
  manifest <- list()
  for (i in selected) {
    path <- paste0("records$", names(records)[[i]])
    serialized <- .ds_serialized(records[[i]], path)
    copies[[names(records)[[i]]]] <- .ds_deep_copy(serialized, path)
    manifest[[length(manifest) + 1L]] <- .ds_manifest_row(
      records[[i]], names(records)[[i]], serialized, timestamp
    )
  }
  manifest <- if (length(manifest)) {
    out <- do.call(rbind, manifest)
    rownames(out) <- NULL
    out
  } else {
    .ds_empty_manifest()
  }
  structure(list(
    records = copies,
    manifest = manifest,
    controller_responsibility = paste(
      "Identity verification, response scope, third-party rights,",
      "and secure delivery require controller review."
    )
  ), class = "data_subject_export")
}

.ds_reason <- function(reason, subject_id) {
  value <- .ps_utf8_scalar(reason, "reason")
  if (grepl(subject_id, value, fixed = TRUE)) {
    .pc_abort("`reason` must not contain the requested subject identifier.")
  }
  value
}

.ds_retain <- function(record, retain, name) {
  if (is.null(retain)) {
    return(NA_character_)
  }
  if (!is.function(retain)) {
    .pc_abort("`retain` must be NULL or a function.")
  }
  value <- tryCatch(
    retain(record),
    error = function(e) structure(FALSE, callback_error = TRUE)
  )
  if (isTRUE(attr(value, "callback_error", exact = TRUE))) {
    .pc_abort(sprintf(
      "The `retain` callback failed for `records$%s`.",
      name
    ))
  }
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(NA_character_)
  }
  if (!is.character(value) || length(value) != 1L ||
      is.na(value) || !nzchar(trimws(value))) {
    .pc_abort(sprintf(
      "The `retain` callback returned an invalid result for `records$%s`.",
      name
    ))
  }
  converted <- iconv(value, from = "", to = "UTF-8", sub = NA_character_)
  if (is.na(converted) || grepl("[[:cntrl:]]", converted)) {
    .pc_abort(sprintf(
      "The `retain` callback returned an invalid result for `records$%s`.",
      name
    ))
  }
  enc2utf8(converted)
}

.ds_plan_table <- function(records, selected, retain, subject_id) {
  rows <- list()
  serialized <- vector("list", length(records))
  for (i in selected) {
    path <- paste0("records$", names(records)[[i]])
    serialized[[i]] <- .ds_serialized(records[[i]], path)
    retain_reason <- .ds_retain(records[[i]], retain, names(records)[[i]])
    if (!is.na(retain_reason) &&
        grepl(subject_id, retain_reason, fixed = TRUE)) {
      .pc_abort(
        "A retention reason must not contain the requested subject identifier."
      )
    }
    rows[[length(rows) + 1L]] <- data.frame(
      record_name = names(records)[[i]],
      pre_erasure_digest = .pc_hash_raw(serialized[[i]]),
      decision = if (is.na(retain_reason)) "erase" else "retain",
      retention_reason = retain_reason,
      stringsAsFactors = FALSE
    )
  }
  plan <- if (length(rows)) {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  } else {
    data.frame(
      record_name = character(), pre_erasure_digest = character(),
      decision = character(), retention_reason = character(),
      stringsAsFactors = FALSE
    )
  }
  list(plan = plan, serialized = serialized)
}

.ds_plan_digest <- function(plan, reason) {
  .pc_hash(list(
    schema_version = "1",
    records = unclass(plan),
    reason = reason
  ))
}

.ds_append_erasure_event <- function(record, plan_digest, reason,
                                     timestamp) {
  if (methods::is(record, "MultiRatePhysioExperiment")) {
    for (i in seq_along(record@streams)) {
      linked <- .ds_append_erasure_event(
        record@streams[[i]], plan_digest, reason, timestamp
      )
      record@streams[[i]] <- linked$record
    }
    return(list(record = record, audit_head = .ds_audit_head(record)))
  }
  if (methods::is(record, "PhysioLongitudinal")) {
    for (i in seq_along(record@sessions)) {
      linked <- .ds_append_erasure_event(
        record@sessions[[i]], plan_digest, reason, timestamp
      )
      record@sessions[[i]] <- linked$record
    }
    return(list(record = record, audit_head = .ds_audit_head(record)))
  }
  if (!methods::is(record, "PhysioExperiment") ||
      is.null(.pc_state(record))) {
    return(list(record = record, audit_head = NA_character_))
  }
  .pc_assert_appendable(record)
  record <- .pc_append_event_to_state(
    x = record,
    state = .pc_state(record),
    action = "physio_compliance.data_subject_erasure",
    actor = "external_authorization_callback",
    reason = reason,
    details = list(
      plan_digest = plan_digest,
      record_count = 1L
    ),
    timestamp = timestamp
  )
  list(record = record, audit_head = .ds_audit_head(record))
}

.ds_receipt_empty <- function() {
  data.frame(
    record_name = character(), pre_erasure_digest = character(),
    status = character(), retention_reason = character(),
    audit_head = character(), stringsAsFactors = FALSE
  )
}

#' Plan or apply a data-subject erasure
#'
#' Builds a no-content plan before authorization. Applying the plan changes
#' only the returned in-memory collection; it does not delete files, databases,
#' backups, replicas, caches, or remote systems. Identity verification,
#' applicable legal bases, Article 17 exceptions, retention obligations, and
#' operational deletion remain controller responsibilities.
#'
#' @param records A named plain list of records.
#' @param subject_id One requested subject identifier.
#' @param locate A callback called exactly once per record.
#' @param authorize A callback invoked exactly once in apply mode with the plan.
#' @param retain Optional callback returning a non-empty retention reason or
#'   `NULL`/`NA` for erasure.
#' @param reason Non-empty operational reason. It must not contain the subject
#'   identifier.
#' @param mode Build a no-op plan or apply an authorized plan.
#'
#' @return A `data_subject_erasure` plan or applied result.
#' @export
dataSubjectErase <- function(
    records,
    subject_id,
    locate,
    authorize,
    retain = NULL,
    reason,
    mode = c("plan", "apply")) {
  mode <- match.arg(mode)
  subject_id <- .ds_subject(subject_id)
  reason <- .ds_reason(reason, subject_id)
  records <- .ds_records(records, subject_id)
  if (!is.function(authorize)) {
    .pc_abort("`authorize` must be a function.")
  }
  located <- .ds_locate(records, locate)
  selected <- which(!is.na(located) & located == subject_id)
  built <- .ds_plan_table(records, selected, retain, subject_id)
  plan_digest <- .ds_plan_digest(built$plan, reason)

  if (identical(mode, "plan")) {
    return(structure(list(
      mode = "plan",
      plan = built$plan,
      reason = reason,
      plan_digest = plan_digest,
      authorized = FALSE,
      operational_follow_up = paste(
        "Files, databases, backups, replicas, caches, and remote systems",
        "are outside this in-memory plan."
      )
    ), class = "data_subject_erasure"))
  }

  authorized <- tryCatch(
    authorize(built$plan),
    error = function(e) structure(FALSE, callback_error = TRUE)
  )
  if (isTRUE(attr(authorized, "callback_error", exact = TRUE))) {
    .pc_abort("The `authorize` callback failed; no erasure was applied.")
  }
  if (!isTRUE(authorized)) {
    .pc_abort("Erasure was not authorized; no erasure was applied.")
  }

  working <- records
  receipt <- list()
  timestamp <- Sys.time()
  for (row in seq_len(nrow(built$plan))) {
    name <- built$plan$record_name[[row]]
    i <- match(name, names(working))
    if (identical(built$plan$decision[[row]], "retain")) {
      receipt[[length(receipt) + 1L]] <- data.frame(
        record_name = name,
        pre_erasure_digest = built$plan$pre_erasure_digest[[row]],
        status = "retained",
        retention_reason = built$plan$retention_reason[[row]],
        audit_head = .ds_audit_head(working[[i]]),
        stringsAsFactors = FALSE
      )
      next
    }
    linked <- .ds_append_erasure_event(
      working[[i]], plan_digest, reason, timestamp
    )
    receipt[[length(receipt) + 1L]] <- data.frame(
      record_name = name,
      pre_erasure_digest = built$plan$pre_erasure_digest[[row]],
      status = "erased",
      retention_reason = NA_character_,
      audit_head = linked$audit_head,
      stringsAsFactors = FALSE
    )
    working[[i]] <- NULL
  }
  receipt <- if (length(receipt)) {
    out <- do.call(rbind, receipt)
    rownames(out) <- NULL
    out
  } else {
    .ds_receipt_empty()
  }
  structure(list(
    records = working,
    receipt = receipt,
    plan_digest = plan_digest,
    authorized = TRUE,
    operational_follow_up = paste(
      "Files, databases, backups, replicas, caches, and remote systems",
      "require separate controller-controlled deletion."
    )
  ), class = "data_subject_erasure")
}

#' @export
print.data_subject_export <- function(x, ...) {
  cat(sprintf(
    "<data_subject_export> records=%d; manifest=%d\n",
    length(x$records), nrow(x$manifest)
  ))
  invisible(x)
}

#' @export
print.data_subject_erasure <- function(x, ...) {
  if (identical(x$mode, "plan")) {
    cat(sprintf(
      "<data_subject_erasure> mode=plan; records=%d; authorized=no\n",
      nrow(x$plan)
    ))
  } else {
    counts <- table(x$receipt$status)
    cat(sprintf(
      "<data_subject_erasure> mode=apply; erased=%d; retained=%d; authorized=yes\n",
      if ("erased" %in% names(counts)) counts[["erased"]] else 0L,
      if ("retained" %in% names(counts)) counts[["retained"]] else 0L
    ))
  }
  invisible(x)
}
