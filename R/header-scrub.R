.hs_supported <- c("edf", "brainvision", "snirf", "dicom")

.hs_format <- function(header, format) {
  if (!identical(format, "auto")) {
    return(format)
  }
  markers <- character()
  explicit <- attr(header, "format", exact = TRUE)
  if (is.character(explicit) && length(explicit) == 1L && !is.na(explicit)) {
    markers <- c(markers, tolower(explicit))
  }
  if (is.list(header) && identical(class(header), "list") &&
      "format" %in% names(header) &&
      is.character(header$format) && length(header$format) == 1L &&
      !is.na(header$format)) {
    markers <- c(markers, tolower(header$format))
  }
  classes <- tolower(class(header))
  for (candidate in .hs_supported) {
    if (any(classes %in% c(
        candidate, paste0(candidate, "_header"),
        paste0(candidate, "header")
      ))) {
      markers <- c(markers, candidate)
    }
  }
  markers <- unique(markers[markers %in% .hs_supported])
  if (length(markers) != 1L) {
    .pc_abort(
      "`format = \"auto\"` requires exactly one explicit supported format marker."
    )
  }
  markers[[1L]]
}

.hs_validate_header <- function(header, path = "header", top = FALSE) {
  if (.di_forbidden(header) ||
      (isS4(header) && !methods::is(header, "DataFrame"))) {
    .pc_abort(sprintf("`%s` contains an unsupported object graph.", path))
  }
  if (methods::is(header, "DataFrame") || is.data.frame(header)) {
    if (is.null(names(header)) && ncol(header)) {
      .pc_abort(sprintf("`%s` must have named fields.", path))
    }
    for (name in names(header)) {
      .hs_validate_header(
        header[[name]], .di_path_name(name, path), top = FALSE
      )
    }
    return(invisible(header))
  }
  if (is.list(header)) {
    header_classes <- tolower(class(header))
    recognized_class <- any(vapply(.hs_supported, function(format) {
      any(header_classes %in% c(
        format, paste0(format, "_header"), paste0(format, "header")
      ))
    }, logical(1)))
    if (!identical(class(header), "list") &&
        !(top && recognized_class)) {
      .pc_abort(sprintf("`%s` contains an unsupported list class.", path))
    }
    .pc_validate_names(header, path, required = top && length(header) > 0L)
    nms <- names(header)
    if (is.null(nms)) {
      for (i in seq_along(header)) {
        .hs_validate_header(
          header[[i]], paste0(path, "[[", i, "]]"), top = FALSE
        )
      }
    } else {
      for (name in nms) {
        .hs_validate_header(
          header[[name]], .di_path_name(name, path), top = FALSE
        )
      }
    }
  }
  invisible(header)
}

.hs_safe_token <- function(subject_token) {
  if (is.null(subject_token)) {
    return(NULL)
  }
  token <- .ps_utf8_scalar(subject_token, "subject_token")
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,127}$", token)) {
    .pc_abort(
      "`subject_token` must contain only safe identifier characters."
    )
  }
  token
}

.hs_add_action <- function(state, path, category, action, value) {
  .di_add_action(state, path, category, action, value)
}

.hs_add_manual <- function(state, path, reason) {
  .di_add_manual(state, path, reason)
}

.hs_date <- function(value, path, subject_id, date_key) {
  if (is.null(subject_id) && is.null(date_key)) {
    return(list(value = NULL, remove = TRUE, action = "remove"))
  }
  if (is.null(subject_id) || is.null(date_key)) {
    .pc_abort(
      "`date_key` and `subject_id` must be supplied together for header dates."
    )
  }
  shifted <- .di_shift_date(value, subject_id, date_key, path)
  list(value = shifted, remove = FALSE, action = "date_shift")
}

.hs_free_text <- function(value, path, free_text) {
  if (identical(free_text, "error")) {
    .pc_abort(sprintf(
      "Detected free-text header content at `%s`.",
      path
    ))
  }
  list(value = NULL, remove = TRUE, action = "drop")
}

.hs_replacement <- function(value, subject_token, prefix = NULL) {
  if (is.null(subject_token)) {
    return(list(value = NULL, remove = TRUE, action = "remove"))
  }
  replacement <- if (is.null(prefix)) {
    subject_token
  } else {
    paste0(prefix, subject_token)
  }
  list(
    value = rep(replacement, length(value)),
    remove = FALSE,
    action = "replace"
  )
}

.hs_safe_link <- function(name, format) {
  normalized <- .di_normalize(name)
  if (identical(format, "brainvision")) {
    if (normalized == "datafile") {
      return("recording.eeg")
    }
    if (normalized == "markerfile") {
      return("recording.vmrk")
    }
    if (normalized %in% c("headerfile", "vhdrfile")) {
      return("recording.vhdr")
    }
  }
  NULL
}

.hs_decimal_128 <- function(value) {
  digits <- 0L
  for (byte in as.integer(value)) {
    carry <- byte
    for (i in seq_along(digits)) {
      current <- digits[[i]] * 256L + carry
      digits[[i]] <- current %% 10L
      carry <- current %/% 10L
    }
    while (carry > 0L) {
      digits <- c(digits, carry %% 10L)
      carry <- carry %/% 10L
    }
  }
  paste(rev(digits), collapse = "")
}

.hs_new_uid <- function() {
  paste0("2.25.", .hs_decimal_128(openssl::rand_bytes(16L)))
}

.hs_replace_uids <- function(value, uid_map, path) {
  if (!is.character(value) || is.factor(value)) {
    .pc_abort(sprintf("`%s` must contain character UID values.", path))
  }
  out <- value
  active <- !is.na(value)
  for (source in unique(value[active])) {
    if (!nzchar(source) || nchar(source, type = "bytes") > 64L ||
        !grepl("^[0-9]+(\\.[0-9]+)+$", source)) {
      .pc_abort(sprintf("`%s` contains a malformed UID.", path))
    }
    if (!exists(source, envir = uid_map, inherits = FALSE)) {
      assign(source, .hs_new_uid(), envir = uid_map)
    }
    out[active & value == source] <- get(
      source, envir = uid_map, inherits = FALSE
    )
  }
  out
}

.hs_dicom_private <- function(name) {
  normalized <- .di_normalize(name)
  if (grepl("^[0-9a-f]{8}$", normalized)) {
    group <- suppressWarnings(strtoi(substr(normalized, 1L, 4L), base = 16L))
    if (!is.na(group) && group %% 2L == 1L) {
      return(TRUE)
    }
  }
  grepl("^private", normalized) || grepl("^oddgroup", normalized)
}

.hs_dicom_pixel <- function(name) {
  .di_normalize(name) %in% c(
    "pixeldata", "overlaydata", "burnedinannotation", "recognizablevisualfeatures"
  )
}

.hs_dicom_uid <- function(name) {
  grepl("uid$", .di_normalize(name))
}

.hs_action_for <- function(name, value, format, path, subject_token,
                           subject_id, date_key, free_text, policy, uid_map,
                           state) {
  normalized <- .di_normalize(name)

  if (identical(format, "dicom") && .hs_dicom_private(name)) {
    .hs_add_manual(
      state, path,
      "DICOM private attributes require manual inspection and policy review."
    )
    return(list(value = value, remove = FALSE))
  }
  if (identical(format, "dicom") && .hs_dicom_pixel(name)) {
    .hs_add_manual(
      state, path,
      paste(
        "DICOM pixel, overlay, or burned-in annotation content was not",
        "inspected or rewritten."
      )
    )
    return(list(value = value, remove = FALSE))
  }
  if (identical(format, "dicom") && .hs_dicom_uid(name)) {
    replacement <- .hs_replace_uids(value, uid_map, path)
    .hs_add_action(state, path, "other_unique", "replace_uid", value)
    return(list(value = replacement, remove = FALSE))
  }

  safe_link <- .hs_safe_link(name, format)
  if (!is.null(safe_link)) {
    .hs_add_action(state, path, "other_unique", "safe_basename", value)
    return(list(
      value = rep(safe_link, length(value)),
      remove = FALSE
    ))
  }

  subject_fields <- switch(
    format,
    edf = c("patientid", "patientcode", "patientname"),
    brainvision = c("subject", "subjectid", "patient", "patientid"),
    snirf = c("subjectid"),
    dicom = c(
      "patientid", "patientname", "otherpatientids",
      "otherpatientnames", "clinicaltrialsubjectid"
    )
  )
  if (normalized %in% subject_fields) {
    result <- .hs_replacement(value, subject_token)
    .hs_add_action(
      state, path, "other_unique", result$action, value
    )
    return(result)
  }

  if (identical(format, "edf") && normalized == "recordingid") {
    result <- .hs_replacement(value, subject_token, prefix = "recording_")
    .hs_add_action(state, path, "other_unique", result$action, value)
    return(result)
  }

  date_fields <- switch(
    format,
    edf = c("startdate", "starttime", "recordingdate", "birthdate"),
    brainvision = c(
      "acquisitiondate", "acquisitiontime", "measurementdate",
      "measurementtime", "timestamp"
    ),
    snirf = c("measurementdate", "measurementtime", "measurementdatetime"),
    dicom = c(
      "patientbirthdate", "patientbirthtime", "studydate", "studytime",
      "seriesdate", "seriestime", "acquisitiondate", "acquisitiontime",
      "contentdate", "contenttime", "admissiondate", "dischargedate"
    )
  )
  if (normalized %in% date_fields) {
    if (grepl("time$", normalized) && !grepl("datetime$", normalized)) {
      result <- list(value = NULL, remove = TRUE, action = "remove")
    } else {
      dicom_date <- identical(format, "dicom") &&
        is.character(value) &&
        all(is.na(value) | grepl("^[0-9]{8}$", value))
      if (dicom_date) {
        parsed <- as.Date(value, format = "%Y%m%d")
        if (any(!is.na(value) & is.na(parsed))) {
          .pc_abort(sprintf(
            "`%s` contains a malformed DICOM date.",
            path
          ))
        }
        result <- .hs_date(parsed, path, subject_id, date_key)
        if (!result$remove) {
          result$value <- format(result$value, "%Y%m%d")
          result$value[is.na(value)] <- NA_character_
        }
      } else {
        result <- .hs_date(value, path, subject_id, date_key)
      }
    }
    .hs_add_action(state, path, "dates_ages", result$action, value)
    return(result)
  }

  path_fields <- switch(
    format,
    edf = c("originalpath", "filepath", "filename"),
    brainvision = c("originalpath", "filepath", "directory"),
    snirf = c("originalpath", "filepath", "filename"),
    dicom = c("originalpath", "filepath", "filename")
  )
  if (normalized %in% path_fields) {
    .hs_add_action(state, path, "other_unique", "remove", value)
    return(list(value = NULL, remove = TRUE))
  }

  free_fields <- switch(
    format,
    edf = c(
      "recordingid", "additionalpatientinformation",
      "additionalrecordinginformation", "recordinginformation"
    ),
    brainvision = c(
      "comments", "comment", "markerdescription", "description",
      "experimenter"
    ),
    snirf = c(
      "measurementsystem", "operator", "operatorname", "description",
      "comments"
    ),
    dicom = c(
      "patientcomments", "additionalpatienthistory",
      "studycomments", "seriesdescription", "studydescription",
      "institutionname", "institutionaddress", "referringphysicianname",
      "performingphysicianname", "operatorsname"
    )
  )
  if (normalized %in% free_fields) {
    result <- .hs_free_text(value, path, free_text)
    .hs_add_action(state, path, "other_unique", result$action, value)
    return(result)
  }

  match <- .di_match(name, policy)
  if (!is.null(match)) {
    if (match$kind == "free_text") {
      result <- .hs_free_text(value, path, free_text)
      .hs_add_action(state, path, match$category, result$action, value)
      return(result)
    }
    if (match$kind %in% c("biometric", "image")) {
      if (identical(format, "dicom")) {
        .hs_add_manual(
          state, path,
          "DICOM image or biometric content requires manual inspection."
        )
        return(list(value = value, remove = FALSE))
      }
      .hs_add_action(state, path, match$category, "drop", value)
      return(list(value = NULL, remove = TRUE))
    }
    if (match$category == "dates_ages") {
      result <- .hs_date(value, path, subject_id, date_key)
      .hs_add_action(state, path, match$category, result$action, value)
      return(result)
    }
    result <- .hs_replacement(value, subject_token)
    .hs_add_action(state, path, match$category, result$action, value)
    return(result)
  }
  NULL
}

.hs_walk <- function(header, format, path, subject_token, subject_id,
                     date_key, free_text, policy, uid_map, state) {
  if (methods::is(header, "DataFrame") || is.data.frame(header)) {
    for (name in names(header)) {
      field_path <- .di_path_name(name, path)
      result <- .hs_action_for(
        name, header[[name]], format, field_path, subject_token,
        subject_id, date_key, free_text, policy, uid_map, state
      )
      if (!is.null(result)) {
        if (result$remove) {
          header[[name]] <- NULL
        } else {
          header[[name]] <- result$value
        }
      } else if (is.list(header[[name]]) ||
                 methods::is(header[[name]], "DataFrame")) {
        header[[name]] <- .hs_walk(
          header[[name]], format, field_path, subject_token, subject_id,
          date_key, free_text, policy, uid_map, state
        )
      } else if (is.character(header[[name]]) ||
                 is.factor(header[[name]])) {
        .hs_add_manual(
          state, field_path,
          "Unclassified retained header text requires manual identifier review."
        )
      }
    }
    return(header)
  }

  if (is.list(header)) {
    if (is.null(names(header))) {
      for (i in seq_along(header)) {
        if (is.list(header[[i]]) ||
            methods::is(header[[i]], "DataFrame")) {
          header[[i]] <- .hs_walk(
            header[[i]], format, paste0(path, "[[", i, "]]"),
            subject_token, subject_id, date_key, free_text, policy,
            uid_map, state
          )
        }
      }
      return(header)
    }
    for (name in names(header)) {
      field_path <- .di_path_name(name, path)
      result <- .hs_action_for(
        name, header[[name]], format, field_path, subject_token,
        subject_id, date_key, free_text, policy, uid_map, state
      )
      if (!is.null(result)) {
        if (result$remove) {
          header[[name]] <- NULL
        } else {
          header[[name]] <- result$value
        }
      } else if (is.list(header[[name]]) ||
                 methods::is(header[[name]], "DataFrame")) {
        header[[name]] <- .hs_walk(
          header[[name]], format, field_path, subject_token, subject_id,
          date_key, free_text, policy, uid_map, state
        )
      } else if (is.character(header[[name]]) ||
                 is.factor(header[[name]])) {
        .hs_add_manual(
          state, field_path,
          "Unclassified retained header text requires manual identifier review."
        )
      }
    }
  }
  header
}

.hs_dicom_finalize <- function(header, state) {
  if (methods::is(header, "DataFrame") || is.data.frame(header)) {
    header[["PatientIdentityRemoved"]] <- rep("YES", nrow(header))
    header[["DeidentificationMethod"]] <- rep(
      "PhysioCompliance conservative structured-header subset",
      nrow(header)
    )
  } else {
    header[["PatientIdentityRemoved"]] <- "YES"
    header[["DeidentificationMethod"]] <-
      "PhysioCompliance conservative structured-header subset"
  }
  .hs_add_action(
    state, "header$PatientIdentityRemoved", "other_unique",
    "set_identity_removed", "YES"
  )
  .hs_add_manual(
    state, "header",
    paste(
      "This function does not parse DICOM, inspect pixels, or establish",
      "a PS3.15 conformance statement."
    )
  )
  header
}

#' Scrub a parsed physiological or imaging header
#'
#' Scrubs a named structured header without parsing or rewriting a binary file.
#' The DICOM-adjacent mode implements a documented conservative subset, not a
#' DICOM PS3.15 conformance statement. DICOM private attributes, pixels,
#' overlays, and burned-in annotations always remain manual-review items.
#'
#' @param header A named plain list, data frame, or `DataFrame`.
#' @param format Explicit format, or `"auto"` when the object carries exactly
#'   one explicit supported format marker.
#' @param subject_token Optional safe replacement token.
#' @param date_key Caller-owned raw date-shifting key.
#' @param subject_id Subject identifier used only to derive a date offset.
#' @param free_text Refuse or drop detected free-text fields.
#'
#' @return A `header_scrub` object containing the scrubbed header and reports.
#' @export
headerScrub <- function(
    header,
    format = c("auto", "edf", "brainvision", "snirf", "dicom"),
    subject_token = NULL,
    date_key = NULL,
    subject_id = NULL,
    free_text = c("error", "drop")) {
  format <- match.arg(format)
  format <- .hs_format(header, format)
  free_text <- match.arg(free_text)
  subject_token <- .hs_safe_token(subject_token)
  if (xor(is.null(date_key), is.null(subject_id))) {
    .pc_abort("`date_key` and `subject_id` must be supplied together.")
  }
  if (!is.null(date_key)) {
    date_key <- .ps_key(date_key)
    subject_id <- .ps_identifiers(subject_id)
    if (length(subject_id) != 1L || is.na(subject_id)) {
      .pc_abort("`subject_id` must be one non-missing identifier for a header.")
    }
  }
  .hs_validate_header(header, top = TRUE)
  state <- .di_initialize_state()
  policy <- safeHarborPolicy(
    free_text = "drop",
    biometric_data = "drop"
  )
  uid_map <- new.env(parent = emptyenv(), hash = TRUE)
  scrubbed <- .hs_walk(
    header, format, "header", subject_token, subject_id, date_key,
    free_text, policy, uid_map, state
  )
  if (identical(format, "dicom")) {
    scrubbed <- .hs_dicom_finalize(scrubbed, state)
  }
  structure(list(
    header = scrubbed,
    report = .di_rows(state$actions, .di_empty_actions),
    format = format,
    manual_review = .di_rows(state$manual, .di_empty_manual)
  ), class = "header_scrub")
}

#' @export
print.header_scrub <- function(x, ...) {
  cat(sprintf(
    "<header_scrub> format=%s; actions=%d; manual-review=%d\n",
    x$format, nrow(x$report), nrow(x$manual_review)
  ))
  invisible(x)
}
