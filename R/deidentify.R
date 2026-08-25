.di_categories <- c(
  "names", "substate_geography", "dates_ages", "telephone", "fax",
  "email", "ssn", "medical_record", "health_plan", "account",
  "certificate_license", "vehicle", "device", "url", "ip_address",
  "biometric", "full_face_image", "other_unique"
)

.di_structural_fields <- c(
  "session_id", "visit_label", "days_from_baseline", "condition",
  "label", "type", "side", "dx", "onset", "duration", "sampling_rate"
)

.di_normalize <- function(value) {
  value <- iconv(value, from = "", to = "UTF-8", sub = NA_character_)
  if (anyNA(value)) {
    .pc_abort("Field aliases must be valid UTF-8.")
  }
  tolower(gsub("[^[:alnum:]]", "", value))
}

.di_dictionary <- function() {
  path <- system.file(
    "extdata", "safe-harbor-fields.csv",
    package = "PhysioCompliance"
  )
  if (!nzchar(path)) {
    .pc_abort("The Safe Harbor field dictionary is unavailable.")
  }
  dictionary <- utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  )
  required <- c("category", "alias", "kind")
  if (!identical(names(dictionary), required) ||
      !all(dictionary$category %in% .di_categories)) {
    .pc_abort("The Safe Harbor field dictionary is malformed.")
  }
  dictionary$normalized <- .di_normalize(dictionary$alias)
  if (any(!nzchar(dictionary$normalized)) ||
      anyDuplicated(dictionary$normalized)) {
    .pc_abort("The Safe Harbor field dictionary contains duplicate aliases.")
  }
  dictionary
}

.di_additional_fields <- function(additional_fields, dictionary) {
  if (!is.list(additional_fields) ||
      !identical(class(additional_fields), "list")) {
    .pc_abort("`additional_fields` must be a plain named list.")
  }
  if (!length(additional_fields)) {
    return(dictionary)
  }
  .pc_validate_names(additional_fields, "additional_fields", required = TRUE)
  unknown <- setdiff(names(additional_fields), .di_categories)
  if (length(unknown)) {
    .pc_abort("`additional_fields` contains an unknown category.")
  }
  rows <- lapply(seq_along(additional_fields), function(i) {
    aliases <- additional_fields[[i]]
    if (!is.character(aliases) || is.factor(aliases) ||
        anyNA(aliases) || any(!nzchar(trimws(aliases)))) {
      .pc_abort(
        "Every additional field alias must be a non-empty character value."
      )
    }
    category <- names(additional_fields)[[i]]
    kind <- if (identical(category, "biometric")) {
      "biometric"
    } else if (identical(category, "full_face_image")) {
      "image"
    } else if (identical(category, "dates_ages")) {
      "date"
    } else {
      "identifier"
    }
    data.frame(
      category = rep(category, length(aliases)),
      alias = aliases,
      kind = rep(kind, length(aliases)),
      stringsAsFactors = FALSE
    )
  })
  additions <- do.call(rbind, rows)
  additions$normalized <- .di_normalize(additions$alias)
  combined <- rbind(dictionary, additions)
  if (any(!nzchar(combined$normalized)) ||
      anyDuplicated(combined$normalized)) {
    .pc_abort("Field aliases must be unique after normalization.")
  }
  combined
}

.di_policy <- function(label, additional_fields, date_action, free_text,
                       biometric_data) {
  dictionary <- .di_additional_fields(additional_fields, .di_dictionary())
  aliases <- lapply(.di_categories, function(category) {
    dictionary$alias[dictionary$category == category]
  })
  if (identical(label, "safe_harbor_candidate")) {
    action <- rep("remove", length(.di_categories))
    action[.di_categories == "dates_ages"] <- date_action
    action[.di_categories == "biometric"] <- biometric_data
    action[.di_categories == "full_face_image"] <- biometric_data
  } else {
    action <- rep("pseudonymize", length(.di_categories))
    action[.di_categories == "dates_ages"] <- "date_shift"
    action[.di_categories == "biometric"] <- "error"
    action[.di_categories == "full_face_image"] <- "error"
  }
  out <- data.frame(
    policy_version = rep("1", length(.di_categories)),
    label = rep(label, length(.di_categories)),
    category = .di_categories,
    action = action,
    source = rep(
      "HHS-OCR-deidentification-guidance",
      length(.di_categories)
    ),
    stringsAsFactors = FALSE
  )
  out$field_aliases <- I(aliases)
  out <- out[c(
    "policy_version", "label", "category", "field_aliases", "action", "source"
  )]
  attr(out, "alias_table") <- dictionary
  attr(out, "free_text") <- free_text
  attr(out, "biometric_data") <- biometric_data
  class(out) <- c("deidentification_policy", "data.frame")
  out
}

#' Conservative de-identification policies
#'
#' Constructs a versioned field policy. These policies are technical controls,
#' not legal determinations. A `safe_harbor_candidate` still requires the
#' no-actual-knowledge and organizational review required by the applicable
#' workflow. Pseudonymized data remain personal data when separately held
#' information permits re-identification.
#'
#' @param additional_fields A named plain list mapping canonical category IDs
#'   to deployment-specific aliases.
#' @param date_action Replace person-related dates with year-only values or
#'   remove them.
#' @param free_text Refuse, drop, or (for pseudonymized data only) retain
#'   detected free text.
#' @param biometric_data Refuse or drop detected biometric/image content.
#'
#' @return A `deidentification_policy` data frame.
#' @export
safeHarborPolicy <- function(
    additional_fields = list(),
    date_action = c("year", "remove"),
    free_text = c("error", "drop"),
    biometric_data = c("error", "drop")) {
  .di_policy(
    "safe_harbor_candidate",
    additional_fields = additional_fields,
    date_action = match.arg(date_action),
    free_text = match.arg(free_text),
    biometric_data = match.arg(biometric_data)
  )
}

#' @rdname safeHarborPolicy
#' @export
pseudonymizedPolicy <- function(
    additional_fields = list(),
    free_text = c("error", "drop", "retain")) {
  .di_policy(
    "pseudonymized",
    additional_fields = additional_fields,
    date_action = "date_shift",
    free_text = match.arg(free_text),
    biometric_data = "error"
  )
}

.di_validate_policy <- function(policy) {
  required <- c(
    "policy_version", "label", "category", "field_aliases", "action", "source"
  )
  if (!inherits(policy, "deidentification_policy") ||
      !is.data.frame(policy) || !identical(names(policy), required) ||
      nrow(policy) != length(.di_categories) ||
      !identical(as.character(policy$category), .di_categories) ||
      !is.character(policy$policy_version) ||
      !all(policy$policy_version == "1") ||
      !is.character(policy$label) ||
      length(unique(policy$label)) != 1L ||
      !(unique(policy$label) %in%
        c("safe_harbor_candidate", "pseudonymized")) ||
      !is.list(policy$field_aliases) ||
      !all(vapply(
        policy$field_aliases,
        function(value) {
          is.character(value) && !is.factor(value) &&
            !anyNA(value) && all(nzchar(value))
        },
        logical(1)
      )) ||
      !is.character(policy$action) ||
      anyNA(policy$action) ||
      !is.character(policy$source) ||
      anyNA(policy$source) ||
      any(!nzchar(policy$source))) {
    .pc_abort("`policy` is not a supported de-identification policy.")
  }
  table <- attr(policy, "alias_table", exact = TRUE)
  free_text <- attr(policy, "free_text", exact = TRUE)
  biometric_data <- attr(policy, "biometric_data", exact = TRUE)
  if (!is.data.frame(table) ||
      !identical(
        names(table),
        c("category", "alias", "kind", "normalized")
      ) ||
      anyNA(table) ||
      !all(table$category %in% .di_categories) ||
      !all(table$kind %in% c(
        "identifier", "geography", "date", "age", "free_text",
        "biometric", "image"
      )) ||
      !identical(table$normalized, .di_normalize(table$alias)) ||
      any(!nzchar(table$normalized)) ||
      anyDuplicated(table$normalized) ||
      !all(vapply(seq_along(.di_categories), function(i) {
        identical(
          as.character(policy$field_aliases[[i]]),
          table$alias[table$category == .di_categories[[i]]]
        )
      }, logical(1))) ||
      !is.character(free_text) || length(free_text) != 1L ||
      is.na(free_text) ||
      !is.character(biometric_data) ||
      length(biometric_data) != 1L || is.na(biometric_data)) {
    .pc_abort("`policy` is not a supported de-identification policy.")
  }
  label <- unique(policy$label)
  if (identical(label, "safe_harbor_candidate")) {
    expected <- rep("remove", length(.di_categories))
    expected[.di_categories == "dates_ages"] <-
      policy$action[policy$category == "dates_ages"]
    expected[.di_categories %in% c("biometric", "full_face_image")] <-
      biometric_data
    if (!(free_text %in% c("error", "drop")) ||
        !(biometric_data %in% c("error", "drop")) ||
        !(policy$action[policy$category == "dates_ages"] %in%
          c("year", "remove")) ||
        !identical(policy$action, expected)) {
      .pc_abort("`policy` is not a supported de-identification policy.")
    }
  } else {
    expected <- rep("pseudonymize", length(.di_categories))
    expected[.di_categories == "dates_ages"] <- "date_shift"
    expected[.di_categories %in% c("biometric", "full_face_image")] <- "error"
    if (!(free_text %in% c("error", "drop", "retain")) ||
        !identical(biometric_data, "error") ||
        !identical(policy$action, expected)) {
      .pc_abort("`policy` is not a supported de-identification policy.")
    }
  }
  invisible(policy)
}

.di_policy_digest <- function(policy) {
  .di_validate_policy(policy)
  .pc_hash(list(
    policy_version = as.character(policy$policy_version),
    label = as.character(policy$label),
    category = as.character(policy$category),
    field_aliases = unclass(policy$field_aliases),
    action = as.character(policy$action),
    source = as.character(policy$source),
    alias_table = unclass(attr(policy, "alias_table", exact = TRUE)),
    free_text = attr(policy, "free_text", exact = TRUE),
    biometric_data = attr(policy, "biometric_data", exact = TRUE)
  ))
}

.di_match <- function(name, policy, context = NULL) {
  candidate <- name
  if (identical(context, "events") && identical(name, "value")) {
    candidate <- "event_value"
  }
  if (identical(context, "subject") && identical(name, "id")) {
    candidate <- "subject_id"
  }
  normalized <- .di_normalize(candidate)
  table <- attr(policy, "alias_table", exact = TRUE)
  index <- match(normalized, table$normalized)
  if (is.na(index)) {
    return(NULL)
  }
  list(
    category = table$category[[index]],
    alias = table$alias[[index]],
    kind = table$kind[[index]]
  )
}

.di_path_name <- function(name, parent) {
  converted <- iconv(name, from = "", to = "UTF-8", sub = NA_character_)
  if (is.na(converted) || !nzchar(converted) ||
      grepl("[[:cntrl:]]", converted)) {
    .pc_abort(sprintf("`%s` contains an unsafe field name.", parent))
  }
  paste0(parent, "$", converted)
}

.di_count_values <- function(value) {
  if (is.null(value)) {
    return(0L)
  }
  if (is.data.frame(value) || methods::is(value, "DataFrame")) {
    return(as.integer(nrow(value)))
  }
  as.integer(length(value))
}

.di_add_action <- function(state, path, category, action, value) {
  state$actions[[length(state$actions) + 1L]] <- data.frame(
    path = path,
    category = category,
    action = action,
    n_values = .di_count_values(value),
    stringsAsFactors = FALSE
  )
}

.di_add_manual <- function(state, path, reason) {
  key <- paste(path, reason, sep = "\r")
  if (!key %in% state$manual_keys) {
    state$manual_keys <- c(state$manual_keys, key)
    state$manual[[length(state$manual) + 1L]] <- data.frame(
      path = path,
      reason = reason,
      stringsAsFactors = FALSE
    )
  }
}

.di_forbidden <- function(value) {
  is.environment(value) || is.function(value) ||
    inherits(value, "connection") || inherits(value, "formula") ||
    typeof(value) %in%
      c("externalptr", "weakref", "language", "symbol", "pairlist")
}

.di_year <- function(value, path) {
  missing <- is.na(value)
  if (inherits(value, "Date") || inherits(value, "POSIXct")) {
    years <- format(value, "%Y", tz = "UTC")
    years[missing] <- NA_character_
    return(years)
  }
  if (!is.character(value) && !is.factor(value)) {
    .pc_abort(sprintf(
      "`%s` contains an unsupported date representation.",
      path
    ))
  }
  text <- as.character(value)
  year_only <- grepl("^[0-9]{4}$", text)
  year_month <- grepl("^[0-9]{4}-(0[1-9]|1[0-2])$", text)
  full <- grepl(
    paste0(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}",
      "([ T]([01][0-9]|2[0-3]):[0-5][0-9]",
      "(:[0-5][0-9](\\.[0-9]+)?)?",
      "(Z|[+-]([01][0-9]|2[0-3]):?[0-5][0-9])?)?$"
    ),
    text
  )
  calendar <- rep(FALSE, length(text))
  candidates <- which(!missing & full)
  if (length(candidates)) {
    date_part <- substr(text[candidates], 1L, 10L)
    parsed <- suppressWarnings(as.Date(date_part, format = "%Y-%m-%d"))
    calendar[candidates] <- !is.na(parsed) &
      format(parsed, "%Y-%m-%d") == date_part
  }
  valid <- missing | year_only | year_month | calendar
  if (!all(valid)) {
    .pc_abort(sprintf("`%s` contains an invalid person-related date.", path))
  }
  years <- substr(text, 1L, 4L)
  years[missing] <- NA_character_
  years
}

.di_age <- function(value, path) {
  if (is.factor(value)) {
    value <- as.character(value)
  }
  numeric_value <- suppressWarnings(as.numeric(value))
  invalid <- !is.na(value) & (
    is.na(numeric_value) | !is.finite(numeric_value) | numeric_value < 0
  )
  if (any(invalid)) {
    .pc_abort(sprintf("`%s` contains an invalid age.", path))
  }
  out <- as.character(value)
  out[!is.na(numeric_value) & numeric_value > 89] <- "90_or_older"
  out[is.na(value)] <- NA_character_
  out
}

.di_aligned_subject <- function(subject_id, n, path) {
  subject_id <- .ps_identifiers(subject_id)
  if (length(subject_id) == 1L && n != 1L) {
    subject_id <- rep(subject_id, n)
  }
  if (length(subject_id) != n) {
    .pc_abort(sprintf("`subject_id` is not aligned to `%s`.", path))
  }
  subject_id
}

.di_pseudonym_values <- function(bundle, value, path) {
  .ps_validate_object(bundle)
  n <- length(value)
  tokens <- bundle$values
  if (length(tokens) == 1L && n != 1L) {
    tokens <- rep(tokens, n)
  }
  if (length(tokens) != n) {
    .pc_abort(sprintf("`pseudonymization` is not aligned to `%s`.", path))
  }
  active <- !is.na(value)
  if (any(active & (
      is.na(tokens) | !grepl("^psn_[0-9a-f]{32}$", tokens)
  ))) {
    .pc_abort(sprintf(
      "`pseudonymization` has no valid token for `%s`.",
      path
    ))
  }
  out <- tokens
  out[!active] <- NA_character_
  names(out) <- names(value)
  out
}

.di_shift_date <- function(value, subject_id, date_key, path) {
  if (is.null(subject_id) || is.null(date_key)) {
    .pc_abort(sprintf(
      "Date shifting at `%s` requires `subject_id` and `date_key`.",
      path
    ))
  }
  original_character <- is.character(value) || is.factor(value)
  if (original_character) {
    text <- as.character(value)
    parsed <- suppressWarnings(as.Date(text))
    invalid <- !is.na(text) & is.na(parsed)
    if (any(invalid)) {
      .pc_abort(sprintf(
        "`%s` contains an invalid person-related date.",
        path
      ))
    }
    value <- parsed
  }
  if (!inherits(value, "Date") && !inherits(value, "POSIXct")) {
    .pc_abort(sprintf(
      "`%s` contains an unsupported date representation.",
      path
    ))
  }
  subjects <- .di_aligned_subject(subject_id, length(value), path)
  shifted <- dateShift(value, subjects, date_key)
  if (original_character) {
    shifted <- format(shifted, "%Y-%m-%d")
    shifted[is.na(value)] <- NA_character_
  }
  shifted
}

.di_field_action <- function(value, match, policy, state, path,
                             subject_id, pseudonymization, date_key) {
  label <- unique(policy$label)
  category <- match$category
  kind <- match$kind

  if (identical(kind, "free_text")) {
    action <- attr(policy, "free_text", exact = TRUE)
    if (identical(action, "error")) {
      .pc_abort(sprintf(
        "Detected free text at `%s`; choose an explicit policy.",
        path
      ))
    }
    if (identical(action, "retain")) {
      .di_add_action(state, path, category, "retain", value)
      .di_add_manual(
        state, path,
        "Retained free text requires manual identifier review."
      )
      return(list(value = value, remove = FALSE))
    }
    .di_add_action(state, path, category, "drop", value)
    return(list(value = NULL, remove = TRUE))
  }

  if (kind %in% c("biometric", "image")) {
    action <- if (identical(label, "safe_harbor_candidate")) {
      attr(policy, "biometric_data", exact = TRUE)
    } else {
      "error"
    }
    if (identical(action, "error")) {
      .pc_abort(sprintf(
        "Detected biometric or image content at `%s`.",
        path
      ))
    }
    .di_add_action(state, path, category, "drop", value)
    return(list(value = NULL, remove = TRUE))
  }

  if (identical(category, "dates_ages") && identical(kind, "age")) {
    if (identical(label, "safe_harbor_candidate")) {
      out <- .di_age(value, path)
      .di_add_action(state, path, category, "age_90_plus", value)
      return(list(value = out, remove = FALSE))
    }
    .di_add_manual(
      state, path,
      "Age values in pseudonymized data require contextual review."
    )
    return(list(value = value, remove = FALSE))
  }

  if (identical(category, "dates_ages")) {
    if (identical(label, "safe_harbor_candidate")) {
      action <- policy$action[policy$category == category]
      if (identical(action, "remove") ||
          .di_normalize(match$alias) %in% "starttime") {
        .di_add_action(state, path, category, "remove", value)
        return(list(value = NULL, remove = TRUE))
      }
      out <- .di_year(value, path)
      .di_add_action(state, path, category, "year", value)
      return(list(value = out, remove = FALSE))
    }
    out <- .di_shift_date(value, subject_id, date_key, path)
    .di_add_action(state, path, category, "date_shift", value)
    return(list(value = out, remove = FALSE))
  }

  if (identical(label, "pseudonymized")) {
    out <- .di_pseudonym_values(pseudonymization, value, path)
    .di_add_action(state, path, category, "pseudonymize", value)
    return(list(value = out, remove = FALSE))
  }

  .di_add_action(state, path, category, "remove", value)
  list(value = NULL, remove = TRUE)
}

.di_transform <- function(value, policy, state, path, context = NULL,
                          subject_id = NULL, pseudonymization = NULL,
                          date_key = NULL) {
  if (.di_forbidden(value) || (isS4(value) &&
      !methods::is(value, "DataFrame") &&
      !methods::is(value, "PhysioEvents"))) {
    .pc_abort(sprintf("`%s` contains an unsupported object graph.", path))
  }

  if (methods::is(value, "PhysioEvents")) {
    value@events <- .di_transform(
      value@events, policy, state, paste0(path, "@events"),
      context = "events", subject_id = subject_id,
      pseudonymization = pseudonymization, date_key = date_key
    )
    return(value)
  }

  if (methods::is(value, "DataFrame") || is.data.frame(value)) {
    nms <- names(value)
    if (is.null(nms) && ncol(value)) {
      .pc_abort(sprintf("`%s` must have named fields.", path))
    }
    for (name in nms) {
      field_path <- .di_path_name(name, path)
      if (identical(context, "design") && identical(name, "session_id")) {
        .di_add_manual(
          state, field_path,
          paste(
            "Structural session identifiers were preserved to maintain",
            "container validity."
          )
        )
        next
      }
      match <- .di_match(name, policy, context)
      if (!is.null(match)) {
        result <- .di_field_action(
          value[[name]], match, policy, state, field_path,
          subject_id, pseudonymization, date_key
        )
        if (result$remove) {
          value[[name]] <- NULL
        } else {
          value[[name]] <- result$value
        }
      } else if (is.list(value[[name]]) &&
                 !is.factor(value[[name]]) &&
                 !is.atomic(value[[name]])) {
        value[[name]] <- .di_transform(
          value[[name]], policy, state, field_path,
          subject_id = subject_id, pseudonymization = pseudonymization,
          date_key = date_key
        )
      } else if ((is.character(value[[name]]) ||
                  is.factor(value[[name]])) &&
                 !(.di_normalize(name) %in%
                   .di_normalize(.di_structural_fields))) {
        .di_add_manual(
          state, field_path,
          "Unclassified character metadata requires manual identifier review."
        )
      }
    }
    return(value)
  }

  if (is.list(value)) {
    if (!identical(class(value), "list")) {
      .pc_abort(sprintf("`%s` contains an unsupported list class.", path))
    }
    .pc_validate_names(value, path, required = FALSE)
    nms <- names(value)
    if (is.null(nms)) {
      for (i in seq_along(value)) {
        if (is.list(value[[i]]) ||
            methods::is(value[[i]], "DataFrame") ||
            methods::is(value[[i]], "PhysioEvents")) {
          value[[i]] <- .di_transform(
            value[[i]], policy, state, paste0(path, "[[", i, "]]"),
            subject_id = subject_id, pseudonymization = pseudonymization,
            date_key = date_key
          )
        } else if (.di_forbidden(value[[i]]) || isS4(value[[i]])) {
          .pc_abort(sprintf(
            "`%s[[%d]]` contains an unsupported object graph.",
            path, i
          ))
        }
      }
      return(value)
    }
    for (name in nms) {
      if (identical(name, "physio_compliance") ||
          identical(name, "deidentification")) {
        next
      }
      field_path <- .di_path_name(name, path)
      match <- .di_match(name, policy, context)
      if (!is.null(match)) {
        result <- .di_field_action(
          value[[name]], match, policy, state, field_path,
          subject_id, pseudonymization, date_key
        )
        if (result$remove) {
          value[[name]] <- NULL
        } else {
          value[[name]] <- result$value
        }
      } else if (is.list(value[[name]]) ||
                 methods::is(value[[name]], "DataFrame") ||
                 methods::is(value[[name]], "PhysioEvents")) {
        value[[name]] <- .di_transform(
          value[[name]], policy, state, field_path,
          subject_id = subject_id, pseudonymization = pseudonymization,
          date_key = date_key
        )
      } else {
        if (.di_forbidden(value[[name]]) || isS4(value[[name]])) {
          .pc_abort(sprintf(
            "`%s` contains an unsupported object graph.",
            field_path
          ))
        }
        if ((is.character(value[[name]]) ||
             is.factor(value[[name]])) &&
            !(.di_normalize(name) %in%
              .di_normalize(.di_structural_fields))) {
          .di_add_manual(
            state, field_path,
            "Unclassified character metadata requires manual identifier review."
          )
        }
      }
    }
    return(value)
  }

  if (.di_forbidden(value)) {
    .pc_abort(sprintf("`%s` contains an unsupported object graph.", path))
  }
  value
}

.di_empty_actions <- function() {
  data.frame(
    path = character(), category = character(), action = character(),
    n_values = integer(), stringsAsFactors = FALSE
  )
}

.di_empty_manual <- function() {
  data.frame(path = character(), reason = character(), stringsAsFactors = FALSE)
}

.di_rows <- function(rows, empty) {
  if (!length(rows)) {
    return(empty())
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.di_report <- function(policy, state, performed_at, pseudonymization,
                       date_strategy) {
  structure(list(
    schema_version = "1",
    policy_label = unique(policy$label),
    policy_digest = .di_policy_digest(policy),
    performed_at = .pc_timestamp(performed_at),
    field_actions = .di_rows(state$actions, .di_empty_actions),
    manual_review = .di_rows(state$manual, .di_empty_manual),
    pseudonym_algorithm = if (is.null(pseudonymization)) {
      NA_character_
    } else {
      pseudonymization$algorithm
    },
    key_fingerprint = if (is.null(pseudonymization)) {
      NA_character_
    } else {
      pseudonymization$key_fingerprint
    },
    date_strategy = date_strategy,
    audit_event_hash = NA_character_
  ), class = "deidentification_report")
}

.di_initialize_state <- function() {
  state <- new.env(parent = emptyenv())
  state$actions <- list()
  state$manual <- list()
  state$manual_keys <- character()
  state
}

.di_has_compliance <- function(x) {
  methods::is(x, "PhysioExperiment") && !is.null(.pc_state(x))
}

.di_transform_assays <- function(x, policy, state) {
  assay_list <- SummarizedExperiment::assays(x, withDimnames = FALSE)
  nms <- names(assay_list)
  if (!length(assay_list)) {
    return(x)
  }
  if (is.null(nms) || any(!nzchar(nms))) {
    .di_add_manual(
      state, "x@assays",
      "Unnamed physiological assays require manual identifier review."
    )
    return(x)
  }
  drop <- logical(length(assay_list))
  for (i in seq_along(assay_list)) {
    match <- .di_match(nms[[i]], policy)
    assay_path <- .di_path_name(nms[[i]], "x@assays")
    if (!is.null(match) && match$kind %in% c("biometric", "image")) {
      result <- .di_field_action(
        assay_list[[i]], match, policy, state, assay_path,
        subject_id = NULL, pseudonymization = NULL, date_key = NULL
      )
      drop[[i]] <- result$remove
    } else {
      .di_add_manual(
        state, assay_path,
        "Retained physiological signal content requires manual identifier review."
      )
    }
  }
  if (any(drop)) {
    SummarizedExperiment::assays(x, withDimnames = FALSE) <- assay_list[!drop]
  }
  x
}

.di_append_audit <- function(x, report, actor, reason, timestamp) {
  compliance_state <- .pc_state(x)
  if (is.null(compliance_state)) {
    return(x)
  }
  actor <- .pc_assert_string(actor, "audit_actor")
  reason <- .pc_assert_string(reason, "audit_reason")
  .pc_assert_appendable(x)
  counts <- table(report$field_actions$action)
  action_counts <- as.list(as.integer(counts))
  names(action_counts) <- names(counts)
  .pc_append_event_to_state(
    x = x,
    state = compliance_state,
    action = "physio_compliance.deidentify",
    actor = actor,
    reason = reason,
    details = list(
      action_counts = action_counts,
      policy_digest = report$policy_digest,
      result_record_digest = .pc_record_hash(x)
    ),
    timestamp = timestamp
  )
}

.di_deidentify_experiment <- function(
    x, policy, subject_id, pseudonymization, date_key, audit_actor,
    audit_reason, performed_at, prefix = "x") {
  metadata <- S4Vectors::metadata(x)
  if ("deidentification" %in% names(metadata)) {
    .pc_abort(sprintf(
      "`%s` already has a de-identification report.",
      prefix
    ))
  }
  if (.di_has_compliance(x)) {
    if (is.null(audit_actor)) {
      .pc_abort("`audit_actor` is required for an initialized audit trail.")
    }
    verification <- verifyAuditTrail(x)
    if (!verification$valid) {
      .pc_abort("The existing compliance chain is invalid.")
    }
  }

  state <- .di_initialize_state()
  x <- .di_transform_assays(x, policy, state)
  metadata <- S4Vectors::metadata(x)
  metadata <- .di_transform(
    metadata, policy, state, paste0(prefix, "@metadata"),
    subject_id = subject_id, pseudonymization = pseudonymization,
    date_key = date_key
  )
  S4Vectors::metadata(x) <- metadata
  SummarizedExperiment::rowData(x) <- .di_transform(
    SummarizedExperiment::rowData(x), policy, state,
    paste0(prefix, "@rowData"), subject_id = subject_id,
    pseudonymization = pseudonymization, date_key = date_key
  )
  SummarizedExperiment::colData(x) <- .di_transform(
    SummarizedExperiment::colData(x), policy, state,
    paste0(prefix, "@colData"), subject_id = subject_id,
    pseudonymization = pseudonymization, date_key = date_key
  )
  date_strategy <- if (
    unique(policy$label) == "safe_harbor_candidate"
  ) {
    policy$action[policy$category == "dates_ages"]
  } else {
    "keyed_day_shift"
  }
  report <- .di_report(
    policy, state, performed_at, pseudonymization, date_strategy
  )
  metadata <- S4Vectors::metadata(x)
  metadata[["deidentification"]] <- report
  S4Vectors::metadata(x) <- metadata
  x <- .di_append_audit(x, report, audit_actor, audit_reason, performed_at)
  methods::validObject(x)
  x
}

.di_merge_reports <- function(reports, policy, performed_at, pseudonymization,
                              date_strategy, manual = .di_empty_manual()) {
  actions <- lapply(reports, function(report) report$field_actions)
  reviews <- lapply(reports, function(report) report$manual_review)
  actions <- actions[vapply(actions, nrow, integer(1)) > 0L]
  reviews <- reviews[vapply(reviews, nrow, integer(1)) > 0L]
  state <- .di_initialize_state()
  state$actions <- actions
  state$manual <- c(reviews, if (nrow(manual)) list(manual) else list())
  .di_report(policy, state, performed_at, pseudonymization, date_strategy)
}

#' De-identify PhysioCore records
#'
#' Applies an explicit field policy while preserving the input S4 class.
#' Assays are never inspected for identifying signal content; retained assays
#' are therefore listed for manual review. Reports contain paths and counts,
#' never removed values, keys, or re-identification material.
#'
#' `MultiRatePhysioExperiment` and `PhysioLongitudinal` do not provide a
#' metadata slot. Their aggregate report is stored as a serializable
#' `deidentification` attribute, while every child `PhysioExperiment` keeps its
#' report in `metadata()`. Existing audit trails are linked per child.
#'
#' @param x A `PhysioExperiment`, `MultiRatePhysioExperiment`,
#'   `PhysioLongitudinal`, or `PhysioCohort` (a multi-subject container whose
#'   subject-level `colData` and every subject timeline are de-identified; unlike
#'   MultiRate/Longitudinal it has a `metadata` slot, so its merged report is
#'   stored there).
#' @param policy A policy from [safeHarborPolicy()] or
#'   [pseudonymizedPolicy()].
#' @param subject_id A scalar or field-aligned subject identifier used only for
#'   keyed date shifting.
#' @param pseudonymization A protected result from [pseudonymize()] used to
#'   replace direct identifiers.
#' @param date_key A caller-owned raw key of at least 32 bytes.
#' @param audit_actor Responsible actor when an input audit trail is initialized.
#' @param audit_reason Non-empty reason stored in linked audit events.
#'
#' @return A modified object of the same S4 class.
#' @export
deidentify <- function(
    x,
    policy = safeHarborPolicy(),
    subject_id = NULL,
    pseudonymization = NULL,
    date_key = NULL,
    audit_actor = NULL,
    audit_reason = "record de-identified") {
  .di_validate_policy(policy)
  label <- unique(policy$label)
  if (identical(label, "pseudonymized") && is.null(pseudonymization)) {
    .pc_abort("A `pseudonymization` object is required by this policy.")
  }
  if (!is.null(pseudonymization)) {
    .ps_validate_object(pseudonymization)
  }
  if (!is.null(date_key)) {
    date_key <- .ps_key(date_key)
  }
  if (!is.null(audit_actor)) {
    audit_actor <- .pc_assert_string(audit_actor, "audit_actor")
  }
  audit_reason <- .pc_assert_string(audit_reason, "audit_reason")
  performed_at <- Sys.time()

  if (methods::is(x, "PhysioExperiment")) {
    return(.di_deidentify_experiment(
      x, policy, subject_id, pseudonymization, date_key, audit_actor,
      audit_reason, performed_at
    ))
  }

  date_strategy <- if (label == "safe_harbor_candidate") {
    policy$action[policy$category == "dates_ages"]
  } else {
    "keyed_day_shift"
  }

  if (methods::is(x, "MultiRatePhysioExperiment")) {
    if (!is.null(attr(x, "deidentification", exact = TRUE))) {
      .pc_abort("`x` already has a de-identification report.")
    }
    reports <- list()
    for (i in seq_along(x@streams)) {
      prefix <- paste0("x@streams$", names(x@streams)[[i]])
      x@streams[[i]] <- .di_deidentify_experiment(
        x@streams[[i]], policy, subject_id, pseudonymization, date_key,
        audit_actor, audit_reason, performed_at, prefix
      )
      reports[[i]] <- S4Vectors::metadata(
        x@streams[[i]]
      )[["deidentification"]]
    }
    manual <- data.frame(
      path = "x",
      reason = paste(
        "The outer MultiRatePhysioExperiment has no metadata slot;",
        "audit linkage is maintained on child records."
      ),
      stringsAsFactors = FALSE
    )
    attr(x, "deidentification") <- .di_merge_reports(
      reports, policy, performed_at, pseudonymization, date_strategy, manual
    )
    methods::validObject(x)
    return(x)
  }

  if (methods::is(x, "PhysioLongitudinal")) {
    if (!is.null(attr(x, "deidentification", exact = TRUE))) {
      .pc_abort("`x` already has a de-identification report.")
    }
    reports <- list()
    for (i in seq_along(x@sessions)) {
      prefix <- paste0("x@sessions$", names(x@sessions)[[i]])
      x@sessions[[i]] <- deidentify(
        x@sessions[[i]], policy, subject_id, pseudonymization, date_key,
        audit_actor, audit_reason
      )
      child_report <- if (
        methods::is(x@sessions[[i]], "PhysioExperiment")
      ) {
        S4Vectors::metadata(x@sessions[[i]])[["deidentification"]]
      } else {
        attr(x@sessions[[i]], "deidentification", exact = TRUE)
      }
      child_report$field_actions$path <- sub(
        "^x", prefix, child_report$field_actions$path
      )
      child_report$manual_review$path <- sub(
        "^x", prefix, child_report$manual_review$path
      )
      reports[[i]] <- child_report
    }
    state <- .di_initialize_state()
    x@design <- .di_transform(
      x@design, policy, state, "x@design", context = "design",
      subject_id = subject_id, pseudonymization = pseudonymization,
      date_key = date_key
    )
    x@subject <- .di_transform(
      x@subject, policy, state, "x@subject", context = "subject",
      subject_id = subject_id, pseudonymization = pseudonymization,
      date_key = date_key
    )
    outer <- .di_report(
      policy, state, performed_at, pseudonymization, date_strategy
    )
    reports[[length(reports) + 1L]] <- outer
    manual <- data.frame(
      path = "x",
      reason = paste(
        "The outer PhysioLongitudinal has no metadata slot;",
        paste(
          "subject/design changes cannot be linked to a",
          "child audit event."
        )
      ),
      stringsAsFactors = FALSE
    )
    attr(x, "deidentification") <- .di_merge_reports(
      reports, policy, performed_at, pseudonymization, date_strategy, manual
    )
    methods::validObject(x)
    return(x)
  }
  if (methods::is(x, "PhysioCohort")) {
    if (!is.null(x@metadata[["deidentification"]])) {
      .pc_abort("`x` already has a de-identification report.")
    }
    reports <- list()
    snames <- names(x@subjects)
    for (i in seq_along(x@subjects)) {
      nm <- if (!is.null(snames) && nzchar(snames[[i]])) snames[[i]] else
        as.character(i)
      prefix <- paste0("x@subjects$", nm)
      x@subjects[[i]] <- deidentify(
        x@subjects[[i]], policy, subject_id, pseudonymization, date_key,
        audit_actor, audit_reason
      )
      child_report <- if (methods::is(x@subjects[[i]], "PhysioExperiment")) {
        S4Vectors::metadata(x@subjects[[i]])[["deidentification"]]
      } else {
        attr(x@subjects[[i]], "deidentification", exact = TRUE)
      }
      child_report$field_actions$path <- sub(
        "^x", prefix, child_report$field_actions$path
      )
      child_report$manual_review$path <- sub(
        "^x", prefix, child_report$manual_review$path
      )
      reports[[i]] <- child_report
    }
    # subject-level colData (one row per subject: id/group/dx/age/sex/... = the
    # cohort-level PII) is transformed on the outer object, which has a metadata
    # slot to carry the merged report (unlike MultiRate/Longitudinal).
    state <- .di_initialize_state()
    x@colData <- .di_transform(
      x@colData, policy, state, "x@colData", context = "colData",
      subject_id = subject_id, pseudonymization = pseudonymization,
      date_key = date_key
    )
    # subject_id is the cohort's structural linkage key: the Safe-Harbor transform
    # drops/alters it, so replace it with a stable positional pseudonym - the real
    # identifier is removed while the subject_id <-> names(subjects) invariant is
    # preserved - and re-align the subject index to match.
    pseudo_ids <- paste0("SUBJ-", seq_along(x@subjects))
    x@colData$subject_id <- pseudo_ids
    names(x@subjects) <- pseudo_ids
    outer <- .di_report(
      policy, state, performed_at, pseudonymization, date_strategy
    )
    reports[[length(reports) + 1L]] <- outer
    x@metadata[["deidentification"]] <- .di_merge_reports(
      reports, policy, performed_at, pseudonymization, date_strategy
    )
    methods::validObject(x)
    return(x)
  }
  .pc_abort(paste(
    "`x` must inherit from PhysioExperiment, MultiRatePhysioExperiment,",
    "PhysioLongitudinal, or PhysioCohort."
  ))
}

.di_report_valid <- function(report) {
  fields <- c(
    "schema_version", "policy_label", "policy_digest", "performed_at",
    "field_actions", "manual_review", "pseudonym_algorithm",
    "key_fingerprint", "date_strategy", "audit_event_hash"
  )
  valid <- inherits(report, "deidentification_report") &&
    is.list(report) && identical(names(report), fields) &&
    identical(report$schema_version, "1") &&
    is.character(report$policy_label) &&
    length(report$policy_label) == 1L &&
    !is.na(report$policy_label) &&
    report$policy_label %in% c("safe_harbor_candidate", "pseudonymized") &&
    .pc_is_hash(report$policy_digest) &&
    .pc_is_timestamp(report$performed_at) &&
    is.data.frame(report$field_actions) &&
    identical(
      names(report$field_actions),
      c("path", "category", "action", "n_values")
    ) &&
    is.data.frame(report$manual_review) &&
    identical(names(report$manual_review), c("path", "reason")) &&
    is.character(report$pseudonym_algorithm) &&
    length(report$pseudonym_algorithm) == 1L &&
    is.character(report$key_fingerprint) &&
    length(report$key_fingerprint) == 1L &&
    is.character(report$date_strategy) &&
    length(report$date_strategy) == 1L &&
    !is.na(report$date_strategy) &&
    is.character(report$audit_event_hash) &&
    length(report$audit_event_hash) == 1L &&
    (is.na(report$key_fingerprint) || .pc_is_hash(report$key_fingerprint)) &&
    (is.na(report$audit_event_hash) || .pc_is_hash(report$audit_event_hash))
  if (!isTRUE(valid)) {
    return(FALSE)
  }
  actions <- report$field_actions
  manual <- report$manual_review
  all(
    is.character(actions$path),
    is.character(actions$category),
    is.character(actions$action),
    is.integer(actions$n_values),
    !anyNA(actions),
    all(nzchar(actions$path)),
    all(actions$category %in% .di_categories),
    all(nzchar(actions$action)),
    all(actions$n_values >= 0L),
    is.character(manual$path),
    is.character(manual$reason),
    !anyNA(manual),
    all(nzchar(manual$path)),
    all(nzchar(manual$reason))
  )
}

.di_empty_findings <- function() {
  data.frame(
    severity = character(), rule_id = character(), path = character(),
    category = character(), message = character(), stringsAsFactors = FALSE
  )
}

.di_finding <- function(severity, rule_id, path, category, message) {
  data.frame(
    severity = severity, rule_id = rule_id, path = path,
    category = category, message = message, stringsAsFactors = FALSE
  )
}

.di_rule <- function(match, value, label) {
  category <- match$category
  if (match$kind == "free_text") {
    return(c("manual_review", "FREE_TEXT_REVIEW"))
  }
  if (match$kind == "biometric") {
    return(c("manual_review", "BIOMETRIC_REVIEW"))
  }
  if (match$kind == "image") {
    return(c("manual_review", "IMAGE_REVIEW"))
  }
  if (category == "dates_ages" && match$kind == "age") {
    numeric_value <- suppressWarnings(as.numeric(as.character(value)))
    if (any(!is.na(numeric_value) & numeric_value > 89)) {
      return(c("error", "AGE_OVER_89"))
    }
    return(NULL)
  }
  if (category == "dates_ages") {
    if (identical(label, "safe_harbor_candidate") &&
        all(is.na(value) | grepl("^[0-9]{4}$", as.character(value)))) {
      return(NULL)
    }
    if (identical(label, "pseudonymized")) {
      return(NULL)
    }
    return(c("error", "DATE_ELEMENT"))
  }
  if (category == "substate_geography") {
    return(c("error", "SUBSTATE_GEOGRAPHY"))
  }
  if (identical(label, "pseudonymized")) {
    if (all(is.na(value) |
        grepl("^psn_[0-9a-f]{32}$", as.character(value)))) {
      return(NULL)
    }
    return(c("error", "PSEUDONYM_LINK"))
  }
  c("error", "DIRECT_IDENTIFIER")
}

.di_scan <- function(value, policy, path, context = NULL) {
  findings <- list()
  add <- function(row) findings[[length(findings) + 1L]] <<- row
  walk <- function(current, current_path, current_context = NULL) {
    if (.di_forbidden(current) || (isS4(current) &&
        !methods::is(current, "DataFrame") &&
        !methods::is(current, "PhysioEvents"))) {
      .pc_abort(sprintf(
        "`%s` contains an unsupported object graph.",
        current_path
      ))
    }
    if (methods::is(current, "PhysioEvents")) {
      walk(current@events, paste0(current_path, "@events"), "events")
      return()
    }
    if (methods::is(current, "DataFrame") || is.data.frame(current)) {
      for (name in names(current)) {
        field_path <- .di_path_name(name, current_path)
        if (identical(current_context, "design") &&
            identical(name, "session_id")) {
          add(.di_finding(
            "manual_review", "UNKNOWN_FIELD_REVIEW", field_path,
            "other_unique",
            "Structural session identifiers require manual review."
          ))
          next
        }
        match <- .di_match(name, policy, current_context)
        if (!is.null(match)) {
          rule <- .di_rule(match, current[[name]], unique(policy$label))
          if (!is.null(rule)) {
            add(.di_finding(
              rule[[1L]], rule[[2L]], field_path, match$category,
              "A configured field remains after transformation."
            ))
          }
        } else if (is.list(current[[name]]) && !is.atomic(current[[name]])) {
          walk(current[[name]], field_path)
        }
      }
      return()
    }
    if (is.list(current)) {
      if (!identical(class(current), "list")) {
        .pc_abort(sprintf(
          "`%s` contains an unsupported list class.",
          current_path
        ))
      }
      for (name in names(current)) {
        if (name %in% c("physio_compliance", "deidentification")) {
          next
        }
        field_path <- .di_path_name(name, current_path)
        match <- .di_match(name, policy, current_context)
        if (!is.null(match)) {
          rule <- .di_rule(match, current[[name]], unique(policy$label))
          if (!is.null(rule)) {
            add(.di_finding(
              rule[[1L]], rule[[2L]], field_path, match$category,
              "A configured field remains after transformation."
            ))
          }
        } else if (is.list(current[[name]]) ||
                   methods::is(current[[name]], "DataFrame") ||
                   methods::is(current[[name]], "PhysioEvents")) {
          walk(current[[name]], field_path)
        } else if ((is.character(current[[name]]) ||
                    is.factor(current[[name]])) &&
                   !(.di_normalize(name) %in%
                     .di_normalize(.di_structural_fields))) {
          add(.di_finding(
            "manual_review", "UNKNOWN_FIELD_REVIEW", field_path,
            "unclassified",
            "Unclassified character metadata requires manual identifier review."
          ))
        }
      }
    }
  }
  walk(value, path, context)
  if (!length(findings)) .di_empty_findings() else do.call(rbind, findings)
}

.di_object_reports <- function(x) {
  if (methods::is(x, "PhysioExperiment")) {
    return(list(S4Vectors::metadata(x)[["deidentification"]]))
  }
  list(attr(x, "deidentification", exact = TRUE))
}

.di_scan_experiment <- function(x, policy, prefix = "x") {
  rows <- list(
    .di_scan(S4Vectors::metadata(x), policy, paste0(prefix, "@metadata")),
    .di_scan(
      SummarizedExperiment::rowData(x), policy, paste0(prefix, "@rowData")
    ),
    .di_scan(
      SummarizedExperiment::colData(x), policy, paste0(prefix, "@colData")
    )
  )
  rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
  if (!length(rows)) .di_empty_findings() else do.call(rbind, rows)
}

.di_report_manual_findings <- function(report) {
  if (is.null(report) || !.di_report_valid(report) ||
      !nrow(report$manual_review)) {
    return(.di_empty_findings())
  }
  rows <- lapply(seq_len(nrow(report$manual_review)), function(i) {
    reason <- report$manual_review$reason[[i]]
    rule <- if (grepl("image|pixel|photograph", reason, ignore.case = TRUE)) {
      "IMAGE_REVIEW"
    } else if (grepl(
      "signal|biometric|voice|finger", reason, ignore.case = TRUE
    )) {
      "BIOMETRIC_REVIEW"
    } else {
      "UNKNOWN_FIELD_REVIEW"
    }
    .di_finding(
      "manual_review", rule, report$manual_review$path[[i]],
      "unclassified", reason
    )
  })
  do.call(rbind, rows)
}

.di_audit_link_findings <- function(x, report, prefix = "x") {
  if (!.di_has_compliance(x) || is.null(report) ||
      !.di_report_valid(report)) {
    return(.di_empty_findings())
  }
  verification <- verifyAuditTrail(x)
  compliance_state <- .pc_state(x)
  counts <- table(report$field_actions$action)
  expected_counts <- as.list(as.integer(counts))
  names(expected_counts) <- names(counts)
  linked <- verification$valid && any(vapply(
    compliance_state$audit,
    function(event) {
      details <- event$details
      identical(event$action, "physio_compliance.deidentify") &&
        identical(details$policy_digest, report$policy_digest) &&
        identical(details$action_counts, expected_counts) &&
        identical(details$result_record_digest, .pc_record_hash(x)) &&
        identical(event$record_hash, .pc_record_hash(x))
    },
    logical(1)
  ))
  if (linked) {
    return(.di_empty_findings())
  }
  .di_finding(
    "error", "AUDIT_LINK",
    paste0(prefix, "@metadata$physio_compliance"),
    NA_character_, "No valid de-identification audit event is linked."
  )
}

#' Audit a stored de-identification transformation
#'
#' Rescans supported metadata and validates the stored report. A passing result
#' means that this checker found no configured residual identifier; it is not a
#' HIPAA, GDPR, Safe Harbor, or file-format compliance determination.
#'
#' @param x A supported de-identified PhysioCore record.
#' @param policy Optional effective policy. When omitted, a built-in policy
#'   matching the stored label is used.
#'
#' @return A list containing status, findings, severity counts, and check time.
#' @export
auditDeidentification <- function(x, policy = NULL) {
  policy_supplied <- !is.null(policy)
  reports <- .di_object_reports(x)
  report <- reports[[1L]]
  findings <- list()
  add <- function(row) findings[[length(findings) + 1L]] <<- row
  if (is.null(report)) {
    add(.di_finding(
      "error", "POLICY_MISSING", "x", NA_character_,
      "No de-identification report is stored."
    ))
    label <- NA_character_
  } else if (!.di_report_valid(report)) {
    add(.di_finding(
      "error", "REPORT_MALFORMED", "x", NA_character_,
      "The de-identification report is malformed."
    ))
    label <- if (is.character(report$policy_label)) {
      report$policy_label[[1L]]
    } else {
      NA_character_
    }
  } else {
    label <- report$policy_label
  }

  if (is.null(policy) && !is.na(label)) {
    policy <- if (identical(label, "pseudonymized")) {
      pseudonymizedPolicy()
    } else {
      safeHarborPolicy()
    }
  }
  if (!is.null(policy)) {
    .di_validate_policy(policy)
    if (policy_supplied && !is.null(report) && .di_report_valid(report) &&
        !identical(report$policy_digest, .di_policy_digest(policy))) {
      add(.di_finding(
        "error", "REPORT_MALFORMED", "x", NA_character_,
        "The supplied policy does not match the stored policy digest."
      ))
    }

    if (methods::is(x, "PhysioExperiment")) {
      manual <- .di_report_manual_findings(report)
      if (nrow(manual)) {
        add(manual)
      }
      scanned <- .di_scan_experiment(x, policy)
      if (nrow(scanned)) {
        add(scanned)
      }
      linked <- .di_audit_link_findings(x, report)
      if (nrow(linked)) {
        add(linked)
      }
    } else if (methods::is(x, "MultiRatePhysioExperiment")) {
      manual <- .di_report_manual_findings(report)
      if (nrow(manual)) {
        add(manual)
      }
      for (i in seq_along(x@streams)) {
        scanned <- .di_scan_experiment(
          x@streams[[i]], policy,
          paste0("x@streams$", names(x@streams)[[i]])
        )
        if (nrow(scanned)) {
          add(scanned)
        }
        child_report <- S4Vectors::metadata(
          x@streams[[i]]
        )[["deidentification"]]
        linked <- .di_audit_link_findings(
          x@streams[[i]], child_report,
          paste0("x@streams$", names(x@streams)[[i]])
        )
        if (nrow(linked)) {
          add(linked)
        }
      }
    } else if (methods::is(x, "PhysioLongitudinal")) {
      manual <- .di_report_manual_findings(report)
      if (nrow(manual)) {
        add(manual)
      }
      scanned <- .di_scan(x@design, policy, "x@design", "design")
      if (nrow(scanned)) {
        add(scanned)
      }
      scanned <- .di_scan(x@subject, policy, "x@subject", "subject")
      if (nrow(scanned)) {
        add(scanned)
      }
      for (i in seq_along(x@sessions)) {
        child <- x@sessions[[i]]
        prefix <- paste0("x@sessions$", names(x@sessions)[[i]])
        if (methods::is(child, "PhysioExperiment")) {
          scanned <- .di_scan_experiment(child, policy, prefix)
          if (nrow(scanned)) {
            add(scanned)
          }
          child_report <- S4Vectors::metadata(child)[["deidentification"]]
          linked <- .di_audit_link_findings(child, child_report, prefix)
          if (nrow(linked)) {
            add(linked)
          }
        } else {
          for (j in seq_along(child@streams)) {
            scanned <- .di_scan_experiment(
              child@streams[[j]], policy,
              paste0(prefix, "@streams$", names(child@streams)[[j]])
            )
            if (nrow(scanned)) {
              add(scanned)
            }
            stream <- child@streams[[j]]
            stream_prefix <- paste0(
              prefix, "@streams$", names(child@streams)[[j]]
            )
            child_report <- S4Vectors::metadata(
              stream
            )[["deidentification"]]
            linked <- .di_audit_link_findings(
              stream, child_report, stream_prefix
            )
            if (nrow(linked)) {
              add(linked)
            }
          }
        }
      }
    } else {
      .pc_abort("`x` is not a supported PhysioCore record.")
    }
  }

  all_findings <- if (!length(findings)) {
    .di_empty_findings()
  } else {
    out <- do.call(rbind, findings)
    rownames(out) <- NULL
    out
  }
  summary <- if (nrow(all_findings)) {
    counts <- table(all_findings$severity)
    data.frame(
      severity = names(counts), n = as.integer(counts),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(severity = character(), n = integer(), stringsAsFactors = FALSE)
  }
  status <- if (any(all_findings$severity == "error")) {
    "fail"
  } else if (nrow(all_findings)) {
    "manual_review"
  } else {
    "pass"
  }
  structure(list(
    status = status,
    policy_label = label,
    findings = all_findings,
    summary = summary,
    checked_at = .pc_timestamp(Sys.time())
  ), class = "deidentification_audit")
}

#' @export
print.deidentification_report <- function(x, ...) {
  cat(sprintf(
    "<deidentification_report> policy=%s; actions=%d; manual-review=%d\n",
    x$policy_label, nrow(x$field_actions), nrow(x$manual_review)
  ))
  invisible(x)
}
