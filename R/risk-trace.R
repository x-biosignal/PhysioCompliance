.pc_trace_schemas <- list(
  requirements = c(
    "requirement_id", "title", "description", "source", "source_version",
    "software_safety_class", "status"
  ),
  risks = c(
    "risk_id", "hazard", "foreseeable_sequence", "hazardous_situation",
    "harm", "initial_severity", "initial_probability", "initial_decision",
    "residual_severity", "residual_probability", "residual_decision",
    "benefit_risk_required", "acceptance_rationale", "status"
  ),
  controls = c(
    "control_id", "control_type", "description", "implementation_status",
    "implementation_evidence", "evidence_sha256"
  ),
  tests = c(
    "test_id", "level", "description", "expected_result", "status",
    "evidence_uri", "evidence_sha256", "executed_at", "executor"
  ),
  links = c(
    "source_type", "source_id", "target_type", "target_id", "link_type",
    "rationale"
  )
)

.pc_trace_id <- function(value, name) {
  if (!is.character(value) || anyNA(value) ||
      any(!grepl("^[A-Z][A-Z0-9_.-]{1,63}$", value)) ||
      any(value != trimws(value))) {
    .pc_abort(sprintf("`%s` contains an invalid stable identifier.", name))
  }
  invisible(value)
}

.pc_optional_strings <- function(value, columns, name) {
  for (column in columns) {
    x <- value[[column]]
    if (!is.character(x) ||
        any(Encoding(x[!is.na(x)]) == "bytes") ||
        anyNA(iconv(
          x[!is.na(x)], from = "", to = "UTF-8", sub = NA_character_
        ))) {
      .pc_abort(sprintf("`%s$%s` must contain UTF-8 strings or NA.", name, column))
    }
  }
  invisible(value)
}

.pc_trace_table <- function(value, name) {
  value <- .pc_plain_df(value, name, .pc_trace_schemas[[name]])
  id_column <- switch(
    name,
    requirements = "requirement_id",
    risks = "risk_id",
    controls = "control_id",
    tests = "test_id",
    links = NULL
  )
  if (!is.null(id_column)) {
    .pc_trace_id(value[[id_column]], paste0(name, "$", id_column))
    if (anyDuplicated(value[[id_column]])) {
      .pc_abort(sprintf("`%s` contains duplicated identifiers.", name))
    }
  }
  value
}

.pc_validate_requirements <- function(value) {
  value <- .pc_trace_table(value, "requirements")
  .pc_required_strings(value, names(value), "requirements")
  if (any(!value$software_safety_class %in%
          c("unclassified", "A", "B", "C"))) {
    .pc_abort("`requirements$software_safety_class` is invalid.")
  }
  if (any(!value$status %in%
          c("proposed", "accepted", "implemented", "retired"))) {
    .pc_abort("`requirements$status` is invalid.")
  }
  value <- value[
    order(value$requirement_id, method = "radix"), , drop = FALSE
  ]
  .pc_no_rownames(value)
}

.pc_validate_risks <- function(value, risk_matrix) {
  value <- .pc_trace_table(value, "risks")
  required <- setdiff(
    names(value), c("acceptance_rationale", "benefit_risk_required")
  )
  .pc_required_strings(value, required, "risks")
  .pc_optional_strings(value, "acceptance_rationale", "risks")
  if (!is.logical(value$benefit_risk_required) ||
      anyNA(value$benefit_risk_required)) {
    .pc_abort("`risks$benefit_risk_required` must contain logical values.")
  }
  decisions <- c(
    "acceptable", "unacceptable", "review_required", "not_evaluated"
  )
  if (any(!value$initial_decision %in% decisions) ||
      any(!value$residual_decision %in% decisions)) {
    .pc_abort("A risk decision contains an unsupported value.")
  }
  if (any(!value$status %in%
          c("identified", "controlled", "accepted", "closed"))) {
    .pc_abort("`risks$status` is invalid.")
  }
  if (is.null(risk_matrix)) {
    if (any(value$initial_decision != "not_evaluated") ||
        any(value$residual_decision != "not_evaluated")) {
      .pc_abort(
        "Risk decisions must be `not_evaluated` without a project risk matrix."
      )
    }
  } else {
    if (!.pc_risk_matrix_valid(risk_matrix)) {
      .pc_abort("`risk_matrix` is malformed or its hash does not match.")
    }
    known_severity <- risk_matrix$severity$level_id
    known_probability <- risk_matrix$probability$level_id
    if (any(!value$initial_severity %in% known_severity) ||
        any(!value$residual_severity %in% known_severity) ||
        any(!value$initial_probability %in% known_probability) ||
        any(!value$residual_probability %in% known_probability)) {
      .pc_abort("A risk references a level absent from `risk_matrix`.")
    }
    initial <- mapply(
      .pc_risk_decision,
      severity_id = value$initial_severity,
      probability_id = value$initial_probability,
      MoreArgs = list(matrix = risk_matrix),
      USE.NAMES = FALSE
    )
    residual <- mapply(
      .pc_risk_decision,
      severity_id = value$residual_severity,
      probability_id = value$residual_probability,
      MoreArgs = list(matrix = risk_matrix),
      USE.NAMES = FALSE
    )
    if (!identical(value$initial_decision, initial) ||
        !identical(value$residual_decision, residual)) {
      .pc_abort("A supplied risk decision conflicts with the exact matrix cell.")
    }
  }
  finalized <- value$status %in% c("accepted", "closed")
  rationale_missing <- is.na(value$acceptance_rationale) |
    !nzchar(trimws(value$acceptance_rationale))
  if (any(finalized & (
    is.null(risk_matrix) |
      value$residual_decision != "acceptable" |
      rationale_missing
  ))) {
    .pc_abort(
      "Accepted or closed risks require an acceptable matrix decision and rationale."
    )
  }
  value <- value[order(value$risk_id, method = "radix"), , drop = FALSE]
  .pc_no_rownames(value)
}

.pc_validate_controls <- function(value) {
  value <- .pc_trace_table(value, "controls")
  .pc_required_strings(
    value, c("control_id", "control_type", "description",
             "implementation_status"), "controls"
  )
  .pc_optional_strings(
    value, c("implementation_evidence", "evidence_sha256"), "controls"
  )
  if (any(!value$control_type %in%
          c("inherent_safety", "protective_measure", "information"))) {
    .pc_abort("`controls$control_type` is invalid.")
  }
  if (any(!value$implementation_status %in%
          c("proposed", "implemented", "verified", "retired"))) {
    .pc_abort("`controls$implementation_status` is invalid.")
  }
  bad_hash <- !is.na(value$evidence_sha256) &
    !grepl("^[0-9a-f]{64}$", value$evidence_sha256)
  if (any(bad_hash)) {
    .pc_abort("`controls$evidence_sha256` contains an invalid SHA-256 value.")
  }
  unpaired <- xor(
    is.na(value$implementation_evidence),
    is.na(value$evidence_sha256)
  )
  if (any(unpaired)) {
    .pc_abort("Control evidence URI and hash must be supplied together.")
  }
  value <- value[order(value$control_id, method = "radix"), , drop = FALSE]
  .pc_no_rownames(value)
}

.pc_validate_tests <- function(value) {
  value <- .pc_trace_table(value, "tests")
  .pc_required_strings(
    value, c("test_id", "level", "description", "expected_result", "status"),
    "tests"
  )
  .pc_optional_strings(
    value, c("evidence_uri", "evidence_sha256", "executed_at", "executor"),
    "tests"
  )
  if (any(!value$level %in%
          c("unit", "integration", "system", "acceptance", "inspection",
            "analysis"))) {
    .pc_abort("`tests$level` is invalid.")
  }
  if (any(!value$status %in% c("planned", "pass", "fail", "blocked"))) {
    .pc_abort("`tests$status` is invalid.")
  }
  bad_hash <- !is.na(value$evidence_sha256) &
    !grepl("^[0-9a-f]{64}$", value$evidence_sha256)
  bad_time <- !is.na(value$executed_at) &
    !vapply(value$executed_at, .pc_is_timestamp, logical(1))
  if (any(bad_hash) || any(bad_time)) {
    .pc_abort("Test evidence hash or execution timestamp is invalid.")
  }
  executed <- value$status %in% c("pass", "fail")
  required_missing <- is.na(value$evidence_uri) |
    is.na(value$evidence_sha256) |
    is.na(value$executed_at) |
    is.na(value$executor) |
    !nzchar(ifelse(is.na(value$executor), "", trimws(value$executor)))
  if (any(executed & required_missing)) {
    .pc_abort("Passing or failing tests require complete execution evidence.")
  }
  planned <- value$status == "planned"
  if (any(planned & (
    !is.na(value$evidence_uri) | !is.na(value$evidence_sha256) |
      !is.na(value$executed_at) | !is.na(value$executor)
  ))) {
    .pc_abort("Planned tests must not carry execution evidence.")
  }
  value <- value[order(value$test_id, method = "radix"), , drop = FALSE]
  .pc_no_rownames(value)
}

.pc_link_key <- function(value) {
  paste(
    value$source_type, value$source_id, value$link_type,
    value$target_type, value$target_id, sep = "\r"
  )
}

.pc_derivation_cycle <- function(links) {
  derives <- links[links$link_type == "derives", , drop = FALSE]
  if (!nrow(derives)) {
    return(FALSE)
  }
  source <- paste(derives$source_type, derives$source_id, sep = ":")
  target <- paste(derives$target_type, derives$target_id, sep = ":")
  nodes <- unique(c(source, target))
  state <- stats::setNames(integer(length(nodes)), nodes)
  adjacency <- split(target, source)
  visit <- function(node) {
    if (state[[node]] == 1L) {
      return(TRUE)
    }
    if (state[[node]] == 2L) {
      return(FALSE)
    }
    state[[node]] <<- 1L
    for (next_node in adjacency[[node]] %||% character()) {
      if (visit(next_node)) {
        return(TRUE)
      }
    }
    state[[node]] <<- 2L
    FALSE
  }
  any(vapply(nodes, visit, logical(1)))
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.pc_validate_links <- function(value, tables) {
  value <- .pc_trace_table(value, "links")
  .pc_required_strings(value, names(value), "links")
  types <- c("requirement", "risk", "control", "test")
  if (any(!value$source_type %in% types) ||
      any(!value$target_type %in% types)) {
    .pc_abort("A traceability link contains an unknown entity type.")
  }
  .pc_trace_id(value$source_id, "links$source_id")
  .pc_trace_id(value$target_id, "links$target_id")
  id_map <- list(
    requirement = tables$requirements$requirement_id,
    risk = tables$risks$risk_id,
    control = tables$controls$control_id,
    test = tables$tests$test_id
  )
  known_source <- mapply(
    function(type, id) id %in% id_map[[type]],
    value$source_type, value$source_id, USE.NAMES = FALSE
  )
  known_target <- mapply(
    function(type, id) id %in% id_map[[type]],
    value$target_type, value$target_id, USE.NAMES = FALSE
  )
  if (any(!known_source | !known_target)) {
    .pc_abort("A traceability link contains a dangling identifier.")
  }
  allowed <- data.frame(
    source_type = c(
      "requirement", "risk", "control", "requirement", "requirement", "risk"
    ),
    target_type = c(
      "risk", "control", "test", "test", "requirement", "risk"
    ),
    link_type = c(
      "traces_to", "mitigates", "verifies", "verifies", "derives",
      "derives"
    ),
    stringsAsFactors = FALSE
  )
  shape <- paste(
    value$source_type, value$target_type, value$link_type, sep = "\r"
  )
  allowed_shape <- paste(
    allowed$source_type, allowed$target_type, allowed$link_type, sep = "\r"
  )
  if (any(!shape %in% allowed_shape)) {
    .pc_abort("A traceability link has an unsupported directed shape.")
  }
  self <- value$source_type == value$target_type &
    value$source_id == value$target_id
  if (any(self) || anyDuplicated(.pc_link_key(value))) {
    .pc_abort("Self-links and duplicated traceability links are not allowed.")
  }
  if (.pc_derivation_cycle(value)) {
    .pc_abort("Traceability `derives` links contain a cycle.")
  }
  value <- value[
    order(
      value$source_type, value$source_id, value$link_type,
      value$target_type, value$target_id, method = "radix"
    ),
    ,
    drop = FALSE
  ]
  .pc_no_rownames(value)
}

.pc_derive_reachable <- function(type, id, links) {
  derives <- links[
    links$source_type == type & links$target_type == type &
      links$link_type == "derives",
    ,
    drop = FALSE
  ]
  found <- id
  repeat {
    next_ids <- derives$target_id[derives$source_id %in% found]
    expanded <- unique(c(found, next_ids))
    if (length(expanded) == length(found)) {
      break
    }
    found <- expanded
  }
  sort(found, method = "radix")
}

.pc_trace_paths <- function(requirements, risks, controls, tests, links) {
  rows <- list()
  add <- function(requirement_id = NA_character_, risk_id = NA_character_,
                  control_id = NA_character_, test_id = NA_character_,
                  path_status) {
    rows[[length(rows) + 1L]] <<- data.frame(
      requirement_id = requirement_id,
      risk_id = risk_id,
      control_id = control_id,
      test_id = test_id,
      path_status = path_status,
      stringsAsFactors = FALSE
    )
  }
  reached_risks <- reached_controls <- reached_tests <- character()
  for (requirement_id in requirements$requirement_id) {
    req_ids <- .pc_derive_reachable("requirement", requirement_id, links)
    direct_tests <- links$target_id[
      links$source_type == "requirement" &
        links$source_id %in% req_ids &
        links$target_type == "test" &
        links$link_type == "verifies"
    ]
    direct_tests <- sort(unique(direct_tests), method = "radix")
    for (test_id in direct_tests) {
      add(requirement_id, test_id = test_id, path_status = "direct_test")
    }
    reached_tests <- unique(c(reached_tests, direct_tests))
    risk_ids <- links$target_id[
      links$source_type == "requirement" &
        links$source_id %in% req_ids &
        links$target_type == "risk" &
        links$link_type == "traces_to"
    ]
    risk_ids <- sort(unique(risk_ids), method = "radix")
    for (risk_id in risk_ids) {
      risk_family <- .pc_derive_reachable("risk", risk_id, links)
      reached_risks <- unique(c(reached_risks, risk_family))
      control_ids <- links$target_id[
        links$source_type == "risk" &
          links$source_id %in% risk_family &
          links$target_type == "control" &
          links$link_type == "mitigates"
      ]
      control_ids <- sort(unique(control_ids), method = "radix")
      if (!length(control_ids)) {
        add(requirement_id, risk_id, path_status = "risk_uncontrolled")
      }
      for (control_id in control_ids) {
        reached_controls <- unique(c(reached_controls, control_id))
        test_ids <- links$target_id[
          links$source_type == "control" &
            links$source_id == control_id &
            links$target_type == "test" &
            links$link_type == "verifies"
        ]
        test_ids <- sort(unique(test_ids), method = "radix")
        if (!length(test_ids)) {
          add(
            requirement_id, risk_id, control_id,
            path_status = "control_untested"
          )
        }
        for (test_id in test_ids) {
          reached_tests <- unique(c(reached_tests, test_id))
          add(
            requirement_id, risk_id, control_id, test_id,
            path_status = "complete"
          )
        }
      }
    }
    if (!length(direct_tests) && !length(risk_ids)) {
      add(requirement_id, path_status = "requirement_only")
    }
  }
  for (risk_id in setdiff(risks$risk_id, reached_risks)) {
    control_ids <- links$target_id[
      links$source_type == "risk" & links$source_id == risk_id &
        links$target_type == "control" & links$link_type == "mitigates"
    ]
    control_ids <- sort(unique(control_ids), method = "radix")
    if (!length(control_ids)) {
      add(risk_id = risk_id, path_status = "risk_uncontrolled")
    }
    for (control_id in control_ids) {
      reached_controls <- unique(c(reached_controls, control_id))
      test_ids <- links$target_id[
        links$source_type == "control" & links$source_id == control_id &
          links$target_type == "test" & links$link_type == "verifies"
      ]
      test_ids <- sort(unique(test_ids), method = "radix")
      if (!length(test_ids)) {
        add(
          risk_id = risk_id, control_id = control_id,
          path_status = "control_untested"
        )
      }
      for (test_id in test_ids) {
        reached_tests <- unique(c(reached_tests, test_id))
        add(
          risk_id = risk_id, control_id = control_id, test_id = test_id,
          path_status = "complete"
        )
      }
    }
  }
  for (control_id in setdiff(controls$control_id, reached_controls)) {
    test_ids <- links$target_id[
      links$source_type == "control" & links$source_id == control_id &
        links$target_type == "test" & links$link_type == "verifies"
    ]
    test_ids <- sort(unique(test_ids), method = "radix")
    if (!length(test_ids)) {
      add(control_id = control_id, path_status = "control_untested")
    }
    for (test_id in test_ids) {
      reached_tests <- unique(c(reached_tests, test_id))
      add(
        control_id = control_id, test_id = test_id,
        path_status = "complete"
      )
    }
  }
  for (test_id in setdiff(tests$test_id, reached_tests)) {
    add(test_id = test_id, path_status = "requirement_only")
  }
  if (!length(rows)) {
    return(data.frame(
      requirement_id = character(),
      risk_id = character(),
      control_id = character(),
      test_id = character(),
      path_status = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  key <- do.call(paste, c(out, sep = "\r"))
  out <- out[!duplicated(key), , drop = FALSE]
  out <- out[
    order(
      is.na(out$requirement_id), out$requirement_id,
      is.na(out$risk_id), out$risk_id,
      is.na(out$control_id), out$control_id,
      is.na(out$test_id), out$test_id, out$path_status,
      method = "radix", na.last = TRUE
    ),
    ,
    drop = FALSE
  ]
  .pc_no_rownames(out)
}

.pc_evidence_root <- function(evidence_root) {
  if (is.null(evidence_root)) {
    return(NULL)
  }
  if (!is.character(evidence_root) || length(evidence_root) != 1L ||
      is.na(evidence_root) || !nzchar(evidence_root) ||
      !dir.exists(evidence_root)) {
    .pc_abort("`evidence_root` must be NULL or one existing directory.")
  }
  if (nzchar(Sys.readlink(evidence_root))) {
    .pc_abort("`evidence_root` must not be a symbolic link.")
  }
  normalized <- normalizePath(evidence_root, winslash = "/", mustWork = TRUE)
  home <- normalizePath("~", winslash = "/", mustWork = TRUE)
  display <- if (identical(normalized, home)) {
    "~"
  } else if (startsWith(normalized, paste0(home, "/"))) {
    paste0("~", substring(normalized, nchar(home) + 1L))
  } else {
    normalized
  }
  list(
    root_id = basename(normalized),
    display_path = display,
    path_hash = .pc_hash_raw(charToRaw(enc2utf8(normalized)))
  )
}

.pc_trace_content <- function(x) {
  x[c(
    "schema_version", "requirements", "risks", "controls", "tests", "links",
    "paths", "risk_matrix", "evidence_root"
  )]
}

#' Construct a normalized requirement-risk-control-test traceability graph
#'
#' @param requirements,risks,controls,tests,links Plain data frames using the
#'   schemas documented in `vignette("PhysioCompliance")` and the package
#'   reference.
#' @param evidence_root Optional evidence directory. Evidence URIs remain
#'   relative to this root.
#' @param risk_matrix Optional project-owned [riskMatrix()].
#' @return A deterministic `traceability_matrix` object.
#' @export
traceabilityMatrix <- function(
    requirements, risks, controls, tests, links,
    evidence_root = NULL, risk_matrix = NULL) {
  if (!is.null(risk_matrix) && !.pc_risk_matrix_valid(risk_matrix)) {
    .pc_abort("`risk_matrix` must be NULL or a valid risk_matrix object.")
  }
  requirements <- .pc_validate_requirements(requirements)
  risks <- .pc_validate_risks(risks, risk_matrix)
  controls <- .pc_validate_controls(controls)
  tests <- .pc_validate_tests(tests)
  tables <- list(
    requirements = requirements,
    risks = risks,
    controls = controls,
    tests = tests
  )
  links <- .pc_validate_links(links, tables)
  paths <- .pc_trace_paths(requirements, risks, controls, tests, links)
  fields <- list(
    schema_version = "1",
    requirements = requirements,
    risks = risks,
    controls = controls,
    tests = tests,
    links = links,
    paths = paths,
    risk_matrix = risk_matrix,
    evidence_root = .pc_evidence_root(evidence_root)
  )
  fields$content_hash <- .pc_hash(fields)
  structure(fields, class = "traceability_matrix")
}

.pc_trace_issue_table <- function() {
  data.frame(
    component = character(),
    entity_id = character(),
    rule_id = character(),
    severity = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}

.pc_evidence_path <- function(root, uri, expected_hash, max_size = 104857600) {
  if (!is.character(uri) || length(uri) != 1L || is.na(uri) || !nzchar(uri) ||
      startsWith(uri, "/") || startsWith(uri, "~") ||
      grepl("^[A-Za-z]:", uri) || grepl("^[A-Za-z][A-Za-z0-9+.-]*:", uri) ||
      grepl("\\\\", uri) ||
      grepl("(^|/)[.][.]($|/)", uri)) {
    return(list(rule_id = "EVIDENCE_PATH_ESCAPE", message = "Evidence URI is not a safe relative path."))
  }
  root_path <- path.expand(root$display_path)
  if (!dir.exists(root_path)) {
    return(list(rule_id = "EVIDENCE_MISSING", message = "Evidence root is missing."))
  }
  if (nzchar(Sys.readlink(root_path))) {
    return(list(rule_id = "EVIDENCE_PATH_ESCAPE", message = "Evidence root is a symbolic link."))
  }
  root_path <- normalizePath(root_path, winslash = "/", mustWork = TRUE)
  root_hash <- .pc_hash_raw(charToRaw(enc2utf8(root_path)))
  if (!identical(root$root_id, basename(root_path)) ||
      !identical(root$path_hash, root_hash)) {
    return(list(rule_id = "EVIDENCE_PATH_ESCAPE", message = "Evidence root metadata do not match the resolved root."))
  }
  target <- file.path(root_path, uri)
  if (!file.exists(target)) {
    return(list(rule_id = "EVIDENCE_MISSING", message = "Evidence file is missing."))
  }
  normalized <- normalizePath(target, winslash = "/", mustWork = TRUE)
  if (!startsWith(normalized, paste0(root_path, "/")) ||
      nzchar(Sys.readlink(target))) {
    return(list(rule_id = "EVIDENCE_PATH_ESCAPE", message = "Evidence resolves outside the evidence root or through a symlink."))
  }
  relative_parts <- strsplit(uri, "/", fixed = TRUE)[[1]]
  current <- root_path
  for (part in utils::head(relative_parts, -1L)) {
    current <- file.path(current, part)
    if (nzchar(Sys.readlink(current))) {
      return(list(rule_id = "EVIDENCE_PATH_ESCAPE", message = "Evidence path contains a symbolic link."))
    }
  }
  info_before <- file.info(normalized)
  if (!isTRUE(utils::file_test("-f", normalized)) ||
      isTRUE(info_before$isdir) ||
      is.na(info_before$size) || info_before$size > max_size) {
    return(list(rule_id = "EVIDENCE_MISSING", message = "Evidence is not a permitted regular file."))
  }
  connection <- file(normalized, open = "rb")
  bytes <- tryCatch(
    readBin(connection, "raw", n = info_before$size),
    finally = close(connection)
  )
  info_after <- file.info(normalized)
  if (!identical(info_before$size, info_after$size) ||
      !identical(as.numeric(info_before$mtime), as.numeric(info_after$mtime))) {
    return(list(rule_id = "EVIDENCE_HASH", message = "Evidence changed while it was read."))
  }
  if (!identical(.pc_hash_raw(bytes), expected_hash)) {
    return(list(rule_id = "EVIDENCE_HASH", message = "Evidence SHA-256 does not match."))
  }
  NULL
}

#' Verify traceability structure, completeness, and objective evidence
#'
#' @param x A `traceability_matrix`.
#' @param verify_evidence Whether to hash evidence files under the recorded
#'   evidence root.
#' @return A `traceability_verification` object.
#' @export
validateTraceability <- function(x, verify_evidence = TRUE) {
  verify_evidence <- .pc_scalar_flag(verify_evidence, "verify_evidence")
  issues <- list()
  add <- function(component, entity_id, rule_id, severity, message) {
    issues[[length(issues) + 1L]] <<- data.frame(
      component = component,
      entity_id = if (is.na(entity_id)) NA_character_ else as.character(entity_id),
      rule_id = rule_id,
      severity = severity,
      message = message,
      stringsAsFactors = FALSE
    )
  }
  expected_names <- c(
    "schema_version", "requirements", "risks", "controls", "tests", "links",
    "paths", "risk_matrix", "evidence_root", "content_hash"
  )
  schema_ok <- inherits(x, "traceability_matrix") && is.list(x) &&
    identical(names(x), expected_names) && identical(x$schema_version, "1")
  if (!schema_ok) {
    add("traceability", NA, "TRACE_SCHEMA", "error",
        "The traceability object schema is invalid.")
    issue_table <- do.call(rbind, issues)
    return(structure(
      list(
        valid = FALSE, complete = FALSE, issues = issue_table,
        counts = c(requirements = 0L, risks = 0L, controls = 0L, tests = 0L),
        content_hash = NA_character_, evidence_checked = FALSE
      ),
      class = "traceability_verification"
    ))
  }
  expected_hash <- tryCatch(
    .pc_hash(.pc_trace_content(x)),
    error = function(e) NA_character_
  )
  if (!.pc_is_hash(x$content_hash) ||
      !identical(x$content_hash, expected_hash)) {
    add("traceability", NA, "CONTENT_HASH", "error",
        "The stored traceability content hash does not match.")
  }
  tables <- c("requirements", "risks", "controls", "tests", "links")
  table_ok <- TRUE
  matrix_valid <- is.null(x$risk_matrix) ||
    .pc_risk_matrix_valid(x$risk_matrix)
  if (!matrix_valid) {
    add("risks", NA, "RISK_DECISION", "error",
        "The project risk matrix is malformed or its hash does not match.")
    table_ok <- FALSE
  }
  canonical <- list()
  validate_table <- function(name, expression) {
    result <- tryCatch(expression, error = identity)
    if (inherits(result, "error")) {
      message <- conditionMessage(result)
      rule_id <- if (grepl("duplicated", message, fixed = TRUE)) {
        "DUPLICATE_ID"
      } else if (grepl("absent from `risk_matrix`", message, fixed = TRUE)) {
        "RISK_MATRIX_LEVEL"
      } else if (grepl("risk decision|Risk decisions|matrix decision",
                       message, ignore.case = TRUE)) {
        "RISK_DECISION"
      } else if (grepl("acceptance|Accepted or closed", message,
                       ignore.case = TRUE)) {
        "ACCEPTANCE_RATIONALE"
      } else if (grepl("dangling", message, fixed = TRUE)) {
        "DANGLING_LINK"
      } else if (grepl("cycle", message, fixed = TRUE)) {
        "DERIVATION_CYCLE"
      } else if (grepl("link|directed shape", message, ignore.case = TRUE)) {
        "LINK_SHAPE"
      } else {
        "TRACE_SCHEMA"
      }
      add(name, NA, rule_id, "error",
          "A canonical traceability table contains invalid stored content.")
      table_ok <<- FALSE
      return(NULL)
    }
    if (!identical(result, x[[name]])) {
      add(name, NA, "TRACE_SCHEMA", "error",
          "A stored traceability table is not in canonical form.")
      table_ok <<- FALSE
      return(NULL)
    }
    canonical[[name]] <<- result
    result
  }
  validate_table(
    "requirements", .pc_validate_requirements(x$requirements)
  )
  if (matrix_valid) {
    validate_table(
      "risks", .pc_validate_risks(x$risks, x$risk_matrix)
    )
  }
  validate_table("controls", .pc_validate_controls(x$controls))
  validate_table("tests", .pc_validate_tests(x$tests))
  if (all(c("requirements", "risks", "controls", "tests") %in%
          names(canonical))) {
    validate_table(
      "links",
      .pc_validate_links(x$links, canonical[c(
        "requirements", "risks", "controls", "tests"
      )])
    )
  } else {
    table_ok <- FALSE
  }
  path_columns <- c(
    "requirement_id", "risk_id", "control_id", "test_id", "path_status"
  )
  canonical_paths <- tryCatch(
    .pc_plain_df(x$paths, "paths", path_columns),
    error = function(e) NULL
  )
  paths_ok <- !is.null(canonical_paths) &&
    identical(canonical_paths, x$paths) &&
    all(vapply(canonical_paths, is.character, logical(1))) &&
    !anyNA(canonical_paths$path_status) &&
    all(canonical_paths$path_status %in% c(
      "complete", "requirement_only", "direct_test", "risk_uncontrolled",
      "control_untested"
    ))
  if (paths_ok) {
    optional_ids <- unlist(
      canonical_paths[c(
        "requirement_id", "risk_id", "control_id", "test_id"
      )],
      use.names = FALSE
    )
    paths_ok <- tryCatch({
      .pc_trace_id(optional_ids[!is.na(optional_ids)], "paths")
      TRUE
    }, error = function(e) FALSE)
  }
  if (!paths_ok) {
    add("paths", NA, "TRACE_SCHEMA", "error",
        "The canonical trace-path table has an invalid schema.")
  }
  evidence_root_ok <- is.null(x$evidence_root) || (
    is.list(x$evidence_root) &&
      identical(
        names(x$evidence_root), c("root_id", "display_path", "path_hash")
      ) &&
      all(vapply(
        x$evidence_root[c("root_id", "display_path")],
        function(value) {
          is.character(value) && length(value) == 1L &&
            !is.na(value) && nzchar(value)
        },
        logical(1)
      )) &&
      .pc_is_hash(x$evidence_root$path_hash)
  )
  if (!evidence_root_ok) {
    add("evidence_root", NA, "TRACE_SCHEMA", "error",
        "The evidence-root metadata schema is invalid.")
    table_ok <- FALSE
  }
  if (table_ok) {
    id_columns <- c(
      requirements = "requirement_id", risks = "risk_id",
      controls = "control_id", tests = "test_id"
    )
    for (name in names(id_columns)) {
      ids <- x[[name]][[id_columns[[name]]]]
      if (anyDuplicated(ids)) {
        add(name, ids[duplicated(ids)][[1]], "DUPLICATE_ID", "error",
            "An entity identifier is duplicated.")
      }
    }
    id_map <- list(
      requirement = x$requirements$requirement_id,
      risk = x$risks$risk_id,
      control = x$controls$control_id,
      test = x$tests$test_id
    )
    allowed_shapes <- paste(
      c(
        "requirement", "risk", "control", "requirement", "requirement",
        "risk"
      ),
      c(
        "risk", "control", "test", "test", "requirement", "risk"
      ),
      c(
        "traces_to", "mitigates", "verifies", "verifies", "derives",
        "derives"
      ),
      sep = "\r"
    )
    for (i in seq_len(nrow(x$links))) {
      link <- x$links[i, , drop = FALSE]
      source_known <- link$source_type %in% names(id_map) &&
        link$source_id %in% id_map[[link$source_type]]
      target_known <- link$target_type %in% names(id_map) &&
        link$target_id %in% id_map[[link$target_type]]
      entity <- paste0(link$source_type, ":", link$source_id)
      if (!source_known || !target_known) {
        add("links", entity, "DANGLING_LINK", "error",
            "A link references an unknown entity.")
      }
      shape <- paste(
        link$source_type, link$target_type, link$link_type, sep = "\r"
      )
      self <- identical(link$source_type, link$target_type) &&
        identical(link$source_id, link$target_id)
      if (!shape %in% allowed_shapes || self) {
        add("links", entity, "LINK_SHAPE", "error",
            "A link has an unsupported direction, type, or self-reference.")
      }
    }
    if (anyDuplicated(.pc_link_key(x$links))) {
      add("links", NA, "LINK_SHAPE", "error",
          "A traceability link is duplicated.")
    }
    if (.pc_derivation_cycle(x$links)) {
      add("links", NA, "DERIVATION_CYCLE", "error",
          "Derivation links contain a cycle.")
    }
    if (!is.null(x$risk_matrix)) {
      severity <- x$risk_matrix$severity$level_id
      probability <- x$risk_matrix$probability$level_id
      for (i in seq_len(nrow(x$risks))) {
        risk <- x$risks[i, , drop = FALSE]
        levels_known <- all(c(
          risk$initial_severity, risk$residual_severity
        ) %in% severity) && all(c(
          risk$initial_probability, risk$residual_probability
        ) %in% probability)
        if (!levels_known) {
          add("risks", risk$risk_id, "RISK_MATRIX_LEVEL", "error",
              "A risk references a level absent from the project matrix.")
        } else {
          expected_initial <- .pc_risk_decision(
            x$risk_matrix, risk$initial_severity, risk$initial_probability
          )
          expected_residual <- .pc_risk_decision(
            x$risk_matrix, risk$residual_severity, risk$residual_probability
          )
          if (!identical(risk$initial_decision, expected_initial) ||
              !identical(risk$residual_decision, expected_residual)) {
            add("risks", risk$risk_id, "RISK_DECISION", "error",
                "A stored risk decision conflicts with its exact matrix cell.")
          }
        }
      }
    }
    expected_paths <- tryCatch(
      .pc_trace_paths(
        x$requirements, x$risks, x$controls, x$tests, x$links
      ),
      error = function(e) NULL
    )
    if (is.null(expected_paths) || !paths_ok ||
        !identical(x$paths, expected_paths)) {
      add("paths", NA, "TRACE_SCHEMA", "error",
          "Stored trace paths do not match the canonical link traversal.")
    }
    paths_for_checks <- if (is.null(expected_paths)) x$paths else expected_paths

    for (id in x$requirements$requirement_id[
      x$requirements$status != "retired"
    ]) {
      covered <- any(
        paths_for_checks$requirement_id == id &
          !is.na(paths_for_checks$test_id),
        na.rm = TRUE
      )
      if (!covered) {
        add("requirements", id, "REQUIREMENT_UNTESTED", "error",
            "A non-retired requirement has no test path.")
      }
    }
    for (i in seq_len(nrow(x$risks))) {
      id <- x$risks$risk_id[[i]]
      inbound <- any(
        x$links$source_type == "requirement" &
          x$links$target_type == "risk" &
          x$links$target_id == id &
          x$links$link_type == "traces_to"
      )
      if (!inbound) {
        add("risks", id, "RISK_ORPHAN", "error",
            "A risk is not linked from a requirement.")
      }
      if (x$risks$status[[i]] != "retired") {
        controlled <- any(
          x$links$source_type == "risk" &
            x$links$source_id == id &
            x$links$target_type == "control" &
            x$links$link_type == "mitigates"
        )
        if (!controlled) {
          add("risks", id, "RISK_UNCONTROLLED", "error",
              "A non-retired risk has no linked control.")
        }
      }
      finalized <- x$risks$status[[i]] %in% c("accepted", "closed")
      rationale <- x$risks$acceptance_rationale[[i]]
      if (finalized && (
        is.null(x$risk_matrix) ||
          x$risks$residual_decision[[i]] != "acceptable"
      )) {
        add("risks", id, "RISK_DECISION", "error",
            "A finalized risk lacks an acceptable project matrix decision.")
      }
      if (finalized && (
        is.na(rationale) || !nzchar(trimws(rationale))
      )) {
        add("risks", id, "ACCEPTANCE_RATIONALE", "error",
            "A finalized risk lacks a recorded acceptance rationale.")
      }
    }
    for (i in seq_len(nrow(x$controls))) {
      id <- x$controls$control_id[[i]]
      if (x$controls$implementation_status[[i]] %in%
          c("implemented", "verified")) {
        tested <- any(
          x$links$source_type == "control" &
            x$links$source_id == id &
            x$links$target_type == "test" &
            x$links$link_type == "verifies"
        )
        if (!tested) {
          add("controls", id, "CONTROL_UNVERIFIED", "error",
              "An implemented control has no linked test.")
        }
      }
    }
    for (i in seq_len(nrow(x$tests))) {
      id <- x$tests$test_id[[i]]
      if (x$tests$status[[i]] == "fail") {
        add("tests", id, "FAILED_TEST", "error",
            "A recorded test result failed.")
      }
      if (x$tests$status[[i]] == "pass" &&
          (is.na(x$tests$evidence_uri[[i]]) ||
           is.na(x$tests$evidence_sha256[[i]]))) {
        add("tests", id, "TEST_EVIDENCE_MISSING", "error",
            "A passing test lacks objective-evidence metadata.")
      }
      if (verify_evidence && x$tests$status[[i]] == "pass") {
        if (is.null(x$evidence_root)) {
          add("tests", id, "TEST_EVIDENCE_MISSING", "warning",
              "File evidence was not evaluated because no evidence root is recorded.")
        } else {
          finding <- .pc_evidence_path(
            x$evidence_root,
            x$tests$evidence_uri[[i]],
            x$tests$evidence_sha256[[i]]
          )
          if (!is.null(finding)) {
            add("tests", id, finding$rule_id, "error", finding$message)
          }
        }
      }
    }
  }
  issue_table <- if (length(issues)) {
    do.call(rbind, issues)
  } else {
    .pc_trace_issue_table()
  }
  if (nrow(issue_table)) {
    issue_table <- issue_table[
      order(
        issue_table$component, is.na(issue_table$entity_id),
        issue_table$entity_id, issue_table$rule_id, issue_table$message,
        method = "radix", na.last = TRUE
      ),
      ,
      drop = FALSE
    ]
    rownames(issue_table) <- NULL
  }
  invalid_rules <- c(
    "TRACE_SCHEMA", "CONTENT_HASH", "DUPLICATE_ID", "DANGLING_LINK",
    "LINK_SHAPE", "DERIVATION_CYCLE", "EVIDENCE_PATH_ESCAPE",
    "EVIDENCE_HASH", "RISK_MATRIX_LEVEL", "RISK_DECISION"
  )
  valid <- !any(issue_table$rule_id %in% invalid_rules)
  counts <- c(
    requirements = if (table_ok) nrow(x$requirements) else 0L,
    risks = if (table_ok) nrow(x$risks) else 0L,
    controls = if (table_ok) nrow(x$controls) else 0L,
    tests = if (table_ok) nrow(x$tests) else 0L
  )
  structure(
    list(
      valid = valid,
      complete = valid && nrow(issue_table) == 0L,
      issues = issue_table,
      counts = counts,
      content_hash = x$content_hash,
      evidence_checked = verify_evidence && !is.null(x$evidence_root) &&
        evidence_root_ok
    ),
    class = "traceability_verification"
  )
}

#' @export
as.data.frame.traceability_matrix <- function(x, ...) {
  x$paths
}

#' @export
print.traceability_matrix <- function(x, ...) {
  cat("<traceability_matrix>\n")
  cat("  requirements:", nrow(x$requirements), "\n")
  cat("  risks:", nrow(x$risks), "\n")
  cat("  controls:", nrow(x$controls), "\n")
  cat("  tests:", nrow(x$tests), "\n")
  cat("  paths:", nrow(x$paths), "\n")
  cat("  risk matrix:", if (is.null(x$risk_matrix)) "not evaluated" else x$risk_matrix$matrix_id, "\n")
  cat("  content hash:", x$content_hash, "\n")
  invisible(x)
}

#' @export
as.data.frame.traceability_verification <- function(x, ...) {
  x$issues
}

#' @export
print.traceability_verification <- function(x, ...) {
  cat("<traceability_verification>\n")
  cat("  structurally valid:", x$valid, "\n")
  cat("  traceability complete:", x$complete, "\n")
  cat("  objective evidence checked:", x$evidence_checked, "\n")
  cat("  issues:", nrow(x$issues), "\n")
  invisible(x)
}
