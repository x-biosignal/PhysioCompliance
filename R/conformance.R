.pc_criteria_ids <- c(
  "DESCRIPTION_VALID", "NAMESPACE_DOCUMENTED", "EXAMPLE_COVERAGE",
  "REFERENCE_PRESENT", "TESTS_PRESENT", "TEST_COVERAGE",
  "PROVENANCE_READINESS", "AUDIT_READINESS",
  "DEIDENTIFICATION_READINESS", "EXTERNAL_CHECKS"
)

#' Define PhysioExperiment project conformance criteria
#'
#' Thresholds are project policies for an engineering-readiness check. They
#' are not regulatory thresholds or clauses of a standard.
#'
#' @param example_floor Minimum fraction of documented exports with examples.
#' @param coverage_floor Minimum caller-supplied line coverage.
#' @return A `conformance_criteria` data frame.
#' @export
conformanceCriteria <- function(example_floor = 0.50, coverage_floor = 0.80) {
  scalar_fraction <- function(value, name) {
    if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
        !is.finite(value) || value < 0 || value > 1) {
      .pc_abort(sprintf("`%s` must be one finite number in [0, 1].", name))
    }
    as.double(value)
  }
  example_floor <- scalar_fraction(example_floor, "example_floor")
  coverage_floor <- scalar_fraction(coverage_floor, "coverage_floor")
  out <- data.frame(
    criterion_id = .pc_criteria_ids,
    description = c(
      "DESCRIPTION parses and records package, version, license, and authors.",
      "Every explicit namespace export resolves to an Rd alias.",
      "Documented exports with runnable example text meet the project floor.",
      "Applicable analytical or format behavior records a citation or Rd reference.",
      "At least one syntactically parsed testthat test file is present.",
      "Caller-supplied line coverage meets the project floor.",
      "Applicable source contains an explicit PhysioCore provenance operation.",
      "Applicable source contains explicit audit initialization or verification evidence.",
      "Applicable source contains explicit de-identification or header-scrub evidence.",
      "Caller-supplied external check evidence has no blocking result."
    ),
    method = c(
      "read.dcf", "parse_namespace_and_rd", "parse_rd_examples",
      "parse_citation_and_rd", "parse_test_files", "supplied_coverage",
      "parse_calls", "parse_calls", "parse_calls", "supplied_results"
    ),
    default_applicability = c(
      TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE
    ),
    threshold = c(
      NA_real_, NA_real_, example_floor, NA_real_, 1, coverage_floor,
      1, 1, 1, 1
    ),
    failure_severity = rep("error", length(.pc_criteria_ids)),
    policy_source = rep("physio-conformance-v1", length(.pc_criteria_ids)),
    stringsAsFactors = FALSE
  )
  class(out) <- c("conformance_criteria", "data.frame")
  out
}

.pc_source_excluded <- function(relative) {
  parts <- strsplit(relative, "/", fixed = TRUE)[[1]]
  any(parts %in% c(".git", ".Rproj.user")) ||
    any(grepl("[.]Rcheck$", parts)) ||
    grepl("[.]tar[.]gz$", relative) ||
    basename(relative) %in% c("Rplots.pdf", ".DS_Store") ||
    endsWith(basename(relative), "~")
}

.pc_source_fingerprint <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  entries <- list.files(
    path, recursive = TRUE, all.files = TRUE, full.names = TRUE,
    include.dirs = TRUE, no.. = TRUE
  )
  relative <- substring(
    enc2utf8(entries),
    nchar(enc2utf8(path)) + 2L
  )
  keep <- !vapply(relative, .pc_source_excluded, logical(1))
  entries <- entries[keep]
  relative <- gsub("\\\\", "/", relative[keep])
  links <- Sys.readlink(entries)
  if (any(nzchar(links))) {
    .pc_abort(sprintf(
      "Package source contains an internal symbolic link: %s.",
      relative[which(nzchar(links))[[1]]]
    ))
  }
  info <- file.info(entries)
  regular <- !is.na(info$isdir) & !info$isdir &
    vapply(entries, function(file) utils::file_test("-f", file), logical(1))
  entries <- entries[regular]
  relative <- relative[regular]
  info <- info[regular, , drop = FALSE]
  ord <- order(relative, method = "radix")
  entries <- entries[ord]
  relative <- relative[ord]
  info <- info[ord, , drop = FALSE]
  hashes <- vapply(entries, function(file) {
    connection <- base::file(file, open = "rb")
    on.exit(close(connection), add = TRUE)
    .pc_hash_raw(readBin(connection, "raw", n = file.info(file)$size))
  }, character(1))
  inventory <- lapply(seq_along(relative), function(i) {
    list(
      path = relative[[i]],
      size = as.double(info$size[[i]]),
      sha256 = hashes[[i]]
    )
  })
  .pc_hash(inventory)
}

.pc_discover_packages <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  candidates <- character()
  if (file.exists(file.path(root, "DESCRIPTION"))) {
    candidates <- root
  }
  ecosystem <- file.path(root, "physio-ecosystem")
  if (dir.exists(ecosystem)) {
    direct <- list.dirs(ecosystem, recursive = FALSE, full.names = TRUE)
    direct <- direct[file.exists(file.path(direct, "DESCRIPTION"))]
    candidates <- c(candidates, direct)
  }
  candidates <- sort(unique(candidates), method = "radix")
  if (!length(candidates)) {
    .pc_abort("No R package DESCRIPTION files were discovered.")
  }
  packages <- lapply(candidates, function(path) {
    if (nzchar(Sys.readlink(path))) {
      .pc_abort("A discovered package directory is a symbolic link.")
    }
    description_path <- file.path(path, "DESCRIPTION")
    dcf <- tryCatch(
      read.dcf(description_path),
      error = function(e) e
    )
    if (inherits(dcf, "error") || !nrow(dcf)) {
      return(list(
        package = basename(path), version = NA_character_, path = path,
        description = NULL,
        description_error = "DESCRIPTION could not be parsed."
      ))
    }
    fields <- as.list(dcf[1, , drop = TRUE])
    package <- fields$Package %||% ""
    if (!nzchar(package)) {
      package <- basename(path)
    }
    list(
      package = package,
      version = fields$Version %||% NA_character_,
      path = path,
      description = fields,
      description_error = NULL
    )
  })
  names_found <- vapply(packages, `[[`, character(1), "package")
  if (anyDuplicated(names_found)) {
    .pc_abort("Discovered DESCRIPTION files contain duplicated package names.")
  }
  packages[order(names_found, method = "radix")]
}

.pc_namespace_exports <- function(path) {
  if (!file.exists(path)) {
    return(list(exports = character(), error = "NAMESPACE is missing."))
  }
  expressions <- tryCatch(parse(path, keep.source = FALSE), error = identity)
  if (inherits(expressions, "error")) {
    return(list(exports = character(), error = "NAMESPACE does not parse."))
  }
  exports <- character()
  for (expression in expressions) {
    if (!is.call(expression) || !as.character(expression[[1]]) %in%
        c("export", "exportMethods")) {
      next
    }
    args <- as.list(expression)[-1L]
    exports <- c(exports, vapply(args, function(arg) {
      if (is.symbol(arg) || is.character(arg)) as.character(arg) else ""
    }, character(1)))
  }
  list(
    exports = sort(unique(exports[nzchar(exports)]), method = "radix"),
    error = NULL
  )
}

.pc_rd_text <- function(node, tag) {
  found <- character()
  walk <- function(x) {
    if (is.list(x)) {
      if (identical(attr(x, "Rd_tag"), tag)) {
        found <<- c(found, paste(unlist(x), collapse = ""))
      }
      lapply(x, walk)
    }
    invisible(NULL)
  }
  walk(node)
  trimws(found)
}

.pc_rd_inventory <- function(package_path) {
  files <- sort(
    list.files(
      file.path(package_path, "man"), pattern = "[.]Rd$",
      full.names = TRUE
    ),
    method = "radix"
  )
  aliases <- examples <- references <- character()
  errors <- character()
  for (file in files) {
    rd <- tryCatch(tools::parse_Rd(file), error = identity)
    relative <- file.path("man", basename(file))
    if (inherits(rd, "error")) {
      errors <- c(errors, relative)
      next
    }
    file_aliases <- .pc_rd_text(rd, "\\alias")
    aliases <- c(aliases, file_aliases)
    if (any(nzchar(.pc_rd_text(rd, "\\examples")))) {
      examples <- c(examples, file_aliases)
    }
    if (any(nzchar(.pc_rd_text(rd, "\\references")))) {
      references <- c(references, file_aliases)
    }
  }
  list(
    aliases = sort(unique(aliases), method = "radix"),
    examples = sort(unique(examples), method = "radix"),
    references = sort(unique(references), method = "radix"),
    errors = sort(unique(errors), method = "radix")
  )
}

.pc_call_name <- function(head) {
  if (is.symbol(head)) {
    return(as.character(head))
  }
  if (is.call(head) && length(head) >= 3L &&
      is.symbol(head[[1]]) &&
      as.character(head[[1]]) %in% c("::", ":::")) {
    return(as.character(head[[3]]))
  }
  ""
}

.pc_parse_calls <- function(files) {
  calls <- character()
  errors <- character()
  walk <- function(node) {
    if (is.call(node)) {
      name <- .pc_call_name(node[[1]])
      if (nzchar(name)) {
        calls <<- c(calls, name)
      }
      if (name %in% c("quote", "substitute", "expression", "alist", "bquote")) {
        return(invisible(NULL))
      }
      lapply(as.list(node), walk)
    } else if (is.pairlist(node) || is.expression(node)) {
      lapply(as.list(node), walk)
    }
    invisible(NULL)
  }
  for (file in files) {
    parsed <- tryCatch(parse(file, keep.source = FALSE), error = identity)
    if (inherits(parsed, "error")) {
      errors <- c(errors, file)
    } else {
      walk(parsed)
    }
  }
  list(
    calls = sort(unique(calls), method = "radix"),
    errors = sort(unique(errors), method = "radix")
  )
}

.pc_static_inspection <- function(package) {
  path <- package$path
  namespace <- .pc_namespace_exports(file.path(path, "NAMESPACE"))
  rd <- .pc_rd_inventory(path)
  r_files <- sort(
    list.files(file.path(path, "R"), pattern = "[.][Rr]$", full.names = TRUE),
    method = "radix"
  )
  test_files <- sort(
    list.files(
      file.path(path, "tests", "testthat"),
      pattern = "^test-.*[.][Rr]$", full.names = TRUE
    ),
    method = "radix"
  )
  parsed <- .pc_parse_calls(c(r_files, test_files))
  relative_errors <- vapply(parsed$errors, function(file) {
    substring(
      normalizePath(file, winslash = "/", mustWork = FALSE),
      nchar(normalizePath(path, winslash = "/", mustWork = TRUE)) + 2L
    )
  }, character(1))
  documented <- namespace$exports %in% rd$aliases
  example_fraction <- if (!length(namespace$exports)) {
    1
  } else {
    mean(namespace$exports %in% rd$examples)
  }
  description <- package$description
  description_valid <- !is.null(description) &&
    all(vapply(
      c("Package", "Version", "License"),
      function(field) nzchar(description[[field]] %||% ""),
      logical(1)
    )) &&
    nzchar(description[["Authors@R"]] %||% description[["Author"]] %||% "")
  imports <- paste(
    description[["Depends"]] %||% "",
    description[["Imports"]] %||% "",
    description[["Suggests"]] %||% ""
  )
  description_text <- tolower(paste(unlist(description), collapse = " "))
  list(
    description_valid = description_valid,
    description_error = package$description_error,
    namespace_error = namespace$error,
    exports = namespace$exports,
    undocumented = namespace$exports[!documented],
    example_fraction = example_fraction,
    rd_references = length(rd$references),
    citation = file.exists(file.path(path, "inst", "CITATION")),
    rd_errors = rd$errors,
    test_count = length(test_files),
    parse_errors = relative_errors,
    calls = parsed$calls,
    imports_compliance = grepl("\\bPhysioCompliance\\b", imports),
    imports_core = grepl("\\bPhysioCore\\b", imports),
    behavior_reference_applicable = length(namespace$exports) > 0L &&
      grepl(
        "signal|analysis|clinical|physio|statistic|format|reader|writer|import|export",
        description_text
      ),
    deid_applicable = grepl(
      "subject|participant|clinical|header|reader|writer|file|export|import",
      description_text
    ) || any(grepl("^(read|write|export|import)", namespace$exports))
  )
}

.pc_validate_applicability <- function(value, packages, criteria, as_of) {
  if (is.null(value)) {
    return(NULL)
  }
  columns <- c(
    "package", "criterion_id", "applicable", "rationale", "reviewer",
    "reviewed_on"
  )
  value <- .pc_plain_df(value, "applicability", columns)
  .pc_required_strings(
    value, c("package", "criterion_id", "reviewer", "reviewed_on"),
    "applicability"
  )
  .pc_optional_strings(value, "rationale", "applicability")
  if (!is.logical(value$applicable) || anyNA(value$applicable) ||
      any(!value$package %in% packages) ||
      any(!value$criterion_id %in% criteria) ||
      any(!vapply(value$reviewed_on, .pc_iso_date, logical(1))) ||
      any(as.Date(value$reviewed_on) > as.Date(as_of)) ||
      any(!value$applicable & (
        is.na(value$rationale) | !nzchar(trimws(value$rationale))
      ))) {
    .pc_abort("`applicability` contains an invalid reviewed decision.")
  }
  key <- paste(value$package, value$criterion_id, sep = "\r")
  if (anyDuplicated(key)) {
    .pc_abort("`applicability` contains duplicated package/criterion rows.")
  }
  value[
    order(value$package, value$criterion_id, method = "radix"), , drop = FALSE
  ]
}

.pc_evidence_table <- function(value, name, columns, packages) {
  if (is.null(value)) {
    return(NULL)
  }
  value <- .pc_plain_df(value, name, columns)
  if (any(!value$package %in% packages)) {
    .pc_abort(sprintf("`%s` names an unknown package.", name))
  }
  value
}

.pc_validate_coverage <- function(value, packages) {
  columns <- c(
    "package", "line_coverage", "tool", "tool_version", "run_id",
    "executed_at", "source_hash"
  )
  value <- .pc_evidence_table(value, "coverage", columns, packages)
  if (is.null(value)) {
    return(NULL)
  }
  .pc_required_strings(
    value,
    c("package", "tool", "tool_version", "run_id", "executed_at", "source_hash"),
    "coverage"
  )
  if (!is.numeric(value$line_coverage) ||
      anyNA(value$line_coverage) ||
      any(!is.finite(value$line_coverage)) ||
      any(value$line_coverage < 0 | value$line_coverage > 1) ||
      any(!vapply(value$executed_at, .pc_is_timestamp, logical(1))) ||
      any(!grepl("^[0-9a-f]{64}$", value$source_hash)) ||
      anyDuplicated(value$package)) {
    .pc_abort("`coverage` contains invalid or duplicated execution evidence.")
  }
  value[order(value$package, method = "radix"), , drop = FALSE]
}

.pc_validate_external <- function(value, packages) {
  columns <- c(
    "package", "tool", "tool_version", "run_id", "executed_at",
    "source_hash", "errors", "warnings", "notes", "status",
    "evidence_uri", "evidence_sha256"
  )
  value <- .pc_evidence_table(value, "external_results", columns, packages)
  if (is.null(value)) {
    return(NULL)
  }
  .pc_required_strings(
    value,
    c(
      "package", "tool", "tool_version", "run_id", "executed_at",
      "source_hash", "status", "evidence_uri", "evidence_sha256"
    ),
    "external_results"
  )
  counts <- c("errors", "warnings", "notes")
  bad_counts <- vapply(counts, function(column) {
    x <- value[[column]]
    !is.numeric(x) || anyNA(x) || any(!is.finite(x)) ||
      any(x < 0) || any(x != floor(x))
  }, logical(1))
  key <- paste(value$package, value$tool, value$run_id, sep = "\r")
  safe_uri <- !startsWith(value$evidence_uri, "/") &
    !startsWith(value$evidence_uri, "~") &
    !grepl("^[A-Za-z]:", value$evidence_uri) &
    !grepl("^[A-Za-z][A-Za-z0-9+.-]*:", value$evidence_uri) &
    !grepl("\\\\", value$evidence_uri) &
    !grepl("(^|/)[.][.]($|/)", value$evidence_uri)
  if (any(bad_counts) ||
      any(!value$status %in% c("pass", "fail")) ||
      any(!vapply(value$executed_at, .pc_is_timestamp, logical(1))) ||
      any(!grepl("^[0-9a-f]{64}$", value$source_hash)) ||
      any(!grepl("^[0-9a-f]{64}$", value$evidence_sha256)) ||
      any(!safe_uri) ||
      anyDuplicated(key)) {
    .pc_abort("`external_results` contains invalid execution evidence.")
  }
  value[
    order(value$package, value$tool, value$run_id, method = "radix"),
    ,
    drop = FALSE
  ]
}

.pc_inferred_applicability <- function(criterion_id, inspection, package) {
  if (criterion_id %in% c(
    "DESCRIPTION_VALID", "NAMESPACE_DOCUMENTED", "EXAMPLE_COVERAGE",
    "TESTS_PRESENT", "TEST_COVERAGE", "EXTERNAL_CHECKS"
  )) {
    return(TRUE)
  }
  switch(
    criterion_id,
    REFERENCE_PRESENT = inspection$behavior_reference_applicable,
    PROVENANCE_READINESS = inspection$imports_core ||
      "PhysioExperiment" %in% inspection$calls,
    AUDIT_READINESS = identical(package, "PhysioCompliance") ||
      inspection$imports_compliance ||
      any(inspection$calls %in%
          c("initializeAuditTrail", "verifyAuditTrail", "appendAuditEvent")),
    DEIDENTIFICATION_READINESS = identical(package, "PhysioCompliance") ||
      inspection$deid_applicable,
    FALSE
  )
}

.pc_result_row <- function(package, version, criterion_id, applicable,
                           source, status, measured, threshold, evidence,
                           rule_id, message) {
  data.frame(
    package = package,
    version = version,
    criterion_id = criterion_id,
    applicable = applicable,
    applicability_source = source,
    status = status,
    measured = measured,
    threshold = threshold,
    evidence = evidence,
    rule_id = rule_id,
    message = message,
    stringsAsFactors = FALSE
  )
}

.pc_evaluate_criterion <- function(
    criterion_id, package, source_hash, inspection, criteria,
    coverage, external) {
  threshold_value <- criteria$threshold[
    match(criterion_id, criteria$criterion_id)
  ]
  threshold <- if (is.na(threshold_value)) NA_character_ else
    format(threshold_value, scientific = FALSE, trim = TRUE)
  parse_problem <- length(inspection$parse_errors) ||
    length(inspection$rd_errors)
  if (criterion_id == "DESCRIPTION_VALID") {
    ok <- inspection$description_valid
    return(list(
      status = if (ok) "PASS" else "ERROR",
      measured = if (ok) "valid" else "invalid",
      threshold = NA_character_,
      evidence = "DESCRIPTION",
      rule_id = "DESCRIPTION_SCHEMA",
      message = if (ok) "DESCRIPTION metadata parsed." else
        "DESCRIPTION metadata is missing or malformed."
    ))
  }
  if (criterion_id == "NAMESPACE_DOCUMENTED") {
    ok <- is.null(inspection$namespace_error) &&
      !length(inspection$undocumented) && !length(inspection$rd_errors)
    evidence <- if (length(inspection$undocumented)) {
      paste(inspection$undocumented, collapse = ",")
    } else {
      "NAMESPACE;man/"
    }
    return(list(
      status = if (ok) "PASS" else "FAIL",
      measured = sprintf(
        "%d/%d documented",
        length(inspection$exports) - length(inspection$undocumented),
        length(inspection$exports)
      ),
      threshold = "all explicit exports",
      evidence = evidence,
      rule_id = "NAMESPACE_ALIAS",
      message = if (ok) "Explicit exports resolve to Rd aliases." else
        "A namespace or Rd parse/documentation gap was found."
    ))
  }
  if (criterion_id == "EXAMPLE_COVERAGE") {
    ok <- !parse_problem &&
      inspection$example_fraction >= threshold_value
    return(list(
      status = if (ok) "PASS" else "FAIL",
      measured = sprintf("%.6f", inspection$example_fraction),
      threshold = threshold,
      evidence = "man/",
      rule_id = "EXAMPLE_FLOOR",
      message = if (ok) "Runnable example-text fraction meets the project floor." else
        "Runnable example-text fraction is below the project floor or Rd did not parse."
    ))
  }
  if (criterion_id == "REFERENCE_PRESENT") {
    ok <- inspection$citation || inspection$rd_references > 0L
    return(list(
      status = if (ok) "PASS" else "FAIL",
      measured = as.character(inspection$rd_references),
      threshold = "at least one source",
      evidence = if (inspection$citation) "inst/CITATION" else "man/",
      rule_id = "REFERENCE_SOURCE",
      message = if (ok) "A citation or non-empty Rd reference is recorded." else
        "No applicable citation or Rd reference was found."
    ))
  }
  if (criterion_id == "TESTS_PRESENT") {
    ok <- inspection$test_count > 0L && !length(inspection$parse_errors)
    return(list(
      status = if (ok) "PASS" else "FAIL",
      measured = as.character(inspection$test_count),
      threshold = "at least one parsed test file",
      evidence = "tests/testthat/",
      rule_id = "TEST_FILE",
      message = if (ok) "Parsed testthat files are present." else
        "No parsed testthat file is available or source syntax failed."
    ))
  }
  if (criterion_id == "TEST_COVERAGE") {
    row <- if (is.null(coverage)) coverage else
      coverage[coverage$package == package$package, , drop = FALSE]
    if (is.null(row) || !nrow(row) ||
        !identical(row$source_hash[[1]], source_hash)) {
      return(list(
        status = "NOT_EVALUATED", measured = NA_character_,
        threshold = threshold,
        evidence = if (!is.null(row) && nrow(row)) row$run_id[[1]] else NA_character_,
        rule_id = "COVERAGE_EVIDENCE",
        message = "Matching caller-supplied coverage evidence was not available."
      ))
    }
    ok <- row$line_coverage[[1]] >= threshold_value
    return(list(
      status = if (ok) "PASS" else "FAIL",
      measured = sprintf("%.6f", row$line_coverage[[1]]),
      threshold = threshold,
      evidence = row$run_id[[1]],
      rule_id = "COVERAGE_FLOOR",
      message = if (ok) "Supplied line coverage meets the project floor." else
        "Supplied line coverage is below the project floor."
    ))
  }
  call_sets <- list(
    PROVENANCE_READINESS = c(
      "recordProvenance", "addProvenance", "appendProvenance",
      "provenance"
    ),
    AUDIT_READINESS = c(
      "initializeAuditTrail", "verifyAuditTrail", "appendAuditEvent"
    ),
    DEIDENTIFICATION_READINESS = c(
      "deidentify", "headerScrub", "auditDeidentification"
    )
  )
  if (criterion_id %in% names(call_sets)) {
    found <- intersect(inspection$calls, call_sets[[criterion_id]])
    ok <- length(found) > 0L && !length(inspection$parse_errors)
    return(list(
      status = if (ok) "PASS" else "FAIL",
      measured = as.character(length(found)),
      threshold = "at least one parsed call",
      evidence = if (length(found)) paste(found, collapse = ",") else "R/;tests/testthat/",
      rule_id = criterion_id,
      message = if (ok) "Explicit parsed call evidence is present." else
        "Applicable explicit parsed call evidence was not found."
    ))
  }
  if (criterion_id == "EXTERNAL_CHECKS") {
    rows <- if (is.null(external)) external else
      external[external$package == package$package, , drop = FALSE]
    fresh <- if (is.null(rows) || !nrow(rows)) rows else
      rows[rows$source_hash == source_hash, , drop = FALSE]
    if (is.null(fresh) || !nrow(fresh)) {
      return(list(
        status = "NOT_EVALUATED", measured = NA_character_,
        threshold = "no blocking result",
        evidence = if (!is.null(rows) && nrow(rows))
          paste(rows$run_id, collapse = ",") else NA_character_,
        rule_id = "EXTERNAL_EVIDENCE",
        message = "Matching caller-supplied external check evidence was not available."
      ))
    }
    failed <- fresh$status == "fail" | fresh$errors > 0
    ok <- !any(failed)
    return(list(
      status = if (ok) "PASS" else "FAIL",
      measured = sprintf(
        "%d runs; %d blocking", nrow(fresh), sum(failed)
      ),
      threshold = "no blocking result",
      evidence = paste(fresh$run_id, collapse = ","),
      rule_id = "EXTERNAL_RESULT",
      message = if (ok) "Supplied external checks have no blocking result." else
        "At least one supplied external check has a blocking result."
    ))
  }
  .pc_abort("An internal conformance criterion was not implemented.")
}

.pc_conformance_content <- function(x) {
  x[c(
    "schema_version", "checked_at", "root_id", "criteria", "packages",
    "results", "summary", "source_editions"
  )]
}

#' Run a read-only ecosystem engineering-readiness audit
#'
#' This function statically parses package metadata and source. It does not
#' load namespaces, execute package code, run tests, or launch external tools.
#'
#' @param root Repository or package root.
#' @param packages Optional exact package-name filter.
#' @param criteria Project criteria from [conformanceCriteria()].
#' @param coverage Caller-supplied coverage evidence or `NULL`.
#' @param external_results Caller-supplied external check evidence or `NULL`.
#' @param applicability Reviewed applicability decisions or `NULL`.
#' @param checked_at Fixed check timestamp.
#' @return A deterministic `conformance_report`.
#' @export
conformanceCheck <- function(
    root, packages = NULL, criteria = conformanceCriteria(), coverage = NULL,
    external_results = NULL, applicability = NULL,
    checked_at = Sys.time()) {
  if (!is.character(root) || length(root) != 1L || is.na(root) ||
      !dir.exists(root)) {
    .pc_abort("`root` must be one existing directory.")
  }
  if (nzchar(Sys.readlink(root))) {
    .pc_abort("`root` must not be a symbolic link.")
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  criteria_columns <- c(
    "criterion_id", "description", "method", "default_applicability",
    "threshold", "failure_severity", "policy_source"
  )
  criteria_shape <- identical(
    class(criteria), c("conformance_criteria", "data.frame")
  ) &&
    identical(names(criteria), criteria_columns) &&
    identical(criteria$criterion_id, .pc_criteria_ids) &&
    is.numeric(criteria$threshold) &&
    length(criteria$threshold) == length(.pc_criteria_ids)
  expected_criteria <- NULL
  if (criteria_shape) {
    expected_criteria <- tryCatch(
      conformanceCriteria(
        example_floor = criteria$threshold[[3L]],
        coverage_floor = criteria$threshold[[6L]]
      ),
      error = function(e) NULL
    )
  }
  if (is.null(expected_criteria) || !identical(criteria, expected_criteria)) {
    .pc_abort("`criteria` must be returned by conformanceCriteria().")
  }
  checked_at <- .pc_timestamp(checked_at, "checked_at")
  as_of <- substr(checked_at, 1L, 10L)
  discovered <- .pc_discover_packages(root)
  known <- vapply(discovered, `[[`, character(1), "package")
  if (!is.null(packages)) {
    if (!is.character(packages) || anyNA(packages) ||
        any(!nzchar(packages)) || anyDuplicated(packages) ||
        any(!packages %in% known)) {
      .pc_abort("`packages` must contain unique, exact discovered package names.")
    }
    discovered <- discovered[known %in% packages]
    discovered <- discovered[
      order(vapply(discovered, `[[`, character(1), "package"),
            method = "radix")
    ]
    known <- vapply(discovered, `[[`, character(1), "package")
  }
  applicability <- .pc_validate_applicability(
    applicability, known, criteria$criterion_id, as_of
  )
  coverage <- .pc_validate_coverage(coverage, known)
  external_results <- .pc_validate_external(external_results, known)

  package_rows <- list()
  results <- list()
  for (i in seq_along(discovered)) {
    package <- discovered[[i]]
    fingerprint <- tryCatch(
      .pc_source_fingerprint(package$path),
      error = function(e) NA_character_
    )
    package_rows[[i]] <- data.frame(
      package = package$package,
      version = package$version,
      source_hash = fingerprint,
      stringsAsFactors = FALSE
    )
    inspection <- .pc_static_inspection(package)
    for (criterion_id in criteria$criterion_id) {
      reviewed <- if (is.null(applicability)) {
        applicability
      } else {
        applicability[
          applicability$package == package$package &
            applicability$criterion_id == criterion_id,
          ,
          drop = FALSE
        ]
      }
      if (!is.null(reviewed) && nrow(reviewed)) {
        applicable <- reviewed$applicable[[1]]
        applicability_source <- "reviewed"
      } else {
        applicable <- .pc_inferred_applicability(
          criterion_id, inspection, package$package
        )
        applicability_source <- "inferred"
      }
      threshold_value <- criteria$threshold[
        match(criterion_id, criteria$criterion_id)
      ]
      threshold <- if (is.na(threshold_value)) NA_character_ else
        format(threshold_value, scientific = FALSE, trim = TRUE)
      if (!applicable) {
        evaluated <- list(
          status = "NOT_APPLICABLE", measured = NA_character_,
          threshold = threshold, evidence = NA_character_,
          rule_id = criterion_id,
          message = "Criterion was recorded as not applicable."
        )
      } else if (is.na(fingerprint)) {
        evaluated <- list(
          status = "ERROR", measured = NA_character_,
          threshold = threshold, evidence = NA_character_,
          rule_id = "SOURCE_FINGERPRINT",
          message = "Package source fingerprinting failed."
        )
      } else {
        evaluated <- .pc_evaluate_criterion(
          criterion_id, package, fingerprint, inspection, criteria,
          coverage, external_results
        )
      }
      results[[length(results) + 1L]] <- .pc_result_row(
        package$package, package$version, criterion_id, applicable,
        applicability_source, evaluated$status, evaluated$measured,
        evaluated$threshold, evaluated$evidence, evaluated$rule_id,
        evaluated$message
      )
    }
  }
  package_table <- .pc_no_rownames(do.call(rbind, package_rows))
  package_table <- package_table[
    order(package_table$package, method = "radix"), , drop = FALSE
  ]
  result_table <- .pc_no_rownames(do.call(rbind, results))
  result_table <- result_table[
    order(
      result_table$package,
      match(result_table$criterion_id, criteria$criterion_id),
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  package_status <- lapply(package_table$package, function(name) {
    status <- result_table$status[result_table$package == name]
    final <- if (any(status %in% c("FAIL", "ERROR"))) {
      "FAIL"
    } else if (any(status == "NOT_EVALUATED")) {
      "INCOMPLETE"
    } else {
      "PASS"
    }
    data.frame(package = name, status = final, stringsAsFactors = FALSE)
  })
  package_status <- .pc_no_rownames(do.call(rbind, package_status))
  status_levels <- c(
    "PASS", "FAIL", "NOT_EVALUATED", "NOT_APPLICABLE", "ERROR"
  )
  result_counts <- data.frame(
    status = status_levels,
    n = as.integer(vapply(
      status_levels, function(status) sum(result_table$status == status),
      integer(1)
    )),
    stringsAsFactors = FALSE
  )
  criterion_counts <- stats::aggregate(
    list(n = result_table$criterion_id),
    list(
      criterion_id = result_table$criterion_id,
      status = result_table$status
    ),
    length
  )
  criterion_counts <- criterion_counts[
    order(
      match(criterion_counts$criterion_id, criteria$criterion_id),
      match(criterion_counts$status, status_levels),
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
  rownames(criterion_counts) <- NULL
  fields <- list(
    schema_version = "1",
    checked_at = checked_at,
    root_id = basename(root),
    criteria = criteria,
    packages = package_table,
    results = result_table,
    summary = list(
      packages = package_status,
      results = result_counts,
      criteria = criterion_counts
    ),
    source_editions = standardsSources(
      max_age_days = 365L, as_of = as.Date(as_of)
    )
  )
  fields$report_hash <- .pc_hash(fields)
  structure(fields, class = "conformance_report")
}

#' @export
as.data.frame.conformance_report <- function(x, ...) {
  x$results
}

#' @export
print.conformance_report <- function(x, ...) {
  cat("<conformance_report>\n")
  cat("  engineering readiness check\n")
  cat("  root id:", x$root_id, "\n")
  cat("  checked at:", x$checked_at, "\n")
  cat("  packages:", nrow(x$packages), "\n")
  counts <- stats::setNames(
    x$summary$results$n, x$summary$results$status
  )
  cat("  PASS:", counts[["PASS"]], "\n")
  cat("  FAIL:", counts[["FAIL"]] + counts[["ERROR"]], "\n")
  cat("  NOT_EVALUATED:", counts[["NOT_EVALUATED"]], "\n")
  cat("  report hash:", x$report_hash, "\n")
  cat("  This report records project engineering-readiness evidence; it is not a conformity conclusion.\n")
  invisible(x)
}
