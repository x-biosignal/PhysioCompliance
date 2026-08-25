.pc_plain_df <- function(value, name, columns) {
  if (!is.data.frame(value) || !identical(class(value), "data.frame") ||
      !identical(names(value), columns) || anyDuplicated(names(value)) ||
      any(vapply(value, is.factor, logical(1))) ||
      any(vapply(value, is.list, logical(1))) ||
      any(vapply(value, function(column) {
        length(attributes(column)) > 0L
      }, logical(1)))) {
    .pc_abort(sprintf(
      "`%s` must be a plain data frame with exact columns: %s.",
      name, paste(columns, collapse = ", ")
    ))
  }
  canonical <- as.data.frame(
    lapply(value, unname),
    optional = TRUE,
    stringsAsFactors = FALSE
  )
  names(canonical) <- columns
  rownames(canonical) <- NULL
  canonical
}

.pc_required_strings <- function(value, columns, name) {
  for (column in columns) {
    x <- value[[column]]
    if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x))) ||
        any(Encoding(x) == "bytes") ||
        anyNA(iconv(x, from = "", to = "UTF-8", sub = NA_character_))) {
      .pc_abort(sprintf(
        "`%s$%s` must contain non-empty UTF-8 strings.", name, column
      ))
    }
  }
  invisible(value)
}

.pc_no_rownames <- function(value) {
  rownames(value) <- NULL
  value
}

#' Define a project-owned risk decision matrix
#'
#' The constructor records caller-supplied ordinal levels and one explicit
#' decision for every severity/probability pair. It does not multiply ranks or
#' infer a risk-acceptability policy.
#'
#' @param severity Severity levels with `level_id`, `rank`, `label`, and
#'   `definition`.
#' @param probability Probability levels with the same columns.
#' @param decisions One explicit decision for every Cartesian level pair.
#' @param matrix_id,version,rationale Non-empty project identifiers and policy
#'   rationale.
#' @return A deterministic `risk_matrix` object.
#' @export
riskMatrix <- function(
    severity, probability, decisions, matrix_id, version, rationale) {
  level_columns <- c("level_id", "rank", "label", "definition")
  severity <- .pc_plain_df(severity, "severity", level_columns)
  probability <- .pc_plain_df(probability, "probability", level_columns)
  decision_columns <- c(
    "severity_id", "probability_id", "decision", "rationale"
  )
  decisions <- .pc_plain_df(decisions, "decisions", decision_columns)
  matrix_id <- .pc_text_scalar(matrix_id, "matrix_id")
  version <- .pc_text_scalar(version, "version")
  rationale <- .pc_text_scalar(rationale, "rationale")

  validate_levels <- function(levels, name) {
    .pc_required_strings(
      levels, c("level_id", "label", "definition"), name
    )
    if (!is.numeric(levels$rank) || anyNA(levels$rank) ||
        any(!is.finite(levels$rank)) ||
        any(levels$rank != floor(levels$rank)) ||
        any(levels$rank < 1) ||
        any(levels$rank > .Machine$integer.max) ||
        anyDuplicated(levels$level_id) || anyDuplicated(levels$rank)) {
      .pc_abort(sprintf(
        "`%s` IDs and positive integer ranks must each be unique.", name
      ))
    }
    levels$rank <- as.integer(levels$rank)
    levels <- levels[
      order(levels$rank, levels$level_id, method = "radix"), , drop = FALSE
    ]
    .pc_no_rownames(levels)
  }
  severity <- validate_levels(severity, "severity")
  probability <- validate_levels(probability, "probability")
  .pc_required_strings(decisions, decision_columns, "decisions")
  allowed <- c("acceptable", "unacceptable", "review_required")
  if (any(!decisions$decision %in% allowed)) {
    .pc_abort("`decisions$decision` contains an unsupported value.")
  }
  if (any(!decisions$severity_id %in% severity$level_id) ||
      any(!decisions$probability_id %in% probability$level_id)) {
    .pc_abort("`decisions` contains an unknown severity or probability level.")
  }
  key <- paste(decisions$severity_id, decisions$probability_id, sep = "\r")
  expected <- expand.grid(
    severity_id = severity$level_id,
    probability_id = probability$level_id,
    stringsAsFactors = FALSE
  )
  expected_key <- paste(
    expected$severity_id, expected$probability_id, sep = "\r"
  )
  if (anyDuplicated(key) || length(key) != length(expected_key) ||
      !setequal(key, expected_key)) {
    .pc_abort("`decisions` must contain exactly one row for every level pair.")
  }
  severity_rank <- match(decisions$severity_id, severity$level_id)
  probability_rank <- match(
    decisions$probability_id, probability$level_id
  )
  decisions <- decisions[
    order(severity_rank, probability_rank, decisions$severity_id,
          decisions$probability_id, method = "radix"),
    ,
    drop = FALSE
  ]
  decisions <- .pc_no_rownames(decisions)
  fields <- list(
    schema_version = "1",
    matrix_id = matrix_id,
    version = version,
    rationale = rationale,
    severity = severity,
    probability = probability,
    decisions = decisions
  )
  fields$matrix_hash <- .pc_hash(fields)
  structure(fields, class = "risk_matrix")
}

.pc_risk_decision <- function(matrix, severity_id, probability_id) {
  index <- which(
    matrix$decisions$severity_id == severity_id &
      matrix$decisions$probability_id == probability_id
  )
  if (length(index) != 1L) {
    return(NA_character_)
  }
  matrix$decisions$decision[[index]]
}

.pc_risk_matrix_valid <- function(x) {
  if (!inherits(x, "risk_matrix") || !is.list(x) ||
      !identical(
        names(x),
        c(
          "schema_version", "matrix_id", "version", "rationale", "severity",
          "probability", "decisions", "matrix_hash"
        )
      ) ||
      !.pc_is_hash(x$matrix_hash)) {
    return(FALSE)
  }
  hash_valid <- identical(
    x$matrix_hash,
    .pc_hash(x[names(x) != "matrix_hash"])
  )
  if (!hash_valid) {
    return(FALSE)
  }
  rebuilt <- tryCatch(
    riskMatrix(
      x$severity, x$probability, x$decisions,
      x$matrix_id, x$version, x$rationale
    ),
    error = function(e) NULL
  )
  !is.null(rebuilt) && identical(rebuilt$matrix_hash, x$matrix_hash)
}

#' @export
print.risk_matrix <- function(x, ...) {
  cat("<risk_matrix>\n")
  cat("  matrix:", x$matrix_id, "\n")
  cat("  version:", x$version, "\n")
  cat("  severity levels:", nrow(x$severity), "\n")
  cat("  probability levels:", nrow(x$probability), "\n")
  cat("  explicit decisions:", nrow(x$decisions), "\n")
  cat("  matrix hash:", x$matrix_hash, "\n")
  invisible(x)
}
