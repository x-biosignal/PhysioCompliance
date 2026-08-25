.pc_df_records <- function(value) {
  if (!is.data.frame(value)) {
    .pc_abort("Internal JSON conversion expected a data frame.")
  }
  lapply(seq_len(nrow(value)), function(i) {
    row <- lapply(value, function(column) {
      item <- column[[i]]
      if (length(item) == 1L && is.na(item)) NULL else item
    })
    names(row) <- names(value)
    row
  })
}

.pc_report_plain <- function(x, include_results = TRUE) {
  summary <- list(
    packages = .pc_df_records(x$summary$packages),
    results = .pc_df_records(x$summary$results),
    criteria = .pc_df_records(x$summary$criteria)
  )
  out <- list(
    schema_version = x$schema_version,
    checked_at = x$checked_at,
    root_id = x$root_id,
    criteria = .pc_df_records(x$criteria),
    packages = .pc_df_records(x$packages),
    summary = summary,
    source_editions = .pc_df_records(x$source_editions),
    report_hash = x$report_hash
  )
  if (include_results) {
    out <- append(
      out,
      list(results = .pc_df_records(x$results)),
      after = 4L
    )
  }
  out
}

.pc_json_bytes <- function(value) {
  text <- jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = TRUE,
    dataframe = "rows"
  )
  charToRaw(paste0(text, "\n"))
}

.pc_markdown_escape <- function(value) {
  value <- ifelse(is.na(value), "", value)
  value <- gsub("\\|", "\\\\|", value)
  gsub("[\r\n]+", " ", value)
}

.pc_markdown_report <- function(x) {
  package_lines <- vapply(seq_len(nrow(x$summary$packages)), function(i) {
    row <- x$summary$packages[i, , drop = FALSE]
    sprintf(
      "| %s | %s |",
      .pc_markdown_escape(row$package),
      .pc_markdown_escape(row$status)
    )
  }, character(1))
  result_lines <- vapply(seq_len(nrow(x$results)), function(i) {
    row <- x$results[i, , drop = FALSE]
    sprintf(
      "| %s | %s | %s | %s | %s |",
      .pc_markdown_escape(row$package),
      .pc_markdown_escape(row$criterion_id),
      .pc_markdown_escape(row$status),
      .pc_markdown_escape(row$rule_id),
      .pc_markdown_escape(row$message)
    )
  }, character(1))
  lines <- c(
    "# Engineering Readiness Check",
    "",
    paste0("Checked at: ", x$checked_at),
    paste0("Repository identifier: ", x$root_id),
    paste0("Report hash: `", x$report_hash, "`"),
    "",
    "This report records project-supplied criteria, traceability completeness,",
    "and objective evidence status. It is not a regulatory conformity, device",
    "validation, risk-acceptability, or release conclusion.",
    "",
    "## Package Summary",
    "",
    "| Package | Status |",
    "|---|---|",
    package_lines,
    "",
    "## Criterion Results",
    "",
    "| Package | Criterion | Status | Rule | Message |",
    "|---|---|---|---|---|",
    result_lines,
    ""
  )
  charToRaw(paste(lines, collapse = "\n"))
}

.pc_stage_bytes <- function(destination, bytes) {
  parent <- dirname(destination)
  if (!dir.exists(parent) || nzchar(Sys.readlink(parent))) {
    .pc_abort("The report destination parent must be an existing non-symlink directory.")
  }
  stage <- tempfile(
    pattern = paste0(".", basename(destination), "-stage-"),
    tmpdir = parent
  )
  connection <- file(stage, open = "wb")
  tryCatch(
    writeBin(bytes, connection),
    finally = close(connection)
  )
  stage
}

.pc_commit_files <- function(stages, destinations, overwrite) {
  existing <- file.exists(destinations)
  if (any(existing & !overwrite)) {
    unlink(stages, force = TRUE)
    .pc_abort("A report destination exists; set `overwrite = TRUE` to replace it.")
  }
  if (any(nzchar(Sys.readlink(destinations[existing])))) {
    unlink(stages, force = TRUE)
    .pc_abort("A report destination must not be a symbolic link.")
  }
  backups <- rep(NA_character_, length(destinations))
  committed <- rep(FALSE, length(destinations))
  cleanup <- TRUE
  on.exit({
    if (cleanup) {
      unlink(stages[file.exists(stages)], force = TRUE)
      for (i in seq_along(destinations)) {
        if (committed[[i]] && file.exists(destinations[[i]])) {
          unlink(destinations[[i]], force = TRUE)
        }
        if (!is.na(backups[[i]]) && file.exists(backups[[i]])) {
          file.rename(backups[[i]], destinations[[i]])
        }
      }
    }
  }, add = TRUE)
  for (i in which(existing)) {
    backups[[i]] <- tempfile(
      pattern = paste0(".", basename(destinations[[i]]), "-backup-"),
      tmpdir = dirname(destinations[[i]])
    )
    if (!file.rename(destinations[[i]], backups[[i]])) {
      .pc_abort("Could not stage an existing report for replacement.")
    }
  }
  for (i in seq_along(destinations)) {
    if (!file.rename(stages[[i]], destinations[[i]])) {
      .pc_abort("Could not atomically install a report file.")
    }
    committed[[i]] <- TRUE
  }
  unlink(backups[!is.na(backups)], force = TRUE)
  cleanup <- FALSE
  invisible(destinations)
}

#' Write a deterministic conformance report
#'
#' @param x A `conformance_report`.
#' @param path Destination path.
#' @param format JSON, CSV, or Markdown output.
#' @param overwrite Whether to replace existing output atomically.
#' @return Normalized output path or paths, invisibly.
#' @export
writeConformanceReport <- function(
    x, path, format = c("json", "csv", "markdown"), overwrite = FALSE) {
  format <- match.arg(format)
  overwrite <- .pc_scalar_flag(overwrite, "overwrite")
  if (!inherits(x, "conformance_report") || !is.list(x) ||
      !identical(
        names(x),
        c(
          "schema_version", "checked_at", "root_id", "criteria", "packages",
          "results", "summary", "source_editions", "report_hash"
        )
      ) ||
      !.pc_is_hash(x$report_hash) ||
      !identical(x$report_hash, .pc_hash(.pc_conformance_content(x)))) {
    .pc_abort("`x` is malformed or its report hash does not match.")
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) ||
      !nzchar(path)) {
    .pc_abort("`path` must be one non-empty path.")
  }
  if (!dir.exists(dirname(path)) || nzchar(Sys.readlink(dirname(path)))) {
    .pc_abort(
      "The report destination parent must be an existing non-symlink directory."
    )
  }
  parent <- normalizePath(dirname(path), winslash = "/", mustWork = TRUE)
  destination <- file.path(parent, basename(path))
  destinations <- destination
  if (format == "json") {
    bytes <- list(.pc_json_bytes(.pc_report_plain(x)))
  } else if (format == "markdown") {
    bytes <- list(.pc_markdown_report(x))
  } else {
    csv_stage <- tempfile(pattern = ".physio-csv-", tmpdir = parent)
    utils::write.csv(
      x$results, csv_stage, row.names = FALSE, na = "", fileEncoding = "UTF-8"
    )
    csv_connection <- file(csv_stage, open = "rb")
    csv_bytes <- tryCatch(
      readBin(csv_connection, "raw", n = file.info(csv_stage)$size),
      finally = close(csv_connection)
    )
    unlink(csv_stage)
    stem <- tools::file_path_sans_ext(destination)
    metadata_path <- paste0(stem, "-metadata.json")
    destinations <- c(destination, metadata_path)
    bytes <- list(
      csv_bytes,
      .pc_json_bytes(.pc_report_plain(x, include_results = FALSE))
    )
  }
  stages <- Map(.pc_stage_bytes, destinations, bytes)
  stages <- unlist(stages, use.names = FALSE)
  .pc_commit_files(stages, destinations, overwrite)
  normalized <- normalizePath(
    destinations, winslash = "/", mustWork = TRUE
  )
  invisible(normalized)
}
