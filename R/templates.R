.pc_scalar_flag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    .pc_abort(sprintf("`%s` must be TRUE or FALSE.", name))
  }
  value
}

.pc_scalar_integer <- function(value, name, minimum = 0L) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value != floor(value) || value < minimum ||
      value > .Machine$integer.max) {
    .pc_abort(sprintf(
      "`%s` must be one finite integer at least %d.", name, minimum
    ))
  }
  as.integer(value)
}

.pc_text_scalar <- function(value, name, path_component = FALSE) {
  value <- .pc_assert_string(value, name)
  if (Encoding(value) == "bytes" ||
      is.na(iconv(value, from = "", to = "UTF-8", sub = NA_character_)) ||
      grepl("[\r\n\\x00-\\x1f\\x7f]", value, perl = TRUE)) {
    .pc_abort(sprintf("`%s` must be valid single-line UTF-8 text.", name))
  }
  if (path_component &&
      (grepl("[/\\\\]", value) || value %in% c(".", ".."))) {
    .pc_abort(sprintf("`%s` must not contain a path separator.", name))
  }
  enc2utf8(value)
}

.pc_date <- function(value, name) {
  if (!inherits(value, "Date") || length(value) != 1L || is.na(value) ||
      !is.finite(as.numeric(value))) {
    .pc_abort(sprintf("`%s` must be one finite Date value.", name))
  }
  format(value, "%Y-%m-%d")
}

.pc_iso_date <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", value)) {
    return(FALSE)
  }
  parsed <- suppressWarnings(as.Date(value))
  !is.na(parsed) && identical(format(parsed, "%Y-%m-%d"), value)
}

.pc_json_path <- function(...) {
  path <- system.file(..., package = "PhysioCompliance")
  if (!nzchar(path)) {
    .pc_abort("Required PhysioCompliance package data are unavailable.")
  }
  path
}

#' Read the bundled standards and guidance source metadata
#'
#' Reads edition metadata recorded when this package was built. It performs no
#' network request and does not determine whether a source remains applicable
#' to a project.
#'
#' @param max_age_days Non-negative integer age after which source metadata is
#'   marked for review.
#' @param as_of Date used to calculate metadata age.
#' @return A `standards_sources` data frame.
#' @export
standardsSources <- function(max_age_days = 365L, as_of = Sys.Date()) {
  max_age_days <- .pc_scalar_integer(max_age_days, "max_age_days", 0L)
  as_of_text <- .pc_date(as_of, "as_of")
  path <- .pc_json_path("standards", "sources.json")
  raw <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  required <- c(
    "source_id", "publisher", "title", "edition", "status",
    "publication_date", "checked_on", "url"
  )
  if (!is.data.frame(raw) || !identical(names(raw), required)) {
    .pc_abort("The bundled standards source schema is invalid.")
  }
  if (any(vapply(raw, is.factor, logical(1))) ||
      anyNA(raw) || anyDuplicated(raw$source_id) ||
      any(!grepl("^[A-Z][A-Z0-9_]+$", raw$source_id)) ||
      any(!startsWith(raw$url, "https://")) ||
      any(!vapply(raw$publication_date, .pc_iso_date, logical(1))) ||
      any(!vapply(raw$checked_on, .pc_iso_date, logical(1)))) {
    .pc_abort("The bundled standards source metadata are invalid.")
  }
  as_of_date <- as.Date(as_of_text)
  checked <- as.Date(raw$checked_on)
  if (any(checked > as_of_date)) {
    .pc_abort("Bundled source metadata contain a future `checked_on` date.")
  }
  raw$age_days <- as.integer(as_of_date - checked)
  raw$review_status <- ifelse(
    raw$age_days > max_age_days, "review_due", "current"
  )
  raw <- raw[order(raw$source_id, method = "radix"), , drop = FALSE]
  rownames(raw) <- NULL
  class(raw) <- c("standards_sources", "data.frame")
  raw
}

#' @export
print.standards_sources <- function(x, ...) {
  cat("<standards_sources>\n")
  cat("  sources:", nrow(x), "\n")
  cat("  current:", sum(x$review_status == "current"), "\n")
  cat("  review due:", sum(x$review_status == "review_due"), "\n")
  invisible(x)
}

.pc_template_manifest <- function() {
  root <- .pc_json_path("templates", "lifecycle-v1")
  manifest_path <- file.path(root, "template-manifest.json")
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = TRUE)
  if (is.list(manifest$sha256)) {
    manifest$sha256 <- unlist(manifest$sha256, use.names = TRUE)
  }
  required <- c(
    "schema_version", "template_set_id", "files", "sha256",
    "placeholders", "source_ids"
  )
  if (!is.list(manifest) || !identical(names(manifest), required) ||
      !identical(manifest$schema_version, "1") ||
      !identical(manifest$template_set_id, "physio-lifecycle-v1") ||
      !is.character(manifest$files) || !length(manifest$files) ||
      anyNA(manifest$files) || anyDuplicated(manifest$files) ||
      !is.character(manifest$placeholders) ||
      !identical(
        manifest$placeholders,
        c(
          "PROJECT", "INTENDED_USE", "SOFTWARE_SAFETY_CLASS", "OWNER",
          "EFFECTIVE_DATE", "SOURCE_EDITIONS"
        )
      ) ||
      !is.character(manifest$source_ids) ||
      !is.character(manifest$sha256) ||
      !identical(names(manifest$sha256), manifest$files)) {
    .pc_abort("The bundled template manifest schema is invalid.")
  }
  unsafe <- startsWith(manifest$files, "/") |
    grepl("(^|/)[.][.]($|/)", manifest$files) |
    grepl("\\\\", manifest$files)
  if (any(unsafe) ||
      any(!vapply(manifest$sha256, .pc_is_hash, logical(1)))) {
    .pc_abort("The bundled template manifest contains an unsafe member.")
  }
  known_sources <- standardsSources(
    max_age_days = .Machine$integer.max,
    as_of = Sys.Date()
  )$source_id
  if (!all(manifest$source_ids %in% known_sources)) {
    .pc_abort("The template manifest names an unknown source.")
  }
  for (i in seq_along(manifest$files)) {
    member <- file.path(root, manifest$files[[i]])
    if (!file.exists(member) || dir.exists(member) ||
        nzchar(Sys.readlink(member))) {
      .pc_abort("A bundled template member is missing or unsafe.")
    }
    bytes <- readBin(member, "raw", n = file.info(member)$size)
    if (!identical(.pc_hash_raw(bytes), unname(manifest$sha256[[i]]))) {
      .pc_abort(sprintf(
        "Bundled template hash mismatch: %s.", manifest$files[[i]]
      ))
    }
  }
  list(root = root, manifest = manifest)
}

.pc_json_escape_string <- function(value) {
  encoded <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")
  substring(encoded, 2L, nchar(encoded) - 1L)
}

.pc_render_text <- function(path, replacements) {
  size <- file.info(path)$size
  bytes <- readBin(path, "raw", n = size)
  text <- rawToChar(bytes)
  if (is.na(iconv(text, from = "UTF-8", to = "UTF-8", sub = NA_character_))) {
    .pc_abort(sprintf("Template `%s` is not valid UTF-8.", basename(path)))
  }
  json_member <- identical(tolower(tools::file_ext(path)), "json")
  for (name in names(replacements)) {
    replacement <- replacements[[name]]
    if (json_member) {
      replacement <- .pc_json_escape_string(replacement)
    }
    text <- paste(
      strsplit(
        text, paste0("{{", name, "}}"), fixed = TRUE
      )[[1]],
      collapse = replacement
    )
  }
  if (grepl("\\{\\{[A-Z][A-Z0-9_]*\\}\\}", text, perl = TRUE)) {
    .pc_abort(sprintf("Template `%s` has an unresolved placeholder.", basename(path)))
  }
  charToRaw(enc2utf8(text))
}

.pc_source_editions <- function(source_editions) {
  required <- c(
    "source_id", "publisher", "title", "edition", "status",
    "publication_date", "checked_on", "url", "age_days", "review_status"
  )
  if (!is.data.frame(source_editions) ||
      !identical(names(source_editions), required) ||
      !nrow(source_editions) || anyDuplicated(source_editions$source_id)) {
    .pc_abort("`source_editions` must be a valid standardsSources() table.")
  }
  source_editions <- source_editions[
    order(source_editions$source_id, method = "radix"), , drop = FALSE
  ]
  paste0(
    source_editions$source_id, " (", source_editions$edition, "; ",
    source_editions$status, ")",
    collapse = "; "
  )
}

.pc_atomic_directory <- function(stage, destination, overwrite) {
  if (!dir.exists(stage)) {
    .pc_abort("The staged template directory is missing.")
  }
  if (!file.exists(destination)) {
    if (!file.rename(stage, destination)) {
      .pc_abort("Could not atomically install the rendered template directory.")
    }
    return(invisible(destination))
  }
  if (!overwrite) {
    .pc_abort("The destination already exists; set `overwrite = TRUE` to replace it.")
  }
  if (nzchar(Sys.readlink(destination))) {
    .pc_abort("The destination must not be a symbolic link.")
  }
  backup <- tempfile(
    pattern = paste0(".", basename(destination), "-backup-"),
    tmpdir = dirname(destination)
  )
  if (!file.rename(destination, backup)) {
    .pc_abort("Could not stage the existing destination for replacement.")
  }
  committed <- FALSE
  on.exit({
    if (!committed && file.exists(backup) && !file.exists(destination)) {
      file.rename(backup, destination)
    }
  }, add = TRUE)
  if (!file.rename(stage, destination)) {
    .pc_abort("Could not atomically replace the rendered template directory.")
  }
  committed <- TRUE
  unlink(backup, recursive = TRUE, force = TRUE)
  invisible(destination)
}

.pc_render_template <- function(
    out_dir, project, intended_use, software_safety_class, owner,
    effective_date, source_editions, overwrite, template_type,
    risk_matrix = NULL) {
  if (!is.character(out_dir) || length(out_dir) != 1L || is.na(out_dir) ||
      !nzchar(out_dir)) {
    .pc_abort("`out_dir` must be one non-empty path.")
  }
  project <- .pc_text_scalar(project, "project", path_component = TRUE)
  intended_use <- .pc_text_scalar(intended_use, "intended_use")
  owner <- .pc_text_scalar(owner, "owner")
  effective_date <- .pc_date(effective_date, "effective_date")
  overwrite <- .pc_scalar_flag(overwrite, "overwrite")
  software_safety_class <- match.arg(
    software_safety_class, c("unclassified", "A", "B", "C")
  )
  source_text <- .pc_source_editions(source_editions)
  source_ids <- sort(source_editions$source_id, method = "radix")

  parent <- dirname(out_dir)
  if (!dir.exists(parent) || nzchar(Sys.readlink(parent))) {
    .pc_abort("The parent of `out_dir` must be an existing non-symlink directory.")
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  destination <- file.path(parent, basename(out_dir))
  if (identical(destination, parent) || basename(destination) %in% c(".", "..")) {
    .pc_abort("`out_dir` must name one child directory.")
  }

  inventory <- .pc_template_manifest()
  selected <- if (identical(template_type, "risk_management")) {
    c(
      "project-record.json", "risk-management-plan.md",
      "hazard-analysis.csv", "risk-controls.csv", "test-evidence.csv",
      "traceability-links.csv", "production-post-production-review.md"
    )
  } else {
    inventory$manifest$files
  }
  replacements <- list(
    PROJECT = project,
    INTENDED_USE = intended_use,
    SOFTWARE_SAFETY_CLASS = software_safety_class,
    OWNER = owner,
    EFFECTIVE_DATE = effective_date,
    SOURCE_EDITIONS = source_text
  )
  stage <- tempfile(
    pattern = paste0(".", basename(destination), "-stage-"),
    tmpdir = parent
  )
  if (!dir.create(stage, mode = "0700")) {
    .pc_abort("Could not create the template staging directory.")
  }
  keep_stage <- FALSE
  on.exit(if (!keep_stage) unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)

  rendered <- vector("list", length(selected))
  for (i in seq_along(selected)) {
    relative <- selected[[i]]
    bytes <- .pc_render_text(file.path(inventory$root, relative), replacements)
    target <- file.path(stage, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    connection <- file(target, open = "wb")
    tryCatch(
      writeBin(bytes, connection),
      finally = close(connection)
    )
    rendered[[i]] <- data.frame(
      path = relative,
      sha256 = .pc_hash_raw(bytes),
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(risk_matrix)) {
    if (!inherits(risk_matrix, "risk_matrix")) {
      .pc_abort("`risk_matrix` must be NULL or a risk_matrix object.")
    }
    matrix_path <- file.path(stage, "risk-matrix.json")
    matrix_plain <- list(
      schema_version = "1",
      matrix_id = risk_matrix$matrix_id,
      version = risk_matrix$version,
      rationale = risk_matrix$rationale,
      severity = .pc_df_records(risk_matrix$severity),
      probability = .pc_df_records(risk_matrix$probability),
      decisions = .pc_df_records(risk_matrix$decisions),
      matrix_hash = risk_matrix$matrix_hash
    )
    text <- jsonlite::toJSON(
      matrix_plain, auto_unbox = TRUE, null = "null", na = "null",
      digits = NA, pretty = TRUE
    )
    bytes <- charToRaw(paste0(text, "\n"))
    writeBin(bytes, matrix_path)
    rendered[[length(rendered) + 1L]] <- data.frame(
      path = "risk-matrix.json",
      sha256 = .pc_hash_raw(bytes),
      stringsAsFactors = FALSE
    )
  }
  files <- do.call(rbind, rendered)
  files <- files[order(files$path, method = "radix"), , drop = FALSE]
  rownames(files) <- NULL
  .pc_atomic_directory(stage, destination, overwrite)
  keep_stage <- TRUE

  structure(
    list(
      schema_version = "1",
      template_set_id = inventory$manifest$template_set_id,
      template_type = template_type,
      project = project,
      intended_use = intended_use,
      software_safety_class = software_safety_class,
      classification_supplied_by = owner,
      classification_review_status = "pending",
      effective_date = effective_date,
      source_ids = source_ids,
      files = files,
      content_hash = .pc_hash(list(
        template_type = template_type,
        project = project,
        intended_use = intended_use,
        software_safety_class = software_safety_class,
        owner = owner,
        effective_date = effective_date,
        source_ids = source_ids,
        files = files
      ))
    ),
    class = "lifecycle_template"
  )
}

#' Render a project-owned software lifecycle template set
#'
#' @param out_dir Destination child directory.
#' @param project Project name. Path separators are not allowed.
#' @param intended_use Intended-use statement supplied by the project.
#' @param software_safety_class Project-supplied class or `"unclassified"`.
#' @param owner Recorded document owner.
#' @param effective_date Effective date to render.
#' @param source_editions Metadata returned by [standardsSources()].
#' @param overwrite Whether to replace an existing destination atomically.
#' @return A `lifecycle_template` manifest.
#' @export
lifecycleTemplate <- function(
    out_dir, project, intended_use,
    software_safety_class = c("unclassified", "A", "B", "C"),
    owner, effective_date = Sys.Date(),
    source_editions = standardsSources(), overwrite = FALSE) {
  .pc_render_template(
    out_dir, project, intended_use, software_safety_class, owner,
    effective_date, source_editions, overwrite, "lifecycle"
  )
}

#' Render a project-owned risk-management template set
#'
#' @inheritParams lifecycleTemplate
#' @param risk_matrix Optional project-owned [riskMatrix()] object to record.
#' @return A `lifecycle_template` manifest.
#' @export
riskManagementTemplate <- function(
    out_dir, project, intended_use, owner, effective_date = Sys.Date(),
    risk_matrix = NULL, source_editions = standardsSources(),
    overwrite = FALSE) {
  .pc_render_template(
    out_dir, project, intended_use, "unclassified", owner, effective_date,
    source_editions, overwrite, "risk_management", risk_matrix
  )
}

#' @export
print.lifecycle_template <- function(x, ...) {
  cat("<lifecycle_template>\n")
  cat("  template set:", x$template_set_id, "\n")
  cat("  template type:", x$template_type, "\n")
  cat("  project:", x$project, "\n")
  cat("  recorded class:", x$software_safety_class, "\n")
  cat("  classification review:", x$classification_review_status, "\n")
  cat("  files:", nrow(x$files), "\n")
  cat("  content hash:", x$content_hash, "\n")
  invisible(x)
}
