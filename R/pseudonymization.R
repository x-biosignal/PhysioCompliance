.ps_hex <- function(value) {
  paste(sprintf("%02x", as.integer(value)), collapse = "")
}

.ps_plain_raw <- function(value) {
  value <- as.raw(value)
  attributes(value) <- NULL
  value
}

.ps_sha256 <- function(value) {
  .ps_plain_raw(openssl::sha256(value))
}

.ps_hmac <- function(key, value) {
  .ps_plain_raw(openssl::sha256(value, key = key))
}

.ps_key <- function(key) {
  if (!is.raw(key) || length(key) < 32L) {
    .pc_abort("`key` must be one raw vector of at least 32 bytes.")
  }
  .ps_plain_raw(key)
}

.ps_utf8_scalar <- function(value, name) {
  value <- .pc_assert_string(value, name)
  converted <- iconv(value, from = "", to = "UTF-8", sub = NA_character_)
  if (is.na(converted) || grepl("[[:cntrl:]]", converted)) {
    .pc_abort(sprintf("`%s` must be valid UTF-8 without control characters.",
                      name))
  }
  enc2utf8(converted)
}

.ps_identifiers <- function(identifiers) {
  if (!is.character(identifiers) || is.factor(identifiers)) {
    .pc_abort("`identifiers` must be a character vector.")
  }
  out <- identifiers
  keep <- !is.na(out)
  if (any(keep)) {
    converted <- iconv(out[keep], from = "", to = "UTF-8",
                       sub = NA_character_)
    if (anyNA(converted) || any(!nzchar(converted)) ||
        any(grepl("[[:cntrl:]]", converted))) {
      .pc_abort(
        "Non-missing identifiers must be non-empty valid UTF-8 without control characters."
      )
    }
    out[keep] <- enc2utf8(converted)
  }
  names(out) <- NULL
  out
}

.ps_context <- function(...) {
  values <- list(...)
  pieces <- lapply(values, function(value) {
    if (is.raw(value)) {
      .ps_plain_raw(value)
    } else {
      charToRaw(enc2utf8(as.character(value)))
    }
  })
  out <- raw()
  for (i in seq_along(pieces)) {
    if (i > 1L) {
      out <- c(out, as.raw(0L))
    }
    out <- c(out, pieces[[i]])
  }
  out
}

.ps_subkeys <- function(key) {
  token_key <- .ps_hmac(
    key,
    charToRaw("PhysioCompliance/token/v1")
  )
  map_key <- .ps_hmac(
    key,
    charToRaw("PhysioCompliance/map/v1")
  )
  list(
    token_key = token_key,
    map_key = map_key,
    encryption_key = .ps_hmac(
      map_key,
      charToRaw("PhysioCompliance/map/encryption/v1")
    ),
    authentication_key = .ps_hmac(
      map_key,
      charToRaw("PhysioCompliance/map/authentication/v1")
    )
  )
}

.ps_constant_equal <- function(x, y) {
  if (!is.raw(x) || !is.raw(y) || length(x) != length(y)) {
    return(FALSE)
  }
  !any(bitwXor(as.integer(x), as.integer(y)) != 0L)
}

.ps_token <- function(identifier, namespace, token_key) {
  mac <- .ps_hmac(
    token_key,
    .ps_context(namespace, identifier)
  )
  paste0("psn_", substr(.ps_hex(mac), 1L, 32L))
}

.ps_random_token <- function() {
  paste0("psn_", .ps_hex(openssl::rand_bytes(16L)))
}

.ps_map_valid <- function(map) {
  is.data.frame(map) &&
    identical(names(map), c("identifier", "token")) &&
    is.character(map$identifier) && is.character(map$token) &&
    !anyNA(map$identifier) && !anyNA(map$token) &&
    !anyDuplicated(map$identifier) && !anyDuplicated(map$token) &&
    all(nzchar(map$identifier)) &&
    all(grepl("^psn_[0-9a-f]{32}$", map$token))
}

.ps_canonical_map <- function(map) {
  if (!.ps_map_valid(map)) {
    .pc_abort("The protected identifier map is malformed.")
  }
  out <- data.frame(
    identifier = enc2utf8(map$identifier),
    token = map$token,
    stringsAsFactors = FALSE
  )
  out <- out[order(enc2utf8(out$identifier), method = "radix"), ,
             drop = FALSE]
  rownames(out) <- NULL
  out
}

.ps_envelope_fields <- c(
  "schema_version", "cipher", "token_method", "namespace", "iv",
  "ciphertext", "authentication_tag"
)

.ps_envelope_payload <- function(envelope) {
  .pc_serialize(envelope[setdiff(
    .ps_envelope_fields,
    "authentication_tag"
  )])
}

.ps_encrypt_map <- function(map, namespace, method, subkeys) {
  map <- .ps_canonical_map(map)
  plaintext <- .pc_serialize(map)
  iv <- .ps_plain_raw(openssl::rand_bytes(12L))
  ciphertext <- .ps_plain_raw(openssl::aes_gcm_encrypt(
    plaintext,
    key = subkeys$encryption_key,
    iv = iv
  ))
  envelope <- list(
    schema_version = "1",
    cipher = "aes-256-gcm+hmac-sha256",
    token_method = method,
    namespace = namespace,
    iv = iv,
    ciphertext = ciphertext,
    authentication_tag = raw()
  )
  envelope$authentication_tag <- .ps_hmac(
    subkeys$authentication_key,
    .ps_envelope_payload(envelope)
  )
  envelope
}

.ps_validate_envelope <- function(envelope, namespace, method) {
  if (!is.list(envelope) || !identical(class(envelope), "list") ||
      !identical(names(envelope), .ps_envelope_fields) ||
      !identical(envelope$schema_version, "1") ||
      !identical(envelope$cipher, "aes-256-gcm+hmac-sha256") ||
      !identical(envelope$token_method, method) ||
      !identical(envelope$namespace, namespace) ||
      !is.raw(envelope$iv) || length(envelope$iv) != 12L ||
      !is.raw(envelope$ciphertext) ||
      !is.raw(envelope$authentication_tag) ||
      length(envelope$authentication_tag) != 32L) {
    .pc_abort("The protected identifier map is malformed.")
  }
  invisible(envelope)
}

.ps_decrypt_map <- function(envelope, namespace, method, subkeys) {
  .ps_validate_envelope(envelope, namespace, method)
  expected_tag <- .ps_hmac(
    subkeys$authentication_key,
    .ps_envelope_payload(envelope)
  )
  if (!.ps_constant_equal(expected_tag, envelope$authentication_tag)) {
    .pc_abort("The protected identifier map could not be authenticated.")
  }
  map <- tryCatch(
    {
      plaintext <- openssl::aes_gcm_decrypt(
        envelope$ciphertext,
        key = subkeys$encryption_key,
        iv = envelope$iv
      )
      unserialize(plaintext)
    },
    error = function(e) NULL
  )
  if (is.null(map) || !.ps_map_valid(map)) {
    .pc_abort("The protected identifier map could not be decrypted.")
  }
  .ps_canonical_map(map)
}

.ps_existing_map <- function(encrypted_map, namespace, method, subkeys) {
  if (is.null(encrypted_map)) {
    return(data.frame(
      identifier = character(),
      token = character(),
      stringsAsFactors = FALSE
    ))
  }
  if (inherits(encrypted_map, "pseudonymization")) {
    .ps_validate_object(encrypted_map)
    if (!identical(encrypted_map$namespace, namespace) ||
        !identical(encrypted_map$algorithm, method)) {
      .pc_abort("The existing protected map uses another namespace or method.")
    }
    encrypted_map <- encrypted_map$encrypted_map
  }
  .ps_decrypt_map(encrypted_map, namespace, method, subkeys)
}

.ps_validate_object <- function(x) {
  fields <- c(
    "values", "encrypted_map", "algorithm", "namespace",
    "key_fingerprint", "n_identifiers"
  )
  if (!inherits(x, "pseudonymization") || !is.list(x) ||
      !identical(names(x), fields) || !is.character(x$values) ||
      !is.character(x$algorithm) || length(x$algorithm) != 1L ||
      is.na(x$algorithm) ||
      !(x$algorithm %in% c("hmac_sha256", "random")) ||
      !is.character(x$namespace) || length(x$namespace) != 1L ||
      is.na(x$namespace) || !nzchar(x$namespace) ||
      grepl("[[:cntrl:]]", x$namespace) ||
      !.pc_is_hash(x$key_fingerprint) ||
      !is.integer(x$n_identifiers) || length(x$n_identifiers) != 1L ||
      is.na(x$n_identifiers) || x$n_identifiers < 0L ||
      !is.list(x$encrypted_map) ||
      any(!is.na(x$values) &
        !grepl("^psn_[0-9a-f]{32}$", x$values))) {
    .pc_abort("`x` is not a valid pseudonymization object.")
  }
  invisible(x)
}

#' Pseudonymize identifiers with a caller-owned key
#'
#' Creates deterministic HMAC-derived or random 128-bit tokens and stores the
#' one-to-one re-identification map in an authenticated encrypted envelope.
#' The encrypted map remains sensitive additional information and should be
#' stored separately from pseudonymized records with deployment-level access
#' controls.
#'
#' This is a pseudonymization primitive, not a declaration of anonymization,
#' HIPAA Safe Harbor status, or GDPR compliance.
#'
#' @param identifiers Character vector. `NA` values remain `NA`.
#' @param key Raw vector of at least 32 cryptographically random bytes.
#' @param namespace Non-empty deployment-specific namespace.
#' @param method Token method.
#' @param encrypted_map Optional prior `pseudonymization` object or protected
#'   map envelope used to continue a mapping.
#'
#' @return A `pseudonymization` object containing tokens and an authenticated
#'   encrypted map, but no plaintext identifier map or key.
#' @export
#'
#' @examples
#' key <- openssl::rand_bytes(32)
#' p <- pseudonymize(c("subject-a", "subject-b", "subject-a"),
#'                   key, namespace = "study-example")
#' p
#' identical(reidentify(p, key), c("subject-a", "subject-b", "subject-a"))
pseudonymize <- function(
    identifiers,
    key,
    namespace,
    method = c("hmac_sha256", "random"),
    encrypted_map = NULL) {
  identifiers <- .ps_identifiers(identifiers)
  key <- .ps_key(key)
  namespace <- .ps_utf8_scalar(namespace, "namespace")
  method <- match.arg(method)
  subkeys <- .ps_subkeys(key)
  map <- .ps_existing_map(encrypted_map, namespace, method, subkeys)

  unique_identifiers <- unique(identifiers[!is.na(identifiers)])
  known <- match(unique_identifiers, map$identifier)
  new_identifiers <- unique_identifiers[is.na(known)]
  if (length(new_identifiers)) {
    if (method == "hmac_sha256") {
      new_tokens <- vapply(
        new_identifiers,
        .ps_token,
        character(1),
        namespace = namespace,
        token_key = subkeys$token_key,
        USE.NAMES = FALSE
      )
    } else {
      new_tokens <- character(length(new_identifiers))
      occupied <- map$token
      for (i in seq_along(new_identifiers)) {
        repeat {
          candidate <- .ps_random_token()
          if (!candidate %in% c(occupied, new_tokens)) {
            new_tokens[[i]] <- candidate
            break
          }
        }
      }
    }
    map <- rbind(
      map,
      data.frame(
        identifier = new_identifiers,
        token = new_tokens,
        stringsAsFactors = FALSE
      )
    )
  }
  map <- .ps_canonical_map(map)

  if (method == "hmac_sha256" && nrow(map)) {
    expected <- vapply(
      map$identifier,
      .ps_token,
      character(1),
      namespace = namespace,
      token_key = subkeys$token_key,
      USE.NAMES = FALSE
    )
    if (!identical(expected, map$token)) {
      .pc_abort("The existing protected map conflicts with the keyed tokens.")
    }
  }
  if (anyDuplicated(map$token)) {
    .pc_abort("Distinct identifiers produced a duplicated pseudonym token.")
  }

  values <- rep(NA_character_, length(identifiers))
  keep <- !is.na(identifiers)
  if (any(keep)) {
    matched <- match(identifiers[keep], map$identifier)
    if (anyNA(matched)) {
      .pc_abort("The protected identifier map is incomplete.")
    }
    values[keep] <- map$token[matched]
  }
  names(values) <- NULL

  structure(
    list(
      values = values,
      encrypted_map = .ps_encrypt_map(map, namespace, method, subkeys),
      algorithm = method,
      namespace = namespace,
      key_fingerprint = .ps_hex(.ps_sha256(subkeys$map_key)),
      n_identifiers = as.integer(nrow(map))
    ),
    class = "pseudonymization"
  )
}

#' Recover identifiers from a protected pseudonymization map
#'
#' @param x A `pseudonymization` object.
#' @param key The caller-owned raw key used to create the map.
#'
#' @return Character vector aligned to `x$values`.
#' @export
reidentify <- function(x, key) {
  .ps_validate_object(x)
  key <- .ps_key(key)
  subkeys <- .ps_subkeys(key)
  fingerprint <- .ps_hex(.ps_sha256(subkeys$map_key))
  if (!.ps_constant_equal(
    charToRaw(fingerprint),
    charToRaw(x$key_fingerprint)
  )) {
    .pc_abort("The protected identifier map could not be authenticated.")
  }
  map <- .ps_decrypt_map(
    x$encrypted_map,
    namespace = x$namespace,
    method = x$algorithm,
    subkeys = subkeys
  )
  if (!identical(as.integer(nrow(map)), x$n_identifiers)) {
    .pc_abort("The protected identifier map is inconsistent.")
  }
  out <- rep(NA_character_, length(x$values))
  keep <- !is.na(x$values)
  if (any(keep)) {
    matched <- match(x$values[keep], map$token)
    if (anyNA(matched)) {
      .pc_abort("The protected identifier map is incomplete.")
    }
    out[keep] <- map$identifier[matched]
  }
  names(out) <- NULL
  out
}

.ds_counter_raw <- function(counter) {
  counter <- as.integer(counter)
  as.raw(c(
    bitwAnd(bitwShiftR(counter, 24L), 255L),
    bitwAnd(bitwShiftR(counter, 16L), 255L),
    bitwAnd(bitwShiftR(counter, 8L), 255L),
    bitwAnd(counter, 255L)
  ))
}

.ds_u48 <- function(value) {
  bytes <- as.integer(value[seq_len(6L)])
  sum(bytes * 256^(5:0))
}

.ds_offset <- function(subject, key, lower, upper) {
  width <- as.double(upper) - as.double(lower) + 1
  limit <- floor(2^48 / width) * width
  for (counter in 0:1000000) {
    value <- .ps_hmac(
      key,
      c(.ps_context("PhysioCompliance/date-offset/v1", subject),
        .ds_counter_raw(counter))
    )
    number <- .ds_u48(value)
    if (number < limit) {
      return(as.integer(lower + (number %% width)))
    }
  }
  .pc_abort("Unable to derive a date offset.")
}

#' Shift dates consistently within subject
#'
#' Derives one deterministic integer-day offset per subject using keyed
#' HMAC-SHA-256. Exact elapsed intervals are preserved. Full shifted dates are
#' pseudonymized personal data, not a HIPAA Safe Harbor output.
#'
#' @param dates A `Date` or `POSIXct` vector.
#' @param subject_id One subject ID or a vector aligned to `dates`.
#' @param key Raw vector of at least 32 cryptographically random bytes.
#' @param range_days Inclusive integer offset range.
#' @param inverse Apply the exact negative keyed offset.
#'
#' @return A vector with the same length and date class.
#' @export
dateShift <- function(
    dates,
    subject_id,
    key,
    range_days = c(-3650L, 3650L),
    inverse = FALSE) {
  if (!inherits(dates, "Date") && !inherits(dates, "POSIXct")) {
    .pc_abort("`dates` must inherit from Date or POSIXct.")
  }
  if (!is.logical(inverse) || length(inverse) != 1L || is.na(inverse)) {
    .pc_abort("`inverse` must be one non-missing logical value.")
  }
  if (!is.numeric(range_days) || length(range_days) != 2L ||
      anyNA(range_days) || any(!is.finite(range_days)) ||
      any(range_days != trunc(range_days)) ||
      any(abs(range_days) > .Machine$integer.max) ||
      range_days[[1L]] > range_days[[2L]] ||
      diff(range_days) + 1 > 1000000) {
    .pc_abort(
      "`range_days` must be two ordered finite integers spanning at most one million days."
    )
  }
  lower <- as.integer(range_days[[1L]])
  upper <- as.integer(range_days[[2L]])
  key <- .ps_key(key)
  date_key <- .ps_hmac(
    key,
    charToRaw("PhysioCompliance/date/v1")
  )
  subject_id <- .ps_identifiers(subject_id)
  if (length(subject_id) == 1L && length(dates) != 1L) {
    subject_id <- rep(subject_id, length(dates))
  }
  if (length(subject_id) != length(dates)) {
    .pc_abort("`subject_id` must be scalar or aligned to `dates`.")
  }
  values <- as.numeric(dates)
  active <- !is.na(values)
  if (any(active & !is.finite(values))) {
    .pc_abort("Non-missing dates must be finite.")
  }
  if (any(active & is.na(subject_id))) {
    .pc_abort("Every non-missing date requires a subject ID.")
  }
  offsets <- integer(length(dates))
  subjects <- unique(subject_id[active])
  for (subject in subjects) {
    offsets[active & subject_id == subject] <- .ds_offset(
      subject,
      date_key,
      lower,
      upper
    )
  }
  if (inverse) {
    offsets <- -offsets
  }
  multiplier <- if (inherits(dates, "POSIXct")) 86400 else 1
  shifted <- values
  shifted[active] <- values[active] + offsets[active] * multiplier
  if (any(active & !is.finite(shifted))) {
    .pc_abort("The requested date shift exceeds the representable range.")
  }

  old_names <- names(dates)
  if (inherits(dates, "POSIXct")) {
    tz <- attr(dates, "tzone", exact = TRUE)
    out <- as.POSIXct(
      shifted,
      origin = "1970-01-01",
      tz = if (is.null(tz) || !length(tz)) "UTC" else tz[[1L]]
    )
    class(out) <- class(dates)
    if (!is.null(tz)) {
      attr(out, "tzone") <- tz
    }
  } else {
    out <- structure(shifted, class = class(dates))
  }
  names(out) <- old_names
  out
}

#' @export
print.pseudonymization <- function(x, ...) {
  cat(sprintf(
    "<pseudonymization> method=%s; values=%d; identifiers=%d; protected-map=authenticated\n",
    x$algorithm,
    length(x$values),
    x$n_identifiers
  ))
  invisible(x)
}
