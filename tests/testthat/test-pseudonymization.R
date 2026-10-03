test_that("HMAC pseudonyms are deterministic, namespaced, and reversible", {
  identifiers <- c(sprintf("subject-%04d", 1:1000), "subject-0001", NA)
  key <- as.raw(0:31)
  other_key <- as.raw(32:63)

  first <- pseudonymize(identifiers, key, "deployment-a")
  repeated <- pseudonymize(identifiers, key, "deployment-a")
  another_namespace <- pseudonymize(identifiers, key, "deployment-b")
  another_key <- pseudonymize(identifiers, other_key, "deployment-a")

  expect_s3_class(first, "pseudonymization")
  expect_identical(first$values, repeated$values)
  expect_false(identical(first$values, another_namespace$values))
  expect_false(identical(first$values, another_key$values))
  expect_identical(first$values[[1L]], first$values[[1001L]])
  expect_true(is.na(first$values[[1002L]]))
  expect_equal(length(unique(stats::na.omit(first$values))), 1000L)
  expect_match(first$values[[1L]], "^psn_[0-9a-f]{32}$")
  expect_identical(reidentify(first, key), identifiers)
  expect_error(reidentify(first, other_key), "authenticated")

  serialized <- serialize(first, NULL, version = 3L, xdr = TRUE)
  for (identifier in identifiers[1:10]) {
    expect_false(raw_contains(serialized, charToRaw(identifier)))
  }
  printed <- paste(capture.output(print(first)), collapse = "\n")
  expect_match(printed, "identifiers=1000", fixed = TRUE)
  expect_false(grepl("subject-0001", printed, fixed = TRUE))
})

test_that("random tokens continue only through the protected map", {
  key <- openssl::rand_bytes(32)
  first <- pseudonymize(c("alpha", "beta"), key, "random-map", "random")
  continued <- pseudonymize(
    c("beta", "gamma", NA),
    key,
    "random-map",
    "random",
    encrypted_map = first
  )
  independent <- pseudonymize(
    c("beta", "gamma"),
    key,
    "random-map",
    "random"
  )

  expect_identical(continued$values[[1L]], first$values[[2L]])
  expect_false(continued$values[[2L]] %in% first$values)
  expect_false(independent$values[[1L]] == first$values[[2L]])
  expect_identical(
    reidentify(continued, key),
    c("beta", "gamma", NA_character_)
  )
  expect_error(
    pseudonymize(
      "alpha", key, "another-map", "random", encrypted_map = first
    ),
    "another namespace"
  )
})

test_that("protected map authentication rejects every envelope mutation", {
  key <- openssl::rand_bytes(32)
  original <- pseudonymize(c("private-one", "private-two"), key, "mutation")
  fields <- c("iv", "ciphertext", "authentication_tag")

  for (field in fields) {
    for (i in seq_len(min(20L, length(original$encrypted_map[[field]])))) {
      changed <- original
      changed$encrypted_map[[field]][[i]] <- as.raw(
        bitwXor(as.integer(changed$encrypted_map[[field]][[i]]), 1L)
      )
      message <- tryCatch(
        {
          reidentify(changed, key)
          NA_character_
        },
        error = conditionMessage
      )
      expect_false(is.na(message))
      expect_false(grepl("private-one|private-two", message))
    }
  }

  replay <- original
  replay$namespace <- "another"
  replay$encrypted_map$namespace <- "another"
  expect_error(reidentify(replay, key), "authenticated")

  missing <- original
  missing$values[[1L]] <- "psn_00000000000000000000000000000000"
  expect_error(reidentify(missing, key), "incomplete")
})

test_that("pseudonymization rejects weak keys and malformed identifiers", {
  expect_error(pseudonymize("a", "password", "n"), "raw vector")
  expect_error(pseudonymize("a", raw(31), "n"), "at least 32")
  expect_error(pseudonymize(factor("a"), raw(32), "n"), "character vector")
  expect_error(pseudonymize("", raw(32), "n"), "non-empty")
  expect_error(pseudonymize("a", raw(32), "bad\nnamespace"), "control")
  expect_error(reidentify(list(), raw(32)), "valid pseudonymization")
  malformed <- pseudonymize("a", raw(32), "n")
  malformed$algorithm <- c("hmac_sha256", "random")
  expect_error(reidentify(malformed, raw(32)), "valid pseudonymization")
})

test_that("date shifting preserves exact within-subject intervals", {
  subjects <- sprintf("subject-%03d", 1:100)
  counts <- rep(2:20, length.out = length(subjects))
  subject_id <- rep(subjects, counts)
  base <- as.Date("2020-02-20")
  dates <- as.Date(unlist(lapply(counts, function(n) 0:(n - 1L))),
                   origin = base)
  key <- as.raw(0:31)
  other_key <- as.raw(32:63)

  shifted <- dateShift(dates, subject_id, key)
  reproduced <- dateShift(dates, subject_id, key)
  restored <- dateShift(shifted, subject_id, key, inverse = TRUE)
  shifted_other <- dateShift(dates, subject_id, other_key)

  expect_identical(shifted, reproduced)
  expect_identical(restored, dates)
  offsets <- split(as.numeric(shifted - dates), subject_id)
  expect_true(all(vapply(offsets, function(x) length(unique(x)) == 1L,
                         logical(1))))
  expect_true(all(vapply(
    split(seq_along(dates), subject_id),
    function(index) identical(diff(shifted[index]), diff(dates[index])),
    logical(1)
  )))
  other_offsets <- split(as.numeric(shifted_other - dates), subject_id)
  same <- vapply(seq_along(offsets), function(i) {
    offsets[[i]][[1L]] == other_offsets[[i]][[1L]]
  }, logical(1))
  expect_lt(mean(same), 0.01)
})

test_that("POSIXct shifting preserves timezone, duplicates, NA, and seconds", {
  dates <- as.POSIXct(
    c(
      "2024-03-09 12:00:00", "2024-03-10 12:00:00",
      "2024-11-03 12:00:00", "2024-11-03 12:00:00", NA
    ),
    tz = "America/New_York"
  )
  key <- as.raw(1:32)
  shifted <- dateShift(dates, "subject-a", key)

  expect_s3_class(shifted, "POSIXct")
  expect_identical(attr(shifted, "tzone"), attr(dates, "tzone"))
  expect_identical(diff(as.numeric(shifted[1:4])),
                   diff(as.numeric(dates[1:4])))
  expect_identical(shifted[[3L]], shifted[[4L]])
  expect_true(is.na(shifted[[5L]]))
  expect_identical(
    dateShift(shifted, "subject-a", key, inverse = TRUE),
    dates
  )
})

test_that("date shifting validates types, ranges, alignment, and overflow", {
  key <- raw(32)
  expect_error(dateShift("2020-01-01", "s", key), "Date or POSIXct")
  expect_error(dateShift(as.Date("2020-01-01"), "s", raw(31)), "at least 32")
  expect_error(
    dateShift(as.Date(1:2, origin = "1970-01-01"), c("s"), key,
              range_days = c(0, 1.5)),
    "finite integers"
  )
  expect_error(
    dateShift(as.Date(1:2, origin = "1970-01-01"), c("s1", "s2", "s3"), key),
    "scalar or aligned"
  )
  expect_error(
    dateShift(as.Date(c(1, 2), origin = "1970-01-01"), c("s", NA), key),
    "requires a subject"
  )
  expect_error(
    dateShift(as.Date("2020-01-01"), "s", key, inverse = NA),
    "one non-missing logical"
  )
  expect_error(
    dateShift(
      as.Date("2020-01-01"), "s", key,
      range_days = c(3e9, 3e9)
    ),
    "finite integers"
  )
})
