test_that("EDF headers remove identifiers and preserve signal structure", {
  header <- list(
    patient_id = "source-patient",
    recording_id = "source-recording",
    start_date = as.Date("2024-02-29"),
    original_path = "/private/source-patient.edf",
    comments = "free text",
    signal_headers = list(
      label = c("ECG", "EMG"),
      transducer = c("AgCl", "AgCl"),
      unit = c("mV", "mV")
    )
  )
  result <- headerScrub(
    header,
    "edf",
    subject_token = "psn_safe",
    free_text = "drop"
  )

  expect_s3_class(result, "header_scrub")
  expect_identical(result$header$patient_id, "psn_safe")
  expect_identical(result$header$recording_id, "recording_psn_safe")
  expect_null(result$header$start_date)
  expect_null(result$header$original_path)
  expect_null(result$header$comments)
  expect_identical(
    result$header$signal_headers,
    header$signal_headers
  )
  expect_gt(nrow(result$manual_review), 0L)
  expect_false(any(c(
    "source-patient", "source-recording", "/private/source-patient.edf"
  ) %in% unlist(result$report, use.names = FALSE)))
  printed <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(printed, "format=edf", fixed = TRUE)
  expect_false(grepl("source-patient", printed, fixed = TRUE))
})

test_that("BrainVision linkage uses safe consistent basenames", {
  header <- list(
    SubjectID = "source",
    Experimenter = "operator name",
    DataFile = "/private/source.eeg",
    MarkerFile = "../private/source.vmrk",
    Comments = "free",
    CommonInfos = list(NumberOfChannels = 2L)
  )
  result <- headerScrub(
    header,
    "brainvision",
    subject_token = "psn_safe",
    free_text = "drop"
  )

  expect_identical(result$header$SubjectID, "psn_safe")
  expect_null(result$header$Experimenter)
  expect_identical(result$header$DataFile, "recording.eeg")
  expect_identical(result$header$MarkerFile, "recording.vmrk")
  expect_null(result$header$Comments)
  expect_identical(result$header$CommonInfos$NumberOfChannels, 2L)
})

test_that("SNIRF metadata tags are scrubbed without changing topology", {
  header <- list(
    metaDataTags = list(
      SubjectID = "source",
      MeasurementDate = as.Date("2024-01-15"),
      OperatorName = "operator",
      Description = "session note"
    ),
    probe = list(
      sourcePos3D = matrix(1:6, nrow = 2),
      detectorPos3D = matrix(7:12, nrow = 2)
    ),
    measurementList = list(
      list(sourceIndex = 1L, detectorIndex = 1L),
      list(sourceIndex = 2L, detectorIndex = 2L)
    )
  )
  key <- as.raw(1:32)
  result <- headerScrub(
    header,
    "snirf",
    subject_token = "psn_safe",
    date_key = key,
    subject_id = "source",
    free_text = "drop"
  )

  expect_identical(result$header$metaDataTags$SubjectID, "psn_safe")
  expect_false(identical(
    result$header$metaDataTags$MeasurementDate,
    header$metaDataTags$MeasurementDate
  ))
  expect_null(result$header$metaDataTags$OperatorName)
  expect_null(result$header$metaDataTags$Description)
  expect_identical(result$header$probe, header$probe)
  expect_identical(result$header$measurementList, header$measurementList)
})

test_that("DICOM-adjacent UIDs are referentially consistent and review is explicit", {
  header <- list(
    PatientName = "source name",
    PatientID = "source-id",
    StudyInstanceUID = c("1.2.3", "1.2.4"),
    ReferencedStudyInstanceUID = c("1.2.3", "1.2.3"),
    PrivateCreator = "vendor-private",
    PixelData = as.raw(1:4),
    OverlayData = as.raw(5:8),
    BurnedInAnnotation = "YES"
  )
  result <- headerScrub(
    header,
    "dicom",
    subject_token = "psn_safe",
    free_text = "drop"
  )

  expect_identical(result$header$PatientName, "psn_safe")
  expect_identical(result$header$PatientID, "psn_safe")
  expect_match(result$header$StudyInstanceUID[[1L]], "^2\\.25\\.[0-9]+$")
  expect_identical(
    result$header$StudyInstanceUID[[1L]],
    result$header$ReferencedStudyInstanceUID[[1L]]
  )
  expect_identical(
    result$header$ReferencedStudyInstanceUID[[1L]],
    result$header$ReferencedStudyInstanceUID[[2L]]
  )
  expect_false(identical(
    result$header$StudyInstanceUID[[1L]],
    result$header$StudyInstanceUID[[2L]]
  ))
  expect_identical(result$header$PatientIdentityRemoved, "YES")
  expect_identical(result$header$PrivateCreator, "vendor-private")
  expect_identical(result$header$PixelData, header$PixelData)
  expect_true(all(c(
    "header$PrivateCreator", "header$PixelData",
    "header$OverlayData", "header$BurnedInAnnotation", "header"
  ) %in% result$manual_review$path))
  expect_false(any(grepl(
    "source name|source-id|1.2.3",
    apply(result$report, 1L, paste, collapse = " ")
  )))
})

test_that("header format detection and risky inputs fail closed", {
  explicit <- structure(
    list(patient_id = "source"),
    format = "edf"
  )
  result <- headerScrub(
    explicit,
    "auto",
    subject_token = "psn_safe",
    free_text = "drop"
  )
  expect_identical(result$format, "edf")

  expect_error(
    headerScrub(list(patient_id = "source"), "auto"),
    "exactly one explicit"
  )
  classed <- structure(
    list(patient_id = "source"),
    class = c("edf_header", "list")
  )
  expect_identical(
    headerScrub(
      classed, "auto", subject_token = "psn_safe", free_text = "drop"
    )$format,
    "edf"
  )
  expect_error(
    headerScrub(
      list(comments = "source"),
      "edf",
      free_text = "error"
    ),
    "free-text"
  )
  expect_error(
    headerScrub(
      list(start_date = as.Date("2024-01-01")),
      "edf",
      date_key = raw(32),
      free_text = "drop"
    ),
    "supplied together"
  )
  expect_error(
    headerScrub(
      list(patient_id = "source"),
      "edf",
      subject_token = "../unsafe",
      free_text = "drop"
    ),
    "safe identifier"
  )
  expect_error(
    headerScrub(
      list(unsafe = function() NULL),
      "edf",
      free_text = "drop"
    ),
    "unsupported object graph"
  )
  expect_error(
    headerScrub(
      list(SeriesDescription = "source"),
      "dicom",
      free_text = "error"
    ),
    "free-text"
  )
  expect_error(
    headerScrub(
      list(StudyInstanceUID = "not-a-dicom-uid"),
      "dicom",
      free_text = "drop"
    ),
    "malformed UID"
  )

  dicom_date <- headerScrub(
    list(
      PatientBirthDate = "20240229",
      `00190010` = "private creator"
    ),
    "dicom",
    date_key = raw(32),
    subject_id = "source",
    free_text = "drop"
  )
  expect_match(dicom_date$header$PatientBirthDate, "^[0-9]{8}$")
  expect_true("header$00190010" %in% dicom_date$manual_review$path)
})
