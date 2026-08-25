raw_contains <- function(haystack, needle) {
  haystack <- as.raw(haystack)
  needle <- as.raw(needle)
  if (!length(needle) || length(needle) > length(haystack)) {
    return(FALSE)
  }
  any(vapply(
    seq_len(length(haystack) - length(needle) + 1L),
    function(i) identical(
      haystack[i:(i + length(needle) - 1L)],
      needle
    ),
    logical(1)
  ))
}

make_deidentification_experiment <- function() {
  events <- PhysioCore::PhysioEvents(
    onset = c(1, 2, 3),
    type = c("stimulus", "response", "stimulus"),
    value = c("participant arrived", "button", "complete")
  )
  PhysioCore::PhysioExperiment(
    assays = list(
      raw = matrix(as.double(1:9), nrow = 3),
      voiceprint = matrix(as.double(10:18), nrow = 3)
    ),
    rowData = S4Vectors::DataFrame(
      device_serial = c("d1", "d2", "d3"),
      url = c("https://one", "https://two", "https://three"),
      ip_address = c("192.0.2.1", "192.0.2.2", "192.0.2.3")
    ),
    colData = S4Vectors::DataFrame(
      patient_name = c("A", "B", "C"),
      age = c(89, 90, 97),
      telephone = c("1", "2", "3"),
      fax = c("4", "5", "6"),
      email = c("a@x", "b@x", "c@x"),
      ssn = c("s1", "s2", "s3"),
      medical_record_number = c("m1", "m2", "m3")
    ),
    metadata = list(
      city = "Example City",
      birth_date = as.Date("2000-02-29"),
      health_plan_id = "h1",
      account_number = "a1",
      license_number = "l1",
      vehicle_id = "v1",
      fingerprint = "finger",
      photo = "photo-marker",
      patient_id = "participant-raw",
      comments = "unstructured note",
      events = events
    ),
    samplingRate = 100
  )
}
