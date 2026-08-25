make_compliance_experiment <- function(provenance = list()) {
  x <- PhysioCore::PhysioExperiment(
    assays = list(
      raw = matrix(as.double(1:24), nrow = 8, dimnames = list(NULL, c("A", "B", "C"))),
      filtered = matrix(as.double(24:1), nrow = 8)
    ),
    colData = S4Vectors::DataFrame(
      channel = c("A", "B", "C"),
      quality = c("good", "review", "good")
    ),
    metadata = list(
      events = data.frame(
        onset = c(0.5, 2.0),
        label = c("start", "stop"),
        stringsAsFactors = FALSE
      )
    ),
    samplingRate = 100,
    provenance = provenance
  )
  x
}

fixed_time <- function(offset = 0) {
  as.POSIXct("2026-01-02 03:04:05", tz = "UTC") + offset
}

test_credential <- function(public_data = list(provider = "test")) {
  list(
    id = "credential-01",
    algorithm = "test-sha256",
    fingerprint = "01:23:45:67",
    public_data = public_data
  )
}

test_signature_bytes <- function(challenge, credential) {
  digest::digest(
    c(challenge, serialize(credential, NULL, version = 3, xdr = TRUE)),
    algo = "sha256",
    serialize = FALSE,
    raw = TRUE
  )
}

test_signer <- function(challenge, credential) {
  test_signature_bytes(challenge, credential)
}

test_verifier <- function(challenge, signature, credential) {
  identical(signature, test_signature_bytes(challenge, credential))
}

compliance_state <- function(x) {
  S4Vectors::metadata(x)[["physio_compliance"]]
}

set_compliance_state <- function(x, state) {
  metadata <- S4Vectors::metadata(x)
  metadata[["physio_compliance"]] <- state
  S4Vectors::metadata(x) <- metadata
  x
}
