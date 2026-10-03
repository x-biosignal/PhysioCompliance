# Verify stored electronic signatures

Reconstructs each canonical challenge and delegates cryptographic and
credential verification to a caller-supplied callback. Provider errors
are reported without storing or exposing provider diagnostics.

## Usage

``` r
verifyESignatures(x, verify)
```

## Arguments

- x:

  A
  [PhysioExperiment::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/PhysioExperiment.html)
  object.

- verify:

  Function called once for each structurally valid signature as
  `verify(challenge_raw, signature_raw, credential)`. It must return one
  non-missing logical value.

## Value

A `compliance_verification` object containing audit and signature
issues.

## Examples

``` r
x <- PhysioExperiment::PhysioExperiment(
  assays = list(raw = matrix(1:6, nrow = 3)),
  samplingRate = 100
)
x <- initializeAuditTrail(x, actor = "operator-01")
credential <- list(id = "credential-01", algorithm = "demo-sha256",
                   fingerprint = "01:23:45:67")
bytes <- function(challenge, credential) {
  digest::digest(c(challenge, serialize(credential, NULL, version = 3)),
                 algo = "sha256", serialize = FALSE, raw = TRUE)
}
x <- eSign(x, signer = "reviewer-02", meaning = "reviewed",
           credential = credential, sign = bytes)
# The verifier recomputes the same bytes; a real one checks a cryptographic
# signature against the signer's credential.
verifyESignatures(x, verify = function(challenge, signature, credential) {
  identical(signature, bytes(challenge, credential))
})$valid
#> [1] TRUE
```
