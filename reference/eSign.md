# Add an externally authenticated electronic signature

The callback is responsible for authenticating and authorizing the
signer. It is called exactly once with canonical challenge bytes and a
sanitized public credential descriptor. The package never accepts
passwords, private keys, API tokens, or reusable secrets.

## Usage

``` r
eSign(x, signer, meaning, credential, sign, timestamp = Sys.time())
```

## Arguments

- x:

  An initialized, currently valid
  [PhysioExperiment::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioExperiment/reference/PhysioExperiment.html).

- signer:

  Non-empty signer identity.

- meaning:

  Non-empty signature meaning.

- credential:

  Named list containing `id`, `algorithm`, `fingerprint`, and optional
  non-secret `public_data`.

- sign:

  Function called once as `sign(challenge_raw, credential)`. It must
  return a non-empty raw vector.

- timestamp:

  One finite `POSIXct` value that does not precede the audit head.

## Value

A modified copy of `x` with one signature and one cross-linked audit
event.

## Details

The callback's algorithm and credential service determine cryptographic
strength, identity assurance, and non-repudiation properties. This
function is a technical control and does not itself establish regulatory
compliance.

## Examples

``` r
x <- PhysioExperiment::PhysioExperiment(
  assays = list(raw = matrix(1:6, nrow = 3)),
  samplingRate = 100
)
x <- initializeAuditTrail(x, actor = "operator-01")
# Signing is delegated to a caller-supplied callback. This illustrative one
# hashes the challenge; a real deployment authenticates the signer and uses
# its own credential service. The package stores no secrets.
credential <- list(id = "credential-01", algorithm = "demo-sha256",
                   fingerprint = "01:23:45:67")
sign <- function(challenge, credential) {
  digest::digest(c(challenge, serialize(credential, NULL, version = 3)),
                 algo = "sha256", serialize = FALSE, raw = TRUE)
}
x <- eSign(x, signer = "reviewer-02", meaning = "reviewed",
           credential = credential, sign = sign)
verifyAuditTrail(x)$n_signatures
#> [1] 1
```
