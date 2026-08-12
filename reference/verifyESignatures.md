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
  [PhysioCore::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioCore/reference/PhysioExperiment.html)
  object.

- verify:

  Function called once for each structurally valid signature as
  `verify(challenge_raw, signature_raw, credential)`. It must return one
  non-missing logical value.

## Value

A `compliance_verification` object containing audit and signature
issues.
