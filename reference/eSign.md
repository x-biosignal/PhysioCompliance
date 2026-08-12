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
  [PhysioCore::PhysioExperiment](https://x-biosignal.r-universe.dev/PhysioCore/reference/PhysioExperiment.html).

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
