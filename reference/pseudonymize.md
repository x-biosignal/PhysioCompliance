# Pseudonymize identifiers with a caller-owned key

Creates deterministic HMAC-derived or random 128-bit tokens and stores
the one-to-one re-identification map in an authenticated encrypted
envelope. The encrypted map remains sensitive additional information and
should be stored separately from pseudonymized records with
deployment-level access controls.

## Usage

``` r
pseudonymize(
  identifiers,
  key,
  namespace,
  method = c("hmac_sha256", "random"),
  encrypted_map = NULL
)
```

## Arguments

- identifiers:

  Character vector. `NA` values remain `NA`.

- key:

  Raw vector of at least 32 cryptographically random bytes.

- namespace:

  Non-empty deployment-specific namespace.

- method:

  Token method.

- encrypted_map:

  Optional prior `pseudonymization` object or protected map envelope
  used to continue a mapping.

## Value

A `pseudonymization` object containing tokens and an authenticated
encrypted map, but no plaintext identifier map or key.

## Details

This is a pseudonymization primitive, not a declaration of
anonymization, HIPAA Safe Harbor status, or GDPR compliance.

## Examples

``` r
key <- openssl::rand_bytes(32)
p <- pseudonymize(c("subject-a", "subject-b", "subject-a"),
                  key, namespace = "study-example")
p
#> <pseudonymization> method=hmac_sha256; values=3; identifiers=2; protected-map=authenticated
identical(reidentify(p, key), c("subject-a", "subject-b", "subject-a"))
#> [1] TRUE
```
