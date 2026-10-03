# Recover identifiers from a protected pseudonymization map

Recover identifiers from a protected pseudonymization map

## Usage

``` r
reidentify(x, key)
```

## Arguments

- x:

  A `pseudonymization` object.

- key:

  The caller-owned raw key used to create the map.

## Value

Character vector aligned to `x$values`.

## Examples

``` r
key <- openssl::rand_bytes(32)
p <- pseudonymize(c("subject-a", "subject-b"), key,
                  namespace = "study-example")
reidentify(p, key)
#> [1] "subject-a" "subject-b"
```
