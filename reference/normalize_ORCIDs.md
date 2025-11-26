# Normalize ORCIDs

This function normalizes ORCIDs to the canonical form (which is the URL
to the profile)

## Usage

``` r
normalize_ORCIDs(x)
```

## Arguments

- x:

  Character vector of ORCIDs to be normalized

## Value

Character vector of normalized ORCIDS

## Examples

``` r
normalize_ORCIDs(c("orcid.org/0000-1234-5678-9111",
    "0000-1234-5678-9111", "  https://orcid.org/0000-1234-5678-9111 "))
#> [1] "https://orcid.org/0000-1234-5678-9111"
#> [2] "https://orcid.org/0000-1234-5678-9111"
#> [3] "https://orcid.org/0000-1234-5678-9111"
```
