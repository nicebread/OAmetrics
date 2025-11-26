# Normalize DOIs

This function normalizes DOIs by trimming leading/trailing whitespace,
replacing 'dx.doi.org' with 'doi.org', replacing 'http:' with 'https:',
and converting the DOI to lowercase, according to the DOI specification

## Usage

``` r
normalize_dois(x, verbose = FALSE)
```

## Arguments

- x:

  Character vector of DOIs to be normalized

- verbose:

  Show diagnostic information?

## Value

Character vector of normalized DOIs

## Examples

``` r
normalize_dois(c("  10.123.10.1/DOI ", "http://dx.doi.org/10.456/Doi"))
#> [1] "https://doi.org/10.123.10.1/doi" "https://doi.org/10.456/doi"     
```
