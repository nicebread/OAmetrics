# Calculates the upper trimmed mean of a vector

Calculates the upper trimmed mean of a vector

## Usage

``` r
upper_trim_mean(x, trim)
```

## Arguments

- x:

  A numeric vector

- trim:

  A numeric value between 0 and 1 indicating the proportion of values to
  trim from the top

## Value

A numeric value representing the upper trimmed mean of the vector

## Examples

``` r
x <- c(1,2,3,4,5,6,7)
upper_trim_mean(x, 0.2)
#> [1] 3
```
