# ecdf2

Calculates an linearly interpolated empirical cumulative distribution
function (ecdf) from a vector of values. This function was provided by
Tal Galili on [StackOverflow](https://stats.stackexchange.com/q/230458).

## Usage

``` r
ecdf2(x)
```

## Arguments

- x:

  A vector of values.

## Value

A function that calculates the linearly interpolated ecdf of the vector
of values.

## Examples

``` r
x <- c(0,0,0,0,0,0,1,1,1,1,2,2,2,5,7,11,20,100)
ecdf2(x)
#> Empirical CDF 
#> Call: ecdf2(x)
#>  x[1:8] =      0,      1,      2,  ...,     20,    100
```
