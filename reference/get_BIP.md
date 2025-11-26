# Retrieve BIP scores for a list of DOIs

This function takes a list of DOIs, normalizes them, queries the BIP API
to retrieve scores, and returns a data frame with bibliometric
indicators.

## Usage

``` r
get_BIP(dois, verbose = FALSE)
```

## Arguments

- dois:

  A character vector of DOIs

- verbose:

  Show diagnostic information?

## Value

A data frame with bibliometric indicators for the provided DOIs

## Examples

``` r
dois <- c("https://doi.org/10.1080/00223891.2020.1726936", "10.1016/j.jrp.2013.05.009")
get_BIP(dois)
#>                                             doi pop_class inf_class imp_class
#> 1 https://doi.org/10.1080/00223891.2020.1726936        C4        C5        C4
#> 2     https://doi.org/10.1016/j.jrp.2013.05.009        C1        C2        C3
#>   cc_class      attrank     pagerank three_year_cc   cc msg
#> 1       C4 1.617936e-08 3.511684e-09            19   23  NA
#> 2       C2 9.022099e-07 6.900538e-08            90 1622  NA
```
