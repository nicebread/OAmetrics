# Evenness Index function

This function calculates Pielou's evenness index (Pielou, 1966).

## Usage

``` r
evenness_index(counts, max_cat)
```

## Arguments

- counts:

  A numeric vector of counts representing the number of observations in
  each category.

- max_cat:

  Maximum number of categories. If less than the number of provided
  categories, the remaining categories are ignored. If more than the
  number of provided categories, the missing categories are assumed with
  count=0.

## Value

The Evenness Index, which combines entropy and richness providing a more
holistic measure. `max_cat` can be a number larger than the number of
provided category counts. `max_cat` is used as a normalizing factor: It
assumes as many existing categories, and missing categories are added
and assumed with count=0.

## Examples

``` r
counts <- c(10, 20, 30, 40)
max_cat <- 6
evenness_index(counts, max_cat)
#> [1] 0.7143002
```
