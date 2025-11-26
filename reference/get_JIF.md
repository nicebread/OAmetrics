# Compute the Journal Impact Factor

This function computes the Journal Impact Factor (JIF) for a given
journal given its ISSN and a target year.

## Usage

``` r
get_JIF(issn, year, limit = NA, verbose = FALSE, seed = NA)
```

## Arguments

- issn:

  The ISSN of the journal

- year:

  The target year

- limit:

  Upper limit of papers to be downloaded (random selection). Some
  mega-journals, such as Scientific Reports, have \> 45,000 papers in
  the two-year time window, which takes quite some time to download.
  Typical journals in psychology have only hundreds of papers. Limiting
  the papers can give you a slight underestimation, as the JIF is also
  driven by rare outliers (with huge citation scores), which are only
  covered when all papers are considered. A limit of 5000 to 10000
  should give you pretty stable estimates.

- verbose:

  Whether to print verbose output (default is FALSE)

- seed:

  Seed for a random retrieval of papers. If NA (the default), a random
  seed is chosen.

## Value

A data frame with columns for journal, ISSN, year, total_citations,
citable_items and JIF.

## Examples

``` r
get_JIF(issn="0022-3514", year=2018)  # JPSP
#>                                        journal      issn year paper_limit
#> 1 Journal of Personality and Social Psychology 0022-3514 2018          NA
#>   total_citations citable_items      JIF
#> 1            1804           214 8.429907
get_JIF(issn="0890-2070", year=2019)  # EJP
#>                           journal      issn year paper_limit total_citations
#> 1 European Journal of Personality 0890-2070 2019          NA             451
#>   citable_items      JIF
#> 1            85 5.305882
# Scientific Reports; according to website the JIF is 3.8 in 2023
get_JIF(issn="2045-2322", year=2023, limit=5000)
#> Warning: The journal published 47176 works in the two-year time window. Limiting to n=5000 random papers.
#>              journal      issn year paper_limit total_citations citable_items
#> 1 Scientific Reports 2045-2322 2023        5000           25229          5000
#>      JIF
#> 1 5.0458
```
