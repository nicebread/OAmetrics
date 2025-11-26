# Get Number of Authors from an OpenAlex Object

Extracts the number of authors for each record in an OpenAlex-style list
object.

## Usage

``` r
get_n_authors(OA_object)
```

## Arguments

- OA_object:

  A list-like object (e.g., from OpenAlex API) containing an `author`
  element.

## Value

A numeric vector giving the number of authors for each entry. Entries
with no author information (`NULL`) are returned as `NA`.

## Details

The function applies `nrow` to each element of `OA_object$author`. If
any entries are `NULL`, they are converted to `NA` in the output.
