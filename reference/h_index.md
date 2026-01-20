# Compute h-index and hIa-index

TODO: Could use a time-window (currently, it uses all citations since
publication).

## Usage

``` r
h_index(
  search = NULL,
  display_name = NULL,
  author.id = NULL,
  ORCID = NULL,
  first_pub_year = NA,
  academic_age_bonus = 0
)
```

## Arguments

- search:

  A character string with the author's name for searching in the
  OpenAlex API. Allows fuzzy search (e.g. with or without middle
  initial).

- display_name:

  A character vector with the exact author's names for searching in the
  OpenAlex API. You cvan provide multiple variants in a vector.

- author.id:

  A character vector with OpenAlex author ID(s) (i.e., an ID starting
  with "A.......") for searching in the OpenAlex API.

- ORCID:

  A character vector with ORCID IDs (either as URI or as the number
  quartet string).

- first_pub_year:

  An integer with the year of the author's first publication. If not
  provided, it is estimated from the retrieved publications. This is
  used to compute the age-corrected h_Ia index.

- academic_age_bonus:

  Years that are subtracted from active academic life (e.g. due to child
  care). For example, SNF subtracts 1.5 years per child (see
  https://www.snf.ch/en/cciM9NWuvhOVKRxv/news/news-200803-career-funding-the-researchers-overall-performance-counts).

## Value

A list containing the net academic age, h-index and hIa-index of the
author.

## Details

Computes the h-index and hIa-index of an author. You need to provide
exactly *one* of the parameters `search`, `display_name`, `author.id`,
or `ORCID`:

- `search` allows fuzzy search of the author's name (e.g. with or
  without middle initial). Might return too many matches

- `display_name` requires an exact match but allows to provide a vector
  with multiple versions of a name (e.g.,
  `c("Hans Müller", "Hans Mueller", "Hans J. Muller")`)

- `author.id` is the OpenAlex ID for authors.

- `ORCID` is the ORCID (either as link or as the number quartet string)

Often the same author is listed under different IDs in the OpenAlex
database, therefore these are merged by the function. **But what if the
function includes wrong authors?** Then you need to manually screen the
table with author names, record the IDs from the correct aliases of the
target person (starting with "A.......") and enter these author IDs into
the `author.id` parameter.

## Examples

``` r
h_index(search = "Felix Schönbrodt")
#> A unique author record has been found: 
#> 
#> # A tibble: 1 × 3
#>   display_name        works_count cited_by_count
#>   <chr>                     <int>          <int>
#> 1 Felix D. Schönbrodt         280          14159
#> Computation of academic age (relative to first publication):
#> Retrieving year of first publication from data: 2006. 
#> 
#> The first publication is: 'Das Evolutionäre Motivprofil - Entwicklung eines Motivfragebogens zum "Zürcher Modell sozialer Motivation" und seine Validierung an der PRF-D, dem MMG sowie biographischen Angaben', published in 'PsyDok Dokumentenserver für die Psychologie (Leibniz-Zentrum für Psychologische Information und Dokumentation)'. It has the domain 'Social Sciences'. 
#> 
#> Is that plausible? If not, provide `first_pub_year` as a parameter. You can see all works of that author with the following command:
#> 
#> oa_fetch(entity = 'works', author.id = 'https://openalex.org/A5022479713', is_paratext = FALSE, is_retracted = FALSE, abstract=FALSE) %>% arrange(publication_year)
#> 
#> $net_academic_age
#> [1] 20
#> 
#> $h_index
#> [1] 39
#> 
#> $h_Ia
#> [1] 1.05
#> 
#> $first_publication
#> # A tibble: 1 × 45
#>   id      title display_name authorships doi   publication_date publication_year
#>   <chr>   <chr> <chr>        <list>      <chr> <date>                      <int>
#> 1 https:… "Das… "Das Evolut… <tibble>    http… 2006-01-01                   2006
#> # ℹ 38 more variables: fwci <dbl>, cited_by_count <int>, counts_by_year <list>,
#> #   ids <list>, type <chr>, is_oa <lgl>, is_oa_anywhere <lgl>, oa_status <chr>,
#> #   oa_url <chr>, any_repository_has_fulltext <lgl>, source_display_name <chr>,
#> #   source_id <chr>, issn_l <chr>, host_organization <chr>,
#> #   host_organization_name <chr>, landing_page_url <chr>, pdf_url <chr>,
#> #   license <chr>, version <chr>, referenced_works <list>,
#> #   referenced_works_count <int>, related_works <list>, concepts <list>, …
#> 
#> $academic_age_note
#> [1] "Computation of academic age (relative to first publication):\nRetrieving year of first publication from data: 2006. \n\nThe first publication is: 'Das Evolutionäre Motivprofil - Entwicklung eines Motivfragebogens zum \"Zürcher Modell sozialer Motivation\" und seine Validierung an der PRF-D, dem MMG sowie biographischen Angaben', published in 'PsyDok Dokumentenserver für die Psychologie (Leibniz-Zentrum für Psychologische Information und Dokumentation)'. It has the domain 'Social Sciences'. \n\nIs that plausible? If not, provide `first_pub_year` as a parameter. You can see all works of that author with the following command:\n\noa_fetch(entity = 'works', author.id = 'https://openalex.org/A5022479713', is_paratext = FALSE, is_retracted = FALSE, abstract=FALSE) %>% arrange(publication_year)\n\n"
#> 
h_index(ORCID = "https://orcid.org/0000-0002-8282-3910")
#> A unique author record has been found: 
#> 
#> # A tibble: 1 × 3
#>   display_name        works_count cited_by_count
#>   <chr>                     <int>          <int>
#> 1 Felix D. Schönbrodt         280          14159
#> Computation of academic age (relative to first publication):
#> Retrieving year of first publication from data: 2006. 
#> 
#> The first publication is: 'Das Evolutionäre Motivprofil - Entwicklung eines Motivfragebogens zum "Zürcher Modell sozialer Motivation" und seine Validierung an der PRF-D, dem MMG sowie biographischen Angaben', published in 'PsyDok Dokumentenserver für die Psychologie (Leibniz-Zentrum für Psychologische Information und Dokumentation)'. It has the domain 'Social Sciences'. 
#> 
#> Is that plausible? If not, provide `first_pub_year` as a parameter. You can see all works of that author with the following command:
#> 
#> oa_fetch(entity = 'works', author.id = 'https://openalex.org/A5022479713', is_paratext = FALSE, is_retracted = FALSE, abstract=FALSE) %>% arrange(publication_year)
#> 
#> $net_academic_age
#> [1] 20
#> 
#> $h_index
#> [1] 39
#> 
#> $h_Ia
#> [1] 1.05
#> 
#> $first_publication
#> # A tibble: 1 × 45
#>   id      title display_name authorships doi   publication_date publication_year
#>   <chr>   <chr> <chr>        <list>      <chr> <date>                      <int>
#> 1 https:… "Das… "Das Evolut… <tibble>    http… 2006-01-01                   2006
#> # ℹ 38 more variables: fwci <dbl>, cited_by_count <int>, counts_by_year <list>,
#> #   ids <list>, type <chr>, is_oa <lgl>, is_oa_anywhere <lgl>, oa_status <chr>,
#> #   oa_url <chr>, any_repository_has_fulltext <lgl>, source_display_name <chr>,
#> #   source_id <chr>, issn_l <chr>, host_organization <chr>,
#> #   host_organization_name <chr>, landing_page_url <chr>, pdf_url <chr>,
#> #   license <chr>, version <chr>, referenced_works <list>,
#> #   referenced_works_count <int>, related_works <list>, concepts <list>, …
#> 
#> $academic_age_note
#> [1] "Computation of academic age (relative to first publication):\nRetrieving year of first publication from data: 2006. \n\nThe first publication is: 'Das Evolutionäre Motivprofil - Entwicklung eines Motivfragebogens zum \"Zürcher Modell sozialer Motivation\" und seine Validierung an der PRF-D, dem MMG sowie biographischen Angaben', published in 'PsyDok Dokumentenserver für die Psychologie (Leibniz-Zentrum für Psychologische Information und Dokumentation)'. It has the domain 'Social Sciences'. \n\nIs that plausible? If not, provide `first_pub_year` as a parameter. You can see all works of that author with the following command:\n\noa_fetch(entity = 'works', author.id = 'https://openalex.org/A5022479713', is_paratext = FALSE, is_retracted = FALSE, abstract=FALSE) %>% arrange(publication_year)\n\n"
#> 
h_index("Markus Bühner", first_pub_year = 1997) # adjust year of first publication
#> A unique author record has been found: 
#> 
#> # A tibble: 1 × 3
#>   display_name  works_count cited_by_count
#>   <chr>               <int>          <int>
#> 1 Markus Bühner         237           8784
#> $net_academic_age
#> [1] 29
#> 
#> $h_index
#> [1] 43
#> 
#> $h_Ia
#> [1] 0.6551724
#> 
#> $first_publication
#> # A tibble: 1 × 45
#>   id      title display_name authorships doi   publication_date publication_year
#>   <chr>   <chr> <chr>        <list>      <chr> <date>                      <int>
#> 1 https:… Attr… Attribution… <tibble>    http… 1998-01-01                   1998
#> # ℹ 38 more variables: fwci <dbl>, cited_by_count <int>, counts_by_year <list>,
#> #   ids <list>, type <chr>, is_oa <lgl>, is_oa_anywhere <lgl>, oa_status <chr>,
#> #   oa_url <chr>, any_repository_has_fulltext <lgl>, source_display_name <chr>,
#> #   source_id <chr>, issn_l <chr>, host_organization <chr>,
#> #   host_organization_name <chr>, landing_page_url <chr>, pdf_url <chr>,
#> #   license <chr>, version <chr>, referenced_works <list>,
#> #   referenced_works_count <int>, related_works <list>, concepts <list>, …
#> 
#> $academic_age_note
#> [1] ""
#> 

```
