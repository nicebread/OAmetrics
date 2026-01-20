# Get a reference set for field- and age-normalized citation rates

This function downloads a large set of publications from a certain field
and certain years. Retrieving 10,000 documents from the API takes about
1-2 min. Ideally, this should be cached locally. OpenAlex will only
return random samples for up to 10,000 records. If you request multiples
random samples, you will get duplicates. Therefore we stratify the
request by month (to get non-overlapping samples). So we can download
10,000 \* 12 unique entries. When more than 120,000 samples are
requested, some duplicates can happen, but should be rare.

## Usage

``` r
get_reference_set(
  years,
  n_per_year = 10000,
  concept.id = "C15744967",
  type = "article",
  seed = 42,
  verbose = TRUE,
  per_page = 200,
  save_intermediate = NULL,
  max_retries = 3,
  retry_delay = 5
)
```

## Arguments

- years:

  The year(s) from which a sample of the reference field should be
  retrieved.

- n_per_year:

  The number of documents to retrieve per requested year (optional,
  defaults to 10000). Values larger than 10000 are possible (they are
  split up to multiple OA requests).

- concept.id:

  A vector of `concept.id`s to search for (optional, defaults to
  "C15744967", i.e. "Psychology")

- type:

  Reference sets should refer to the same type of publication; defaults
  to "article"

- seed:

  Set a seed for reproducible analyses. However, as the underlying OA
  database changes frequently, the results will still not be very stable
  ...

- verbose:

  Show OA API progress?

- per_page:

  How many records should be requested per page? (200 is the maximum)

- save_intermediate:

  If a path is provided here, the intermediate downloaded files are
  saved at that path.

- max_retries:

  How many times to retry if the OpenAlex API returns an error

- retry_delay:

  Base delay (in seconds) between retries

## Value

A data frame containing the document id, year, cited_by_count, and
number of authors of the retrieved documents

## Examples

``` r
# Get reference set for "Psychology" for multiple years (small n here for demo)
psych_ref <- get_reference_set(
  years = 2018:2020, n_per_year = 120,
  concept.id = "C15744967"
 )
#> Year 2018: sampling 10 per month
#>   Month 01: 2018-01-01 --> 2018-01-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-01-01%2Cto_publication_date%3A2018-01-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201843&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 02: 2018-02-01 --> 2018-02-28
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-02-01%2Cto_publication_date%3A2018-02-28%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201844&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 03: 2018-03-01 --> 2018-03-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-03-01%2Cto_publication_date%3A2018-03-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201845&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 04: 2018-04-01 --> 2018-04-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-04-01%2Cto_publication_date%3A2018-04-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201846&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 05: 2018-05-01 --> 2018-05-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-05-01%2Cto_publication_date%3A2018-05-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201847&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 06: 2018-06-01 --> 2018-06-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-06-01%2Cto_publication_date%3A2018-06-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201848&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 07: 2018-07-01 --> 2018-07-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-07-01%2Cto_publication_date%3A2018-07-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201849&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 08: 2018-08-01 --> 2018-08-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-08-01%2Cto_publication_date%3A2018-08-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201850&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 09: 2018-09-01 --> 2018-09-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-09-01%2Cto_publication_date%3A2018-09-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201851&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 10: 2018-10-01 --> 2018-10-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-10-01%2Cto_publication_date%3A2018-10-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201852&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 11: 2018-11-01 --> 2018-11-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-11-01%2Cto_publication_date%3A2018-11-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201853&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 12: 2018-12-01 --> 2018-12-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2018%2Cfrom_publication_date%3A2018-12-01%2Cto_publication_date%3A2018-12-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201854&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#> Year 2018 completed in 0.2 min
#> Year 2019: sampling 10 per month
#>   Month 01: 2019-01-01 --> 2019-01-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-01-01%2Cto_publication_date%3A2019-01-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201943&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 02: 2019-02-01 --> 2019-02-28
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-02-01%2Cto_publication_date%3A2019-02-28%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201944&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 03: 2019-03-01 --> 2019-03-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-03-01%2Cto_publication_date%3A2019-03-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201945&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 04: 2019-04-01 --> 2019-04-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-04-01%2Cto_publication_date%3A2019-04-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201946&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 05: 2019-05-01 --> 2019-05-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-05-01%2Cto_publication_date%3A2019-05-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201947&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 06: 2019-06-01 --> 2019-06-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-06-01%2Cto_publication_date%3A2019-06-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201948&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 07: 2019-07-01 --> 2019-07-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-07-01%2Cto_publication_date%3A2019-07-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201949&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 08: 2019-08-01 --> 2019-08-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-08-01%2Cto_publication_date%3A2019-08-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201950&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 09: 2019-09-01 --> 2019-09-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-09-01%2Cto_publication_date%3A2019-09-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201951&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 10: 2019-10-01 --> 2019-10-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-10-01%2Cto_publication_date%3A2019-10-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201952&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 11: 2019-11-01 --> 2019-11-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-11-01%2Cto_publication_date%3A2019-11-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201953&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 12: 2019-12-01 --> 2019-12-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2019%2Cfrom_publication_date%3A2019-12-01%2Cto_publication_date%3A2019-12-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=201954&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#> Year 2019 completed in 0.2 min
#> Year 2020: sampling 10 per month
#>   Month 01: 2020-01-01 --> 2020-01-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-01-01%2Cto_publication_date%3A2020-01-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202043&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 02: 2020-02-01 --> 2020-02-29
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-02-01%2Cto_publication_date%3A2020-02-29%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202044&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 03: 2020-03-01 --> 2020-03-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-03-01%2Cto_publication_date%3A2020-03-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202045&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 04: 2020-04-01 --> 2020-04-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-04-01%2Cto_publication_date%3A2020-04-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202046&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 05: 2020-05-01 --> 2020-05-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-05-01%2Cto_publication_date%3A2020-05-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202047&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 06: 2020-06-01 --> 2020-06-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-06-01%2Cto_publication_date%3A2020-06-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202048&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 07: 2020-07-01 --> 2020-07-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-07-01%2Cto_publication_date%3A2020-07-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202049&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 08: 2020-08-01 --> 2020-08-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-08-01%2Cto_publication_date%3A2020-08-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202050&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 09: 2020-09-01 --> 2020-09-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-09-01%2Cto_publication_date%3A2020-09-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202051&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 10: 2020-10-01 --> 2020-10-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-10-01%2Cto_publication_date%3A2020-10-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202052&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 11: 2020-11-01 --> 2020-11-30
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-11-01%2Cto_publication_date%3A2020-11-30%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202053&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#>   Month 12: 2020-12-01 --> 2020-12-31
#> -> Fetch attempt 1
#> Requesting url:
#> <https://api.openalex.org/works?filter=publication_year%3A2020%2Cfrom_publication_date%3A2020-12-01%2Cto_publication_date%3A2020-12-31%2Cconcept.id%3AC15744967%2Ctype%3Aarticle%2Cis_paratext%3Afalse%2Cis_retracted%3Afalse%2Cauthors_count%3A%3E0%2Chas_doi%3Atrue%2Cprimary_location.source.has_issn%3Atrue&sample=10&seed=202054&select=id%2Cauthorships%2Cpublication_year%2Ccited_by_count>
#> Using basic paging...
#> ℹ Getting 1 page of results with a total of 10 records...
#> Year 2020 completed in 0.3 min

if (FALSE) { # \dontrun{
# Get a large reference set for psychology. Save intermediate (yearly)
# results in case the process is interrupted
start <- Sys.time()
refset <- get_reference_set(
    years = 2001:2024,
    n_per_year    = 100000,
    concept.id    = "C15744967",
    type          = "article",
    seed          = 42,
    verbose       = TRUE,
    save_intermediate = "~/refset_temp"
)
end <- Sys.time()
print(end-start)
} # }
```
