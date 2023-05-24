#' Get a reference set for field normalized citation rates
#'
#' This function downloads a large set of publications from a certain field and certain years.
#' Retrieving 10,000 documents from the API takes about 1-2 min.
#' Ideally, this should be cached locally.
#'
#' @param years The year(s) from which a sample of the reference field should be retrieved.
#' @param n_per_year The number of documents to retrieve per requested year (optional, defaults to 10000). Values larger than 10000 are possible (they are split up to multiple OA requests).
#' @param concept.id A vector of `concept.id`s to search for (optional, defaults to "C15744967", i.e. "Psychology")
#' @param verbose Show OA API progress?
#' @return A data frame containing the document id, year, cited_by_count, and number of authors of the retrieved documents
#'
#' @export
#'
#' @examples
#' # Get reference set for "Psychology" for multiple years (small n here for demo)
#' psych_ref <- get_reference_set(
#'   2013:2020, n_per_year = 100,
#'   concept.id = "C15744967"
#'  )

get_reference_set <- function(years, n_per_year=10000, concept.id = "C15744967", verbose=TRUE, seed = NULL) {

  pages <- list()
  for (y in years) {
    if (verbose==TRUE) {print(paste0(Sys.time(), ": Retrieving documents for ", y, " ..."))}

    n_retrieved <- 0
    page <- 1

    while (n_retrieved < n_per_year) {
      if (!is.null(seed)) {
        seed2 <- seed + y*100 + page
      } else {
        seed2 <- NULL
      }

      pages[[paste0(y, "_", page)]] <- oa_fetch(
        entity = "works",
        options = list(
            sample = ifelse((n_per_year - n_retrieved) >= 10000, 10000, n_per_year - n_retrieved),
            seed = seed2
          ),
        concept.id = concept.id,
        type = "journal-article",
        from_publication_date = paste0(y, "-01-01"),
        to_publication_date = paste0(y, "-12-31"),
        is_paratext = FALSE,
        is_retracted = FALSE,
        abstract = FALSE,
        authors_count = ">0",  # remove corrections (which have no authors)
        has_doi = TRUE,   # TODO: is that filter legit?
        #primary_location.source.has_issn = TRUE,    # TODO: is that filter legit?
        verbose=verbose
      )

      n_retrieved <- n_retrieved + nrow(pages[[paste0(y, "_", page)]])
      if (verbose==TRUE) {print(paste0("Retrieved ", n_retrieved, " documents"))}
      page <- page + 1
    }
  }

  res <- data.table::rbindlist(pages)
  res$n_authors <- sapply(res$author, nrow)

  # remove supplemental material and corrections
  # TODO: No good mechanism yet ...

  # TODO: Which columns are necessary to return?
  return(data.frame(
    id = res$id,
    publication_year = res$publication_year,
    n_authors = res$n_authors,
    cited_by_count = res$cited_by_count
  ))
}

