#' Get a reference set for field normalized citation rates
#'
#' This function downloads a large set of publications from a certain field and certain years.
#' Retrieving 10,000 documents from the API takes about 1-2 min.
#' Ideally, this should be cached locally.
#'
#' @param years The year(s) from which a sample of the reference field should be retrieved.
#' @param n_per_year The number of documents to retrieve per requested year (optional, defaults to 10000). Values larger than 10000 are possible (they are split up to multiple OA requests).
#' @param concept.id A vector of `concept.id`s to search for (optional, defaults to "C15744967", i.e. "Psychology")
#' @param type Reference sets should refer to the same type of publication; defaults to "article"
#' @param verbose Show OA API progress?
#' @param seed Set a seed for reproducible analyses. However, as the underlying OA database changes frequently, the results will still not be very stable ...
#' @param save_intermediate If a path is provided here, the intermediate downloaded files are saved at that path.
#' @return A data frame containing the document id, year, cited_by_count, and number of authors of the retrieved documents
#'
#' @export
#'
#' @examples
#' # Get reference set for "Psychology" for multiple years (small n here for demo)
#' psych_ref <- get_reference_set(
#'   years = 2018:2020, n_per_year = 20,
#'   concept.id = "C15744967"
#'  )

get_reference_set <- function(years, n_per_year=10000, concept.id = "C15744967", type="article", verbose=TRUE, seed = NULL, save_intermediate = NULL) {

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

      # TODO: around 20% of sampled document are duplicates. Ensure that only unique documents are sampled (use paging?)
      pages[[paste0(y, "_", page)]] <- oa_fetch(
        entity = "works",
        options = list(
            sample = ifelse((n_per_year - n_retrieved) >= 10000, 10000, n_per_year - n_retrieved),
            seed = seed2,
            select = c("id", "authorships", "publication_year", "cited_by_count")
          ),
        concept.id = concept.id,
        type = type,
        from_publication_date = paste0(y, "-01-01"),
        to_publication_date = paste0(y, "-12-31"),
        is_paratext = FALSE,
        is_retracted = FALSE,
        abstract = FALSE,
        authors_count = ">0",  # remove corrections (which have no authors)
        has_doi = TRUE,   # TODO: is that filter legit?
        primary_location.source.has_issn = TRUE,    # TODO: is that filter legit?
        verbose=verbose
      )

      if (!is.null(save_intermediate)) {
        saveRDS(pages[[paste0(y, "_", page)]], file=paste0(save_intermediate, "/page_", y, "_", page, ".RDS"))
      }

      n_retrieved <- n_retrieved + nrow(pages[[paste0(y, "_", page)]])
      if (verbose==TRUE) {print(paste0("Retrieved ", n_retrieved, " documents"))}
      page <- page + 1
    }
  }

  res <- data.table::rbindlist(pages)
  res$n_authors <- sapply(res$author, nrow)

  return(data.frame(
    id = res$id,
    publication_year = res$publication_year,
    n_authors = res$n_authors,
    cited_by_count = res$cited_by_count
  ))
}





#' @title Helper function: Get reference set from files
#' @description The `get_reference_set` function can save intermediate files on a drive. In case that the download or the function aborts with an error, all existing files in a given path can be read combined.
#' @param path The relative or absolute path of the files
#' @return Returns a data frame with a reference set.
#' @export
#' @importFrom data.table rbindlist
#'
get_reference_set_from_files <- function(path) {
  pages <- list()
  for (f in list.files(path, full.names = TRUE)) {
    print(paste0("Reading ", f))
    res <- readRDS(f)
    pages[[f]] <- data.frame(
      id = res$id,
      publication_year = res$publication_year,
      n_authors = sapply(res$author, nrow),
      cited_by_count = res$cited_by_count
    )
  }

  return(data.table::rbindlist(pages))
}
