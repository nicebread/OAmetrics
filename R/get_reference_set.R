#' Get a reference set for field- and age-normalized citation rates
#'
#' This function downloads a large set of publications from a certain field and certain years.
#' Retrieving 10,000 documents from the API takes about 1-2 min.
#' Ideally, this should be cached locally. OpenAlex will only return random samples for up to 10,000 records. If you request multiples random samples, you will get duplicates. Therefore we stratify the request by month (to get non-overlapping samples). So we can download 10,000 * 12 unique entries. When more than 120,000 samples are requested, some duplicates can happen, but should be rare.
#'
#' @param years The year(s) from which a sample of the reference field should be retrieved.
#' @param n_per_year The number of documents to retrieve per requested year (optional, defaults to 10000). Values larger than 10000 are possible (they are split up to multiple OA requests).
#' @param concept.id A vector of `concept.id`s to search for (optional, defaults to "C15744967", i.e. "Psychology")
#' @param type Reference sets should refer to the same type of publication; defaults to "article"
#' @param verbose Show OA API progress?
#' @param per_page How many records should be requested per page? (200 is the maximum)
#' @param seed Set a seed for reproducible analyses. However, as the underlying OA database changes frequently, the results will still not be very stable ...
#' @param save_intermediate If a path is provided here, the intermediate downloaded files are saved at that path.
#' @param max_retries How many times to retry if the OpenAlex API returns an error
#' @param retry_delay Base delay (in seconds) between retries
#' @importFrom lubridate days days_in_month
#' @return A data frame containing the document id, year, cited_by_count, and number of authors of the retrieved documents
#'
#' @export
#'
#' @examples
#' # Get reference set for "Psychology" for multiple years (small n here for demo)
#' psych_ref <- get_reference_set(
#'   years = 2018:2020, n_per_year = 120,
#'   concept.id = "C15744967"
#'  )
#'
#' \dontrun{
#' # Get a large reference set for psychology. Save intermediate (yearly)
#' # results in case the process is interrupted
#' start <- Sys.time()
#' refset <- get_reference_set(
#'     years = 2001:2024,
#'     n_per_year    = 100000,
#'     concept.id    = "C15744967",
#'     type          = "article",
#'     seed          = 42,
#'     verbose       = TRUE,
#'     save_intermediate = "~/refset_temp"
#' )
#' end <- Sys.time()
#' print(end-start)
#' }
get_reference_set <- function(
    years,
    n_per_year    = 10000,
    concept.id    = "C15744967",
    type          = "article",
    seed          = 42,
    verbose       = TRUE,
    per_page      = 200,
    save_intermediate = NULL,
    max_retries       = 3,
    retry_delay       = 5
) {
  # Maximum per-month sample is capped at 10,000 (API limit)
  samp_per_month <- ceiling(n_per_year / 12)
  samp_per_month <- pmin(samp_per_month, 10000L)

  # A small helper to retry a call to oa_fetch()
  fetch_month_with_retry <- function(fetch_args) {
    attempt <- 1
    repeat {
      if (verbose) message("-> Fetch attempt ", attempt)
      res <- try(do.call(oa_fetch, fetch_args), silent = TRUE)
      if (!inherits(res, "try-error")) {
        return(res)
      }
      if (attempt >= max_retries) {
        stop("Failed after ", max_retries, " attempts: ", conditionMessage(res))
      }
      wait <- retry_delay * 2^(attempt - 1)
      message("Error: ", conditionMessage(res),
              " - retrying in ", wait, "s ...")
      Sys.sleep(wait)
      attempt <- attempt + 1
    }
  }


  yearly_results <- lapply(years, function(y) {
    if (verbose) message("Year ", y, ": sampling ", samp_per_month, " per month")
    start_y <- Sys.time()

    month_slices <- lapply(1:12, function(m) {
      start_date <- as.Date(sprintf("%04d-%02d-01", y, m))
      end_date   <- start_date + days(days_in_month(start_date) - 1)
      pages_needed <- seq_len(ceiling(samp_per_month / per_page))
      slice_seed   <- as.integer(seed + y * 100 + m)

      if (verbose) message("  Month ", sprintf("%02d", m),
                           ": ", start_date, " --> ", end_date)

      # Build arguments list for do.call()
      fetch_args <- list(
        entity                 = "works",
        publication_year       = y,
        from_publication_date  = as.character(start_date),
        to_publication_date    = as.character(end_date),
        `concept.id`           = concept.id,
        type                   = type,
        is_paratext            = FALSE,
        is_retracted           = FALSE,
        authors_count          = ">0",
        has_doi                = TRUE,
        `primary_location.source.has_issn` = TRUE,
        options                = list(
          sample = samp_per_month,
          seed   = slice_seed,
          select = c("id",
                     "authorships",
                     "publication_year",
                     "cited_by_count")
        ),
        per_page = per_page,
        pages    = pages_needed,
        verbose  = verbose
      )

      # Fetch with retry logic
      df_month <- fetch_month_with_retry(fetch_args)

      # Count authors
      df_month$n_authors <- sapply(df_month$author, nrow)

      # Return core columns
      df_month %>%
        select(id, publication_year, n_authors, cited_by_count)
    })

    # Combine 12 monthly results (disjoint by design)
    dt_year <- rbindlist(month_slices)

    end_y <- Sys.time()
    if (verbose) {
      message("Year ", y, " completed in ", round(difftime(end_y, start_y, units = "mins"), 1), " min")
    }

    if (!is.null(save_intermediate)) {
      if (!dir.exists(save_intermediate)) {
        dir.create(save_intermediate, recursive = TRUE, showWarnings = FALSE)
      }
      saveRDS(dt_year, file=paste0(save_intermediate, "/refset_", y, ".RDS"))
    }

    dt_year
  })

  # Bind all years and return data.frame
  result <- rbindlist(yearly_results)
  as.data.frame(result)
}





#' @title Helper function: Get reference set from files
#' @description The `get_reference_set` function can save intermediate files on a drive. In case that the download or the function aborts with an error, all existing files in a given path can be read combined.
#' @param path The relative or absolute path of the files
#' @return Returns a data frame with a reference set.
#' @importFrom data.table rbindlist
#'
get_reference_set_from_files <- function(path) {
  pages <- list()
  for (f in list.files(path, full.names = TRUE, pattern = "*.RDS")) {
    print(paste0("Reading ", f))
    res <- readRDS(f)
    pages[[f]] <- res
  }

  return(data.table::rbindlist(pages))
}
