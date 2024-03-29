% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_reference_set.R
\name{get_reference_set}
\alias{get_reference_set}
\title{Get a reference set for field normalized citation rates}
\usage{
get_reference_set(
  years,
  n_per_year = 10000,
  concept.id = "C15744967",
  type = "article",
  verbose = TRUE,
  seed = NULL,
  save_intermediate = NULL
)
}
\arguments{
\item{years}{The year(s) from which a sample of the reference field should be retrieved.}

\item{n_per_year}{The number of documents to retrieve per requested year (optional, defaults to 10000). Values larger than 10000 are possible (they are split up to multiple OA requests).}

\item{concept.id}{A vector of \code{concept.id}s to search for (optional, defaults to "C15744967", i.e. "Psychology")}

\item{type}{Reference sets should refer to the same type of publication; defaults to "article"}

\item{verbose}{Show OA API progress?}

\item{seed}{Set a seed for reproducible analyses. However, as the underlying OA database changes frequently, the results will still not be very stable ...}

\item{save_intermediate}{If a path is provided here, the intermediate downloaded files are saved at that path.}
}
\value{
A data frame containing the document id, year, cited_by_count, and number of authors of the retrieved documents
}
\description{
This function downloads a large set of publications from a certain field and certain years.
Retrieving 10,000 documents from the API takes about 1-2 min.
Ideally, this should be cached locally.
}
\examples{
# Get reference set for "Psychology" for multiple years (small n here for demo)
psych_ref <- get_reference_set(
  years = 2018:2020, n_per_year = 20,
  concept.id = "C15744967"
 )
}
