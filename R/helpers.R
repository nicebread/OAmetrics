#' Normalize DOIs
#'
#' This function normalizes DOIs by trimming leading/trailing whitespace,
#' replacing 'dx.doi.org' with 'doi.org', replacing 'http:' with 'https:',
#' and converting the DOI to lowercase, according to the DOI specification
#'
#' @param x Character vector of DOIs to be normalized
#' @param verbose Show diagnostic information?
#' @return Character vector of normalized DOIs
#' @import stringr
#' @examples
#' normalize_dois(c("  10.123.10.1/DOI ", "http://dx.doi.org/10.456/Doi"))
#' @export
normalize_dois <- function(x, verbose=FALSE) {
  x2 <- x |>
    str_trim() |>
    str_replace("doi: ", "") |>
    str_replace("dx.doi.org", "doi.org") |>
    str_replace("^doi.org/10.", "https://doi.org/10.") |>
    str_replace("http:", "https:") |>
    str_replace("^10.", "https://doi.org/10.") |>
    str_to_lower()  # according to the DOI specification, it is case-insensitive. OpenAlex uses only lowercase dois.

  return(x2)
}



#' Normalize ORCIDs
#'
#' This function normalizes ORCIDs to the canonical form
#' (which is the URL to the profile)
#'
#' @param x Character vector of ORCIDs to be normalized
#' @return Character vector of normalized ORCIDS
#' @importFrom stringr str_extract
#' @examples
#' normalize_ORCIDs(c("orcid.org/0000-1234-5678-9111",
#'     "0000-1234-5678-9111", "  https://orcid.org/0000-1234-5678-9111 "))
#' @export
normalize_ORCIDs <- function(x) {
  # 1) trim whitespace
  xx <- str_trim(x)
  # 2) extract the 16-digit ORCID (allow hyphen or space between blocks)
  orc <- str_extract(xx, "(?:\\d{4}[- ]?){3}\\d{3}[\\dX]")
  # 3) build full URL where not NA, else NA_character_
  ifelse(is.na(orc),
         NA_character_,
         paste0("https://orcid.org/", orc))
}



#' Get Number of Authors from an OpenAlex Object
#'
#' Extracts the number of authors for each record in an OpenAlex-style list object.
#'
#' @param OA_object A list-like object (e.g., from OpenAlex API) containing an \code{author} element.
#'
#' @return A numeric vector giving the number of authors for each entry.
#'   Entries with no author information (\code{NULL}) are returned as \code{NA}.
#'
#' @details
#' The function applies \code{nrow} to each element of \code{OA_object$author}.
#' If any entries are \code{NULL}, they are converted to \code{NA} in the output.
#' @export
get_n_authors <- function(OA_object) {
  n_authors <- sapply(OA_object$author, nrow)

  # find NULL entries
  NULL_entries <- sapply(n_authors, is.null)
  if (any(NULL_entries)) {
    n_authors[NULL_entries] <- NA
  }

  return(n_authors |> unlist())
}
