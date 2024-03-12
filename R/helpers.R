#' Normalize DOIs
#'
#' This function normalizes DOIs by trimming leading/trailing whitespace,
#' replacing 'dx.doi.org' with 'doi.org', replacing 'http:' with 'https:',
#' and converting the DOI to lowercase, according to the DOI specification
#'
#' @param x Character vector of DOIs to be normalized
#' @return Character vector of normalized DOIs
#' @import stringr
#' @examples
#' normalize_dois(c("  10.123.10.1/DOI ", "http://dx.doi.org/10.456/Doi"))
#' @export
normalize_dois <- function(x, verbose=FALSE) {
  x2 <- x |>
    str_trim() |>
    str_replace("dx.doi.org", "doi.org") |>
    str_replace("http:", "https:") |>
    str_replace("^10.", "https://doi.org/10.") |>
    str_to_lower()  # according to the DOI specification, it is case-insensitive. OpenAlex uses only lowercase dois.

  if (any(x2 != x) & verbose==TRUE) {
    warning("It is strongly recommended to normalize dois with the `normalize_dois()` function.")
  }

  return(x2)
}
