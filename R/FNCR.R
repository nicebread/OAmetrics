
# ref_set <- readRDS(file="/Users/felix/Documents/Gitlab/R-openAlex/raw_data/c_counts_psy.RDS")



#' Calculates the upper trimmed mean of a vector
#'
#' @param x A numeric vector
#' @param trim A numeric value between 0 and 1 indicating the proportion of values to trim from the top
#'
#' @return A numeric value representing the upper trimmed mean of the vector
#'
#' @examples
#' x <- c(1,2,3,4,5,6,7)
#' upper_trim_mean(x, 0.2)
#'
#' @export
upper_trim_mean <- function(x, trim) {
  x <- sort(x)
  mean(x[1:floor(length(x)*(1-trim))])
}



#' @title ecdf2
#' @description Calculates an linearly interpolated empirical cumulative distribution function (ecdf) from a vector of values. This function was provided by Tal Galili on [StackOverflow](https://stats.stackexchange.com/q/230458).
#' @param x A vector of values.
#' @return A function that calculates the linearly interpolated ecdf of the vector of values.
#' @importFrom stats approxfun ecdf
#' @export
#' @examples
#' x <- c(1,2,3,4,5,6,7,8)
#' ecdf2(x)
ecdf2 <- function (x) {
  x <- sort(x)
  n <- length(x)
  if (n < 1)
    stop("'x' must have 1 or more non-missing values")
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n,
                    method = "linear", yleft = 0, yright = 1, f = 0, ties = "ordered")
  class(rval) <- c("ecdf", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}


#' Compute the Field Normalized Citation Score of a publication
#'
#' The percentage rank is the "CP-IN" measure described in Bornmann & Williams (2020). For the percentile rank, the function uses a linear interpolation (cf. Bornmann & Williams, 2020) using a function provided by Tal Galili (https://stats.stackexchange.com/q/230458)
#'
#' @param doi A character string of the DOI of the paper for which the FNCS should be computed.
#' @param ref_set A data frame containing the reference set for the paper of interest. This is an object from the `get_reference_set` function.
#' @param upper_trim A numeric value between 0 and 1 that indicates the fraction of values to be trimmed from the upper end of the reference set. Scheidsteger et al. (2023) remove the upper 1\% of citation counts when using OpenAlex. This only affects the FNCS, not the percentiel rank.
#' @return A list containing the computed FNCS and the percentile rank of the paper.
#' @export
#' @references
#' Bornmann, L., & Williams, R. (2020). An evaluation of percentile measures of citation impact, and a proposal for making them better. Scientometrics, 124(2), 1457–1478. https://doi.org/10.1007/s11192-020-03512-7
#' Scheidsteger, T., Haunschild, R., & Bornmann, L. (2023). How similar are field-normalized scores from different free or commercial databases calculated for large German universities? https://dapp.orvium.io/deposits/6441118c643beb0d90fc543f/view

#' @examples
#' \dontrun{
#' # TODO: ref_set does not exist yet
#' FNCS(doi = "10.1038/s41586-019-1712-z", ref_set = ref_set, upper_trim = .01)
#' }

# Compute the field normalized citation scores
FNCS <- function(doi, ref_set, upper_trim = .01) {

  # get citation counts for a specific paper.
  paper <- oa_fetch(entity = "works", doi = doi)

  ref_set_year <- ref_set %>% filter(publication_year == paper$publication_year)

  if (nrow(ref_set_year) == 0) {
    stop("The reference set does not contain publication from the same year (", paper$publication_year,")")
  } else {
    print(paste0("The reference set for the year ", paper$publication_year, " has ", nrow(ref_set_year), " publications."))
  }

  # What citation counts would be expected in the same field from publications of the same year?
  expected_cc <- upper_trim_mean(ref_set_year$cited_by_count, trim=upper_trim)

  # TODO: If a paper is assigned to multiple fields, this should be an weighted average...
  FNCS <- paper$cited_by_count / expected_cc

  # Quantile / percentage rank
  c_count_ecdf <- ecdf(ref_set_year$cited_by_count)
  c_count_ecdf2 <- ecdf2(ref_set_year$cited_by_count)

  # Plot the step-wise and the linear interpolation ECDF
  #par(mfrow = c(1,2))
  #curve(c_count_ecdf,  0, max(ref_set_year$cited_by_count), main = "step function ecdf")
  #curve(c_count_ecdf2, 0, max(ref_set_year$cited_by_count), main = "linear interpolation function ecdf")

  perc_rank <- c_count_ecdf2(paper$cited_by_count)

  return(list(citation_count = paper$cited_by_count, FNCS = FNCS, FN_PR = perc_rank))
}
