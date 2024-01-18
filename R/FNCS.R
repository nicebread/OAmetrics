
# ref_set <- c_counts_psy <- readRDS(file="/Users/felix/Documents/Gitlab/R-openAlex/raw_data/c_counts_psy.RDS")
# doi <- c("https://doi.org/10.1027/2151-2604/a000342", "https://doi.org/10.3389/fpsyg.2017.02119", "https://doi.org/10.1177/0956797617723726")


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
#' x <- c(0,0,0,0,0,0,1,1,1,1,2,2,2,5,7,11,20,100)
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


#' Compute the Field Normalized Citation Score (FNCS) and Percentage Rank (FNPR) of a publication
#'
#' The FNCS reflects the impact of a paper relative to a reference set, namely publications of the same type (typically, journal articles), published in the same year, and in the same field. The “same field” can be operationalized, for example, by the top-level concepts as assigned by the OpenAlex database. Once a reference set has been compiled, the FNCS of a paper is computed as the raw citation count divided by the average citation count of papers in the reference set. It can be interpreted as a ratio: A value of 1 means that a paper has received as many citations as an average paper in the reference set, 2 means that it received two times as many citations.
#' The percentage rank is the "CP-EX" measure described in Bornmann & Williams (2020), which returns the percentage of publications with strictly less citations (and *not* "less or equal"). As many publications have 0 citations, the alternative CP-IN measure (which return "less or equal") returns often very high percentiles although a paper has 0 citations. This is not intuitive. For the percentile rank, the function applies a linear interpolation on the empirical reference data (cf. Bornmann & Williams, 2020) using a function provided by Tal Galili (https://stats.stackexchange.com/q/230458). The FNPR is a monotonic transformation of the FNCS; except adding new aspects of interpretation and providing some robustness against outliers, it doesn't add any new information compared tot he FNCS.
#'
#' @param dois A character vector of the DOI of the paper for which the FNCS should be computed.
#' @param papers A data frame with the papers that should be analyzed, as provided by an `oa_fetch` call. Either provide `dois` or `papers`.
#' @param ref_set A data frame containing the reference set for the paper of interest. This is an object from the `get_reference_set` function.
#' @param upper_trim A numeric value between 0 and 1 that indicates the fraction of values to be trimmed from the upper end of the reference set. Scheidsteger et al. (2023) remove the upper 1 percent of citation counts when using OpenAlex. This only affects the FNCS, not the percentile rank (FNPR).
#' @return A list containing the computed FNCS and the percentile rank of the paper. The latter is the CP-EX measure which means "how many citations in the reference set have *less* citations than the target paper".
#' @export
#' @references
#' Bornmann, L., & Williams, R. (2020). An evaluation of percentile measures of citation impact, and a proposal for making them better. Scientometrics, 124(2), 1457–1478. https://doi.org/10.1007/s11192-020-03512-7
#' Scheidsteger, T., Haunschild, R., & Bornmann, L. (2023). How similar are field-normalized scores from different free or commercial databases calculated for large German universities? https://dapp.orvium.io/deposits/6441118c643beb0d90fc543f/view

#' @examples
#' \dontrun{
#' # TODO: ref_set does not exist yet
#' FNCS(dois = "10.1177/2515245918810225", ref_set = ref_set, upper_trim = .01)
#' }

# Compute the field normalized citation scores
FNCS <- function(dois=NULL, papers=NULL, ref_set=NULL, upper_trim = 0) {

  if (is.null(ref_set)) stop("You need to provide a reference set.")

  if (is.null(dois) & is.null(papers)) {
    stop("You have to provide either dois or works.")
  } else if (!is.null(dois) & is.null(papers)) {
    # get citation counts for all provided dois
    papers <- oa_fetch(entity = "works", doi = dois, abstract=FALSE,
                       options = list(select=c("id", "doi", "cited_by_count", "publication_year", "display_name")))
  } else if (is.null(dois) & !is.null(papers)) {
    # check if all necessary columns exist
    coldiff <- setdiff(c("id", "doi", "cited_by_count", "publication_year", "display_name"), colnames(papers))
    if (length(coldiff) > 0) stop(paste0("The following columns are missing in the `paper` object: ", paste(coldiff, collapse=", ")))
  }


  # What citation counts would be expected in the same field from publications of the same year?
  yearly_expected_c <- ref_set %>%
    select(-id) %>%
    group_by(publication_year) %>%
    summarise(mean_c = upper_trim_mean(cited_by_count, trim=upper_trim))

  # Does the reference set contain publications from the relevant years?
  years_not_in_ref_set <- setdiff(papers$publication_year, yearly_expected_c$publication_year)
  if (length(years_not_in_ref_set) > 0) {
    warning(paste0("The following years are not in the reference set: ", paste(years_not_in_ref_set, collapse=", "), ". No metrics can be computed for publications from these years."))
  }

  # TODO: If a paper is assigned to multiple fields, this should be an weighted average...
  res <- data.frame(
    doi = papers$doi,
    cited_by_count = papers$cited_by_count,
    FNCS = NA,
    FNPR = NA
  )

  papers <- left_join(papers, yearly_expected_c, by="publication_year") %>%
    mutate(
      FNCS = cited_by_count / mean_c,
      FNPR = NA  # initialize with NA; fill in below
    )

  for (i in 1:nrow(papers)) {
    if (papers$publication_year[i] %in% yearly_expected_c$publication_year) {
      # Quantile / percentage rank. We add 1 to each citation count; this way we obtain the CP-EX percentage rank (instead of the CP-IN rank), which returns "percentage of publications with less than X citations" (in contrast to "less or equal", which is returned by the regular ecdf function).
      ref_set_cites <- ref_set[ref_set$publication_year == papers$publication_year[i], "cited_by_count"] + 1
      c_count_ecdf2 <- ecdf2(unlist(ref_set_cites))
      papers$FNPR[i] <- c_count_ecdf2(papers$cited_by_count[i])
    } else {
      print(paste0("No metrics could be computed for ", papers$doi[i], ": Publication_year ", papers$publication_year[i], " missing in reference set"))
    }
  }

  # Plot the step-wise and the linear interpolation ECDF
  #par(mfrow = c(1,2))
  #curve(c_count_ecdf,  0, max(ref_set_year$cited_by_count), main = "step function ecdf")
  #curve(c_count_ecdf2, 0, max(ref_set_year$cited_by_count), main = "linear interpolation function ecdf")

  return(papers %>% select(doi, publication_year, title=display_name, cited_by_count, expected_citations = mean_c, FNCS, FNPR))
}

