% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JIF.R
\name{get_JIF}
\alias{get_JIF}
\title{Compute the Journal Impact Factor}
\usage{
get_JIF(issn, year, limit = NA, verbose = FALSE, seed = NA)
}
\arguments{
\item{issn}{The ISSN of the journal}

\item{year}{The target year}

\item{limit}{Upper limit of papers to be downloaded (random selection). Some mega-journals, such as Scientific Reports, have > 45,000 papers in the two-year time window, which takes multiple to download. Typical journals in psychology have only hundreds of papers. Limiting the papers can give you a slight underestimation, as the JIF is also driven by rare outliers (with huge citation scores), which are only covered when all papers are considered.}

\item{verbose}{Whether to print verbose output (default is FALSE)}

\item{seed}{Seed for a random retrieval of papers. If NA (the default), a random seed is chosen.}
}
\value{
A data frame with columns for journal, ISSN, year, total_citations,
citable_items and JIF.
}
\description{
This function computes the Journal Impact Factor (JIF) for a given journal
given its ISSN and a target year.
}
\examples{
get_JIF(issn="0022-3514", year=2018)  # JPSP
get_JIF(issn="0890-2070", year=2019)  # EJP
# Scientific Reports; according to website the JIF is 3.8 in 2023
get_JIF(issn="2045-2322", year=2023, limit=5000)

}
