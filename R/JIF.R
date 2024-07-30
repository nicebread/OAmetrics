#' Compute the Journal Impact Factor
#'
#' This function computes the Journal Impact Factor (JIF) for a given journal
#' given its ISSN and a target year.
#'
#' @param issn The ISSN of the journal
#' @param year The target year
#' @param verbose Whether to print verbose output (default is FALSE)
#'
#' @return A data frame with columns for journal, ISSN, year, total_citations,
#' citable_items and JIF.
#'
#' @examples
#' get_JIF(issn="0022-3514", year=2018)  # JPSP
#' get_JIF(issn="0890-2070", year=2019)  # EJP
#'
#' @importFrom data.table rbindlist
#' @importFrom lubridate year
#' @export

get_JIF <- function(issn, year, verbose=FALSE) {

  # if year is the current year, then the citation data will not be available for the computation
  if (year >= as.integer(format(Sys.Date(), "%Y"))) {
    warning(paste0("Cannot compute JIF for the year '", year, "'"))
    return(data.frame(
      journal = NA,
      issn = issn,
      year = year,
      total_citations = NA,
      citable_items = NA,
      JIF = NA
    ))
  }

  all_works_search <- oa_fetch(
    entity = "works",
    primary_location.source.issn = issn,
    from_publication_date = paste0(year-2, "-01-01"),
    to_publication_date = paste0(year-1, "-12-31"),
    abstract=FALSE,
    authors_count = ">0",  # remove corrections (which have no authors)
   # options = list(
   #   select = c("id", "publication_date", "cited_by_count", "counts_by_year")
   # ),
    verbose=verbose
  )

  if (is.null(all_works_search)) {
    return(data.frame(
      journal = NA,
      issn = issn,
      year = year,
      total_citations = NA,
      citable_items = NA,
      JIF = NA
    ))
  }

  all_works_search$publication_year <- lubridate::year(all_works_search$publication_date)

  journal_info <- oa_fetch(
    entity = "sources",
    issn = issn
  )

  # For denominator: remove supplemental material and corrections
  citable_items <- all_works_search %>% filter(
    # !is.na(referenced_works),  # TODO: This does not work for all journals: Some don't have referenced_work in the OA data (although they do have references); add to "options(select = )" in oa_fetch if you need it.
    # type == "article"  # some journals have "book-chapter" as meta-data ...
  )

  # get citations to these articles from a specific target year
  has_citations <- !(sapply(all_works_search$counts_by_year, nrow) |> sapply(is.null))
  cc_per_year <- data.table::rbindlist(all_works_search[has_citations, ]$counts_by_year)

  total_citations <- sum(cc_per_year$cited_by_count[cc_per_year$year==year])

  res <- data.frame(
    journal = journal_info[1, "display_name"],
    issn = issn,
    year = year,
    total_citations = total_citations,
    citable_items = nrow(citable_items),
    JIF = total_citations/nrow(citable_items)
  )
  colnames(res)[1] <- "journal"
  return(res)
}
