#' Compute h-index and hIa-index
#'
#'TODO: Could use a time-window (currently, it uses all citations since publication).
#'
#' Computes the h-index and hIa-index of an author. You need to provide exactly *one* of the parameters `search`, `display_name`, `author.id`, or `ORCID`:
#' - `search` allows fuzzy search of the author's name (e.g. with or without middle initial). Might return too many matches
#' - `display_name` requires an exact match but allows to provide a vector with multiple versions of a name (e.g., `c("Hans Müller", "Hans Mueller", "Hans J. Muller")`)
#' - `author.id` is the OpenAlex ID for authors.
#' - `ORCID` is the ORCID (either as link or as the number quartet string)
#'
#' Often the same author is listed under different IDs in the OpenAlex database, therefore these are merged by the function. **But what if the function includes wrong authors?** Then you need to manually screen the table with author names, record the IDs from the correct aliases of the target person (starting with "A.......") and enter these author IDs into the `author.id` parameter.
#'
#' @param search A character string with the author's name for searching in the OpenAlex API. Allows fuzzy search (e.g. with or without middle initial).
#' @param display_name A character vector with the exact author's names for searching in the OpenAlex API. You cvan provide multiple variants in a vector.
#' @param author.id A character vector with OpenAlex author ID(s) (i.e., an ID starting with "A.......") for searching in the OpenAlex API.
#' @param ORCID A character vector with ORCID IDs (either as URI or as the number quartet string).
#' @param first_pub_year An integer with the year of the author's first publication. If not provided, it is estimated from the retrieved publications.
#' @param academic_age_bonus Years that are subtracted from active academic life (e.g. due to child care). For example, SNF subtracts 1.5 years per child (see https://www.snf.ch/en/cciM9NWuvhOVKRxv/news/news-200803-career-funding-the-researchers-overall-performance-counts).
#'
#' @return A list containing the net academic age, h-index and hIa-index of the author.
#'
#' @export
#'
#' @examples
#' h_index(search = "Felix Schönbrodt")
#' h_index(ORCID = "https://orcid.org/0000-0002-8282-3910")
#' h_index("Markus Bühner", first_pub_year = 1997) # adjust year of first publication
#'
#'
#' @import openalexR
#' @import dplyr
#'
h_index <- function(search = NULL, display_name = NULL, author.id = NULL, ORCID = NULL, first_pub_year = NA, academic_age_bonus = 0) {

  if (sum(is.null(display_name), is.null(author.id), is.null(search), is.null(ORCID)) != 3) {
    stop("Please provide EITHER display_name OR author.id OR search OR ORCID.")
  }


  if (!is.null(display_name) | !is.null(search) | !is.null(ORCID)) {

    if (!is.null(display_name)) {
      authors_from_names <- oa_fetch(entity = "authors", display_name = display_name)
    } else if (!is.null(search)) {
      authors_from_names <- oa_fetch(entity = "authors", search = search)
    } else if (!is.null(ORCID)) {
      authors_from_names <- oa_fetch(entity = "authors", orcid = ORCID)
    }


    if (nrow(authors_from_names) > 1 & authors_from_names[2, "cited_by_count"] > 1) {
      print(authors_from_names[, c("id", "display_name", "works_count", "cited_by_count")])

      author.id <- authors_from_names$id[authors_from_names$cited_by_count > 1]
      cat("Multiple author IDs have substantial citation count - merged author IDs. Please check if they are correct.\n\n")
    } else {
      cat("A unique author record has been found: \n\n")
      print(authors_from_names[1, c("display_name", "works_count", "cited_by_count")])
      author.id <- authors_from_names[1, "id"]
    }
  }

  works <- oa_fetch(
    entity = "works",
    author.id = author.id,
    is_paratext = FALSE,
    is_retracted = FALSE,
    abstract=FALSE,
    verbose = FALSE
  )

  # filter out works that should not count for the h-index, and for the computation of academic age (i.e., works without doi and which are not published in an outlet).
  # This is mostly irrelevant for the h-index, but important for the automatic retrieval of academic age.
  works <- works %>% filter(!is.na(doi), !is.na(so))

  # if a first_publication_year is provided: strip all older publications from h-index computation
  if (!is.na(first_pub_year)) {
    works <- works %>% filter(publication_year >= first_pub_year)
  }

  # early stop:
  if (nrow(works) == 0) {
    warning("Author has zero works in data base, cannot compute h-index.")
    return(list(net_academic_age=NA, h_index=NA, h_Ia = NA, first_publication=NA))
  }

  works$n_authors <- sapply(works$author, nrow)

  works <- works %>% arrange(-cited_by_count)
  works$h_position <- 1:nrow(works)

  h_index <- sum(works$cited_by_count > works$h_position)

  # retrieve first publication from data set
  if (is.na(first_pub_year)) {
    first_pub_year <- min(works$publication_year)
    first_work <- works %>% arrange(publication_year) %>% slice(1)
    co <- first_work$concepts[[1]]
    top_L0_concept <- co %>% filter(level==0) %>% slice(1) %>% pull("display_name")

    academic_age_note <- paste0("Computation of academic age (relative to first publication):\nRetrieving year of first publication from data: ", first_pub_year, ". \n\nThe first publication is: '", first_work$display_name,"', published in '", first_work$so, "'. It has the top L0 concept '", top_L0_concept, "'. \n\nIs that plausible? If not, provide `first_pub_year` as a parameter. You can see all works of that author with the following command:\n\noa_fetch(entity = 'works', author.id = '", author.id, "', is_paratext = F, is_retracted = F, abstract=F) %>% arrange(publication_year)\n\n")
    cat(academic_age_note)
  } else {
    first_work <- works %>% arrange(publication_year) %>% slice(1)
    academic_age_note <- ""
  }



  # hIa: hI,norm/academic age, where:
  #   hI,norm: normalize the number of citations for each paper by dividing the number of citations by the number of authors for that paper, and then calculate the h-index of the normalized citation counts
  # academic age: number of years elapsed since first publication
  works$cit_norm <- works$cited_by_count / works$n_authors
  works <- works %>% arrange(-cit_norm)
  works$h_Ia_position <- 1:nrow(works)

  net_academic_age <- as.integer(format(Sys.Date(), "%Y")) - first_pub_year - academic_age_bonus

  # h_Ia: The hIa-index thus measures the average number of single-author equivalent h-index points that an academic has accumulated in each year of their academic career. A hIa of 1.0 means that an academic has consistently published one article per year that, when corrected for the number of co-authors, has accumulated enough citations to be included in the h-index (https://harzing.com/publications/white-papers/from-h-index-to-hia)
  h_Ia0 <- sum(works$cit_norm > works$h_Ia_position)
  h_Ia <- h_Ia0 / net_academic_age

  return(list(
    net_academic_age=net_academic_age,
    h_index=h_index,
    h_Ia = h_Ia,
    first_publication=first_work,
    academic_age_note = academic_age_note
  ))
}

