# author.id="https://openalex.org/A5022479713"
# author.id="https://openalex.org/A5089676027"

# if you only provide the author.id, it downloads all works of that person.
# If you provide, in addition, a vector of dois, only these are analyzed. (But you always have to provide an author.id).

#' @title Get network analysis of co-authorships for a given author or set of works
#' @description This function performs a network analysis for a given author or set of works.
#'
#' @param author.id The ID of the author for whom the network will be analyzed. Must be provided.
#' @param doi Optional vector of DOIs. If provided, only works with these DOIs will be analyzed.
#' @param verbose Whether to display the result string (default is TRUE).
#' @param min_edges Minimal number of co-authorships. By default set to 2: This way, single co-authorship connections are ignored (e.g., in ManyLabs-style papers, this can lead to a strong inflation of co-authorship connections.)
#'
#' @return A list containing various results of the network analysis, including unique coauthor edges, counts of international and same-country co-authors,
#' country codes of coauthors, and a result string summarizing the analysis.
#'
#' @importFrom data.table rbindlist
#' @importFrom openalexR oa_fetch
#' @importFrom entropy entropy
#' @import dplyr
#' @export

#'
#' @examples
#' # Analyze the network of a specific author
#' get_network(author.id = 'https://openalex.org/A5022479713')
#' # Analyze the network of a specific author for selected works
#' get_network(author.id = 'https://openalex.org/A5022479713', min_edges=1,
#'     doi = c(
#'         'https://doi.org/10.1037/pspp0000428',
#'         'https://doi.org/10.1017/S0033291722003294',
#'         'https://doi.org/10.5964/ps.6029',
#'         'https://doi.org/10.1146/annurev-psych-020821-114157'))

#'
get_network <- function(author.id, doi=NA, min_edges=2, verbose=TRUE) {

  if (all(is.na(doi))) {
    works <- oa_fetch(
      entity = "works",
      author.id = author.id,
      is_paratext = FALSE,
      is_retracted = FALSE,
      abstract=FALSE,
      verbose = verbose
    )
  } else {
    works <- oa_fetch(
      entity = "works",
      doi = doi,
      is_paratext = FALSE,
      is_retracted = FALSE,
      abstract=FALSE,
      verbose = verbose
    )
  }

  # get_all_coauthors
  all_coauthor_edges <- apply(works, 1, function(x) x$author) |> rbindlist()

  # own country code is the most frequent code of that person.
  own_country_codes <- all_coauthor_edges %>% filter(au_id == author.id) %>% pull("institution_country_code")
  own_country_codes_tab <- table(own_country_codes) |> sort(decreasing = TRUE)
  own_country_code <- names(own_country_codes_tab)[1]

  all_coauthor_edges <- all_coauthor_edges %>%
    filter(au_id != author.id)

  # each row is one unique coauthor
  unique_coauthor_edges <- all_coauthor_edges %>%
    group_by(au_id) %>%
    mutate(n_coauthorships = n()) %>%
    slice(1) %>%
    ungroup() %>%
    arrange(-n_coauthorships)

  # each row is one unique coauthor
  # We remove here single co-authorships, as this can be highly inflated by Many-Labs-style papers.
  unique_repeated_coauthor_edges <- unique_coauthor_edges %>%
    filter(n_coauthorships >= min_edges)

  country_codes_repeated <- unique_repeated_coauthor_edges %>%
    count(institution_country_code) %>%
    filter(!is.na(institution_country_code)) %>%
    arrange(-n)

  # some country codes are missing values; take of that!
  n_coauthors_international <- unique_repeated_coauthor_edges %>% filter(institution_country_code != own_country_code) %>% nrow()
  n_coauthors_same_country <- unique_repeated_coauthor_edges %>% filter(institution_country_code == own_country_code) %>% nrow()

  # ignoring NA country codes here
  perc_international <- (n_coauthors_international*100/(n_coauthors_international+n_coauthors_same_country)) |> round()
  perc_same_country <- 100 - perc_international

  # compute entropy index
  if (nrow(country_codes_repeated) > 1) {
    ent.norm <- entropy::entropy(country_codes_repeated$n) / log(nrow(country_codes_repeated))

    n_countries_displayed <- min(4, nrow(country_codes_repeated))
    ent.string <- paste0("The normalized entropy (ranging from 0=only one country to 1=even distribution among all countries) is ", round(ent.norm, 2), ". The ", n_countries_displayed, " countries with the most coauthors are ", paste0(country_codes_repeated[1:n_countries_displayed, 1, drop=TRUE] |> as.vector(), collapse=", "))
  } else {
    ent.norm <- NA
    warning("Only one country present, could not compute entropy index.")
    ent.string <- ""
  }

  result_string <- paste0(nrow(unique_repeated_coauthor_edges), " unique co-authors (n>=", min_edges, " joint papers). ", perc_international, "% from ", nrow(country_codes_repeated)-1, " international countries, ", perc_same_country, "% from the same country. ", ent.string)
  if (verbose == TRUE) {
    print(result_string)
  }

  return(list(
    unique_coauthor_edges=unique_coauthor_edges,
    n_coauthors_international=n_coauthors_international,
    n_coauthors_same_country=n_coauthors_same_country,
    country_codes_repeated=country_codes_repeated,
    result_string=result_string
    ))

}

