#' Evenness Index function
#'
#' This function calculates Pielou's evenness index (Pielou, 1966).
#'
#' @param counts A numeric vector of counts representing the number of observations in each category.
#' @param max_cat Maximum number of categories. If less than the number of provided categories, the remaining categories are ignored. If more than the number of provided categories, the missing categories are assumed with count=0.
#' @return The Evenness Index, which combines entropy and richness providing a more holistic measure. `max_cat` can be a number larger than the number of provided category counts. `max_cat` is used as a normalizing factor: It assumes as many existing categories, and missing categories are added and assumed with count=0.
#'
#' @import entropy entropy
#'
#' @examples
#' counts <- c(10, 20, 30, 40)
#' max_cat <- 6
#' evenness_index(counts, max_cat)
#'
#' @export
evenness_index <- function(counts, max_cat) {
  if (length(counts) > max_cat) {
    counts <- counts[1:max_cat]
  } else if (length(counts) < max_cat) {
    # strictly speaking this extension of the vector with zeros is not necessary
    # as the entropy function ignores zero counts anyway.
    counts <- c(counts, rep(0, max_cat - length(counts)))
  }
  H <- entropy::entropy(counts)
  H_max <- log(max_cat)
  H / H_max
}




# author.id="https://openalex.org/A5022479713"  # FS
# author.id="https://openalex.org/A5089676027"  # AG
# author.id="https://openalex.org/A5031368517" # DL

# get_network(author.id = 'https://openalex.org/A5022479713')
# get_network(author.id = 'https://openalex.org/A5089676027')
# get_network(author.id = "https://openalex.org/A5031368517")

#' @title Get network analysis of co-authorships for a given author or set of works
#' @description This function performs a network analysis for a given author or set of works, providing indicators for the degree of international collaborations.
#'
#' @param author.id The ID of the author for whom the network will be analyzed. Must be provided.
#' @param doi Optional vector of DOIs. If provided, only works with these DOIs will be analyzed.
#' @param works Optional data frame of works (that have already been fetched by oa_fetch). Either provide dois or works.
#' @param verbose Whether to display the result string (default is TRUE).
#' @param min_coauthorships Minimal number of co-authorships. By default set to 2: This way, single co-authorship connections are ignored (e.g., in ManyLabs-style papers, this can lead to a strong inflation of co-authorship connections.)
#'
#' @return A list containing various results of the network analysis, including unique coauthor edges, counts of international and same-country co-authors,
#' country codes of coauthors, and a result string summarizing the analysis.
#' It also computes the normalized Shannon entropy. It is normalized by dividing
#' it by log_2(n_countries), which bounds its range to \[0; 1\].
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
#' get_network(author.id = 'https://openalex.org/A5022479713',
#'     doi = c(
#'         'https://doi.org/10.1037/pspp0000428',
#'         'https://doi.org/10.1017/S0033291722003294',
#'         'https://doi.org/10.5964/ps.6029',
#'         'https://doi.org/10.1146/annurev-psych-020821-114157'))

#'
get_network <- function(author.id, doi=NA, works=NA, min_coauthorships=2, verbose=TRUE) {

  # fetch all works from an author ID
  if (all(is.na(doi)) & all(is.na(works))) {
    works <- oa_fetch(
      entity = "works",
      author.id = author.id,
      is_paratext = FALSE,
      is_retracted = FALSE,
      abstract=FALSE,
      verbose = verbose
    )
  }

  # fetch works from given dois
  if (!all(is.na(doi)) & all(is.na(works))) {
    works <- oa_fetch(
      entity = "works",
      doi = doi,
      is_paratext = FALSE,
      is_retracted = FALSE,
      abstract=TRUE,
      verbose = verbose
    )
  }

  # use the provided works (that already have been fetched from OpenAlex)
  if (all(is.na(doi)) & !all(is.na(works))) {
   # works = works
    if (verbose == TRUE) {
      print("Using the provided works.")
    }
  }

  #----------------------------------------------------------------------------
  # Analysis of co-authorships: Internationalization
  #----------------------------------------------------------------------------

  # get_all_coauthors that have not NA as institution_country_code
  all_edges <- apply(works, 1, function(x) x$author[, c("au_id", "institution_country_code")]) |>
    rbindlist() |>
    as.data.frame() |>
    dplyr::filter(!is.na(institution_country_code))

  # own country code is the most frequent code of that person.
  own_country_codes <- all_edges %>% filter(au_id == author.id) %>% pull("institution_country_code")
  own_country_codes_tab <- table(own_country_codes) |> sort(decreasing = TRUE)
  own_country_code <- names(own_country_codes_tab)[1]

  if (is.null(own_country_code)) {
    get_mode <- function(x) {
      names(sort(table(x), decreasing = TRUE))[1]
    }

    own_country_code <- get_mode(all_edges$institution_country_code)

    warning(paste0("Could not determine the own country code of the applicant - defaulting to the majority of existing country codes, which is ", own_country_code))
  }

  all_coauthor_edges <- all_edges %>%
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
    filter(n_coauthorships >= min_coauthorships)

  country_codes_repeated <- unique_repeated_coauthor_edges %>%
    count(institution_country_code) %>%
    filter(!is.na(institution_country_code)) %>%
    arrange(-n)

  n_coauthors_international <- unique_repeated_coauthor_edges %>% filter(institution_country_code != own_country_code) %>% nrow()
  n_coauthors_same_country <- unique_repeated_coauthor_edges %>% filter(institution_country_code == own_country_code) %>% nrow()

  perc_international <- (n_coauthors_international*100/(n_coauthors_international+n_coauthors_same_country)) |> round()
  perc_same_country <- 100 - perc_international

  # compute internationalization evenness index, with a maximum of 10 countries
  if (nrow(country_codes_repeated) > 1) {

    max_countries <- 10
    international.evenness <- evenness_index(country_codes_repeated$n[1:min(nrow(country_codes_repeated), max_countries)], max_countries)

    n_countries_displayed <- min(4, nrow(country_codes_repeated))

    country_string <- ""
    for (i in 1:n_countries_displayed) {
      country_string <- paste0(country_string, country_codes_repeated$institution_country_code[i], " (", country_codes_repeated$n[i], ")")

      if (i < n_countries_displayed) {
        country_string <- paste0(country_string, ", ")
      }
    }

    ent.international.string <- paste0("The evenness (ranging from 0=only one country to 1=even distribution among all countries) is ", round(international.evenness, 2), ". The ", n_countries_displayed, " countries with the most coauthors are: ", country_string, ".")
  } else {
    international.evenness <- 0
    warning("Only one country present, setting evenness index to 0.")
    ent.international.string <- paste0("The evenness (ranging from 0=only one country to 1=even distribution among all countries) is ", round(international.evenness, 2), ".")
  }

  internationalization_string <- paste0(nrow(unique_repeated_coauthor_edges), " unique identifiable co-authors with at least ", min_coauthorships, " joint papers; ", perc_international, "% from ", nrow(country_codes_repeated)-1, " international countries, ", perc_same_country, "% from the same country. ", ent.international.string)
  if (verbose == TRUE) {
    print(internationalization_string)
  }


  #----------------------------------------------------------------------------
  # Analysis of top concepts: Interdisciplinarity
  #----------------------------------------------------------------------------

  # return the primary fields of the works (Level 2)
  primary_fields <- apply(works, 1, function(x) {
      topics <- x$topics |> as.data.frame()
      topics <- topics[topics$i == 1 & topics$name == "field", ]
      return(topics$display_name)
    }) |> unlist()

  # return all subfields of the works (Level 3)
  subfields <- apply(works, 1, function(x) {
    sf <- x$topics |> as.data.frame()
    sf <- sf[sf$name == "subfield", ]
    return(sf$display_name)
  }) |> unlist() |> as.vector()

  # return all topics of the works (Level 4)
  topics <- apply(works, 1, function(x) {
    sf <- x$topics |> as.data.frame()
    sf <- sf[sf$name == "topic", ]
    return(sf$display_name)
  }) |> unlist() |> as.vector()

  # manual adjustment:
  primary_fields[primary_fields=="Social Sciences"] <- "Psychology"
  primary_fields_tab <- table(primary_fields) |> sort(decreasing=TRUE) |> as.data.frame()

  if (ncol(primary_fields_tab) == 1) {
    primary_fields_tab2 <- cbind(primary_field = rownames(primary_fields_tab), primary_fields_tab)
    primary_fields_tab <- primary_fields_tab2
  }

  colnames(primary_fields_tab) <- c("primary_field", "n")

  subfields_tab <- table(subfields) |> sort(decreasing=TRUE) |> as.data.frame()
  colnames(subfields_tab) <- c("subfield", "n")

  topics_tab <- table(topics) |> sort(decreasing=TRUE) |> as.data.frame()
  colnames(topics_tab) <- c("topic", "n")

  # only keep topics that show up in at least 5% of all papers (to remove some wrongly assigned topics)
  primary_fields_tab_reduced <- primary_fields_tab[primary_fields_tab$n / sum(primary_fields_tab$n) > .03, ]

  # compute evenness index, with a maximum of 6 fields
  if (nrow(primary_fields_tab_reduced) > 1) {

    max_fields <- 6
    interdisc.evenness <- evenness_index(primary_fields_tab_reduced$n[1:min(nrow(primary_fields_tab_reduced), max_fields)], max_fields)

    ent.interdisc.string <- paste0("The evenness (ranging from 0=only one field to 1=even distribution among all fields) is ", round(interdisc.evenness, 2), ".")
  } else {
    interdisc.evenness <- 0
    warning("Only one field present, setting evenness index to 0.")
    ent.interdisc.string <- paste0("The evenness (ranging from 0=only one field to 1=even distribution among all fields) is ", round(interdisc.evenness, 2), ".")
  }


  field_string <- ""
  for (i in 1:nrow(primary_fields_tab_reduced)) {
    field_string <- paste0(field_string, primary_fields_tab_reduced$primary_field[i], " (", primary_fields_tab_reduced$n[i], ")")

    if (i < nrow(primary_fields_tab_reduced)) {
      field_string <- paste0(field_string, ", ")
    }
  }



  interdisc_string <- paste0(nrow(primary_fields_tab_reduced), " primary fields: ", field_string, ". ", ent.interdisc.string)
  if (verbose == TRUE) {
    print(interdisc_string)
  }


  #----------------------------------------------------------------------------
  # Return object
  #----------------------------------------------------------------------------

  return(list(
    unique_coauthor_edges=unique_coauthor_edges,
    n_coauthors_international=n_coauthors_international,
    n_coauthors_same_country=n_coauthors_same_country,
    international_evenness = international.evenness,
    country_codes_repeated=country_codes_repeated,
    internationalization_string=internationalization_string,

    interdisc_evenness = interdisc.evenness,
    primary_fields_tab = primary_fields_tab,
    primary_fields_tab_reduced = primary_fields_tab_reduced,
    subfields_tab = subfields_tab,
    topics_tab = topics_tab,
    interdisc_string = interdisc_string
    ))

}

