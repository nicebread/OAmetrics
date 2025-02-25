#' Retrieve BIP scores for a list of DOIs
#'
#' This function takes a list of DOIs, normalizes them, queries the BIP API to retrieve scores, and returns a data frame with bibliometric indicators.
#'
#' @param dois A character vector of DOIs
#' @param verbose Show diagnostic information?
#' @return A data frame with bibliometric indicators for the provided DOIs
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl_fetch_memory
#' @importFrom utils URLencode
#' @importFrom dplyr arrange
#' @examples
#' dois <- c("https://doi.org/10.1080/00223891.2020.1726936", "10.1016/j.jrp.2013.05.009")
#' get_BIP(dois)
#' @export
get_BIP <- function(dois, verbose=FALSE) {

  dois_normalized <- normalize_dois(dois)
  dois_minimal <- str_extract(dois_normalized, pattern="10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+")

  # we need this doi_df to re-order the results to the original order
  doi_df <- data.frame(index=1:length(dois_minimal), doi=dois_minimal)

  # paging: BIP only returns a maximum of 50 dois per request
  n_pages <- ceiling(length(dois_minimal) / 50)
  BIP <- data.frame()
  for (i in 1:n_pages) {
    start <- ((i-1)*50+1)
    end <- min(i*50, length(dois_minimal))
    if (verbose==TRUE) {
      print(paste0("Retrieving page ", i, "/", n_pages, " (", start, " to ", end, ")"))
    }

    doi_csv <- paste0(dois_minimal[start:end], collapse=",") |> URLencode(reserved=TRUE)
    req <- curl_fetch_memory(paste0("https://bip-api.imsi.athenarc.gr/paper/scores/batch/", doi_csv))

    if (req$status_code %in% c("503", "417")) {
      # no response from BIP API? Return empty object
      BIP <- data.frame(
        doi = dois,
        pop_class = rep(NA, length(dois)),
        inf_class = rep(NA, length(dois)),
        imp_class = rep(NA, length(dois)),
        "3_year_cc" = rep(NA, length(dois))
      )

      if (verbose==TRUE) print(paste0("Warning: BIP request failed with status code ", req$status_code))
    } else {
        BIP0 <- jsonlite::fromJSON(rawToChar(req$content))

        # if necessary: add missing column
        if (is.null(BIP0$msg)) BIP0$msg <- NA
        BIP <- rbind(BIP, BIP0)
      }
    }


  # If no doi could be retrieved, the returned BIP object does not have all
  # columns --> add missing columns to ensure a consistent output
  if (!"pop_class" %in% names(BIP)) {
    NA -> BIP$attrank -> BIP$tar_ram -> BIP$pagerank -> BIP[, "3_year_cc"] ->
      BIP$cc -> BIP$pop_class -> BIP$inf_class -> BIP$imp_class
    BIP <- BIP %>% relocate(msg, .after = last_col())
  }

  BIP$pop_class <- factor(BIP$pop_class, levels=paste0("C", 1:5), ordered=TRUE)
  BIP$inf_class <- factor(BIP$inf_class, levels=paste0("C", 1:5), ordered=TRUE)
  BIP$imp_class <- factor(BIP$imp_class, levels=paste0("C", 1:5), ordered=TRUE)
  colnames(BIP)[5] <- "three_year_cc"

  # fix bug in BIP API: Sometimes additional records are added. Reduce to the actual papers.
  BIP <- BIP[BIP$doi %in% dois_minimal, ]

  # the BIP API orders the dois alphabetically - bring back to original order:
  BIP <- full_join(BIP, doi_df, by="doi") %>%
    arrange(index) %>%
    select(-index)

  if (!all.equal(BIP$doi, dois_minimal)) {
    warning("Error in BIP retrieval: Not all dois have been fetched.")
  }

  BIP$doi <- normalize_dois(BIP$doi)

  return(BIP)
}


#dois <- c("https://doi.org/10.1037/bul0000217", "https://doi.org/10.1016/j.jrp.2016.06.003", "https://doi.org/10.1016/s0160-2896(03)00062-x", "https://doi.org/10.1037/pspp0000093", "https://doi.org/10.1073/pnas.1506451112", "https://doi.org/10.1037/pspi0000118", "https://doi.org/10.1016/j.appet.2018.06.011", "https://doi.org/10.3390/jintelligence6030030", "https://doi.org/10.1038/s41598-018-24139-y", "https://doi.org/10.1037/pspp0000208", "https://doi.org/10.1016/j.chb.2021.107062", "https://doi.org/10.1177/0956797617723726", "https://doi.org/10.1073/pnas.1807942116", "https://doi.org/10.1016/j.intell.2015.11.004", "https://doi.org/10.1037/pspp0000420", "https://doi.org/10.1136/bmjment-2023-300675", "https://doi.org/10.1002/per.2288", "https://doi.org/10.3390/jintelligence8040038", "https://doi.org/10.1037/met0000294", "https://doi.org/10.1037/pspp0000304", "https://doi.org/10.1111/jopy.12621", "https://doi.org/10.1177/13684302221140002", "https://doi.org/10.1037/pspi0000423", "https://doi.org/10.1177/25152459211007368", "https://doi.org/10.3389/fpsyg.2017.02119", "https://doi.org/10.1080/00273171.2018.1491381", "https://doi.org/10.1002/per.2258", "https://doi.org/10.1027/2151-2604/a000481", "https://doi.org/10.1037/pspp0000174", "https://doi.org/10.1027/2151-2604/a000342", "https://doi.org/10.1177/08902070211012923", "https://doi.org/10.1002/per.2145", "https://doi.org/10.1177/0146167220986310", "https://doi.org/10.1037/pspp0000398", "https://doi.org/10.1016/j.obhdp.2018.07.001", "https://doi.org/10.1017/s0033291721000131", "https://doi.org/10.1525/collabra.73236", "https://doi.org/10.1111/psyp.14023", "https://doi.org/10.1073/pnas.2116924119", "https://doi.org/10.1016/j.jrp.2020.104016", "https://doi.org/10.1080/00223891.2022.2043879", "https://doi.org/10.1016/j.jrp.2019.06.001", "https://doi.org/10.1016/j.joep.2021.102445", "https://doi.org/10.1016/j.jesp.2022.104307", "https://doi.org/10.1038/s41598-022-17013-5", "https://doi.org/10.1027/2698-1866/a000024", "https://doi.org/10.1177/09567976221094630", "https://doi.org/10.1111/jasp.12969", "https://doi.org/10.31234/osf.io/f3htz", "https://doi.org/10.1017/s1930297500008950", "https://doi.org/10.1016/j.neubiorev.2021.01.022", "https://doi.org/10.1111/1460-6984.12101", "https://doi.org/10.3389/fpsyt.2020.579985", "https://doi.org/10.1111/1460-6984.12335", "https://doi.org/10.1038/s41380-021-01200-3", "https://doi.org/10.1177/2515245919882903", "https://doi.org/10.3389/fpsyg.2019.01915", "https://doi.org/10.1002/per.2259", "https://doi.org/10.1038/s41597-020-00680-2", "https://doi.org/10.1080/17405629.2021.1952863", "https://doi.org/10.1037/amp0000879", "https://doi.org/10.1037/pspp0000446", "https://doi.org/10.1177/0956797618810000", "https://doi.org/10.1002/eat.23400", "https://doi.org/10.1098/rsos.201494", "https://doi.org/10.1016/j.ejvs.2014.12.002", "https://doi.org/10.1080/23279095.2018.1429441", "https://doi.org/10.1177/0963721419830382", "https://doi.org/10.1098/rstb.2020.0001", "https://doi.org/10.7554/elife.72185", "https://doi.org/10.1037/pspp0000438", "https://doi.org/10.1111/aphw.12364", "https://doi.org/10.1080/1047840x.2022.2065128", "https://doi.org/10.1037/ocp0000340", "https://doi.org/10.1037/spy0000311", "https://doi.org/10.1002/da.22764", "https://doi.org/10.1037/ccp0000550", "https://doi.org/10.1186/s10194-021-01339-y", "https://doi.org/10.1111/bjop.12430", "https://doi.org/10.1037/rev0000069", "https://doi.org/10.1525/collabra.184", "https://doi.org/10.1525/collabra.278", "https://doi.org/10.1037/pspp0000402", "https://doi.org/10.1037/ser0000724", "https://doi.org/10.1007/s12124-020-09516-5", "https://doi.org/10.3390/nu14142927", "https://doi.org/10.1097/j.pain.0000000000002753", "https://doi.org/10.1016/j.jrp.2020.103929", "https://doi.org/10.1093/tbm/ibz150", "https://doi.org/10.1525/collabra.248", "https://doi.org/10.2139/ssrn.3584834", "https://doi.org/10.1177/0033294120926669", "https://doi.org/10.1080/00273171.2022.2119927", "https://doi.org/10.12688/wellcomeopenres.15758.2", "https://doi.org/10.1177/1745691620904775", "https://doi.org/10.1002/per.2270", "https://doi.org/10.1037/pro0000493", "https://doi.org/10.1038/s41584-020-0399-z", "https://doi.org/10.1037/stl0000341", "https://doi.org/10.1525/collabra.38599", "https://doi.org/10.1186/s12966-017-0525-8", "https://doi.org/10.1080/1750984x.2017.1317357", "https://doi.org/10.1136/bjsports-2019-100715", "https://doi.org/10.1007/s11606-021-06737-1", "https://doi.org/10.1177/0956797619830329", "https://doi.org/10.1016/j.im.2019.103174", "https://doi.org/10.1002/nbm.4257", "https://doi.org/10.1037/amp0000622", "https://doi.org/10.1002/ijop.12529", "https://doi.org/10.1080/1612197x.2020.1737836", "https://doi.org/10.1016/j.jad.2020.02.023", "https://doi.org/10.1177/0956797619830326", "https://doi.org/10.1002/per.2113", "https://doi.org/10.1177/2515245919838781", "https://doi.org/10.1037/pspp0000202", "https://doi.org/10.1177/2515245920918872", "https://doi.org/10.1038/s41597-022-01164-1", "https://doi.org/10.1177/0956797618774252", "https://doi.org/10.1177/1088868319845938", "https://doi.org/10.31234/osf.io/km37w", "https://doi.org/10.1177/17456916221075615", "https://doi.org/10.1007/s11136-021-02870-w", "https://doi.org/10.3389/fpsyt.2018.00151", "https://doi.org/10.1371/journal.pone.0231160", "https://doi.org/10.1037/xge0001036", "https://doi.org/10.1097/cco.0000000000000837", "https://doi.org/10.13169/statecrime.4.2.0128", "https://doi.org/10.1016/j.cogpsych.2018.04.002", "https://doi.org/10.1521/soco.2021.39.3.352", "https://doi.org/10.1037/pst0000465", "https://doi.org/10.1037/ser0000720", "https://doi.org/10.1037/vio0000453", "https://doi.org/10.1037/mac0000074", "https://doi.org/10.1080/17461391.2020.1792558", "https://doi.org/10.1080/23311908.2021.2024640", "https://doi.org/10.1016/j.jrp.2022.104297", "https://doi.org/10.3389/fpsyg.2022.838029", "https://doi.org/10.1136/bmjopen-2020-038107", "https://doi.org/10.5964/ps.7177", "https://doi.org/10.1080/02646838.2014.924228", "https://doi.org/10.1080/09540261.2020.1813091", "https://doi.org/10.1016/j.eclinm.2021.101048", "https://doi.org/10.1080/00223891.2021.1955695", "https://doi.org/10.1037/xlm0001195", "https://doi.org/10.1111/jasp.12761", "https://doi.org/10.1080/23743603.2022.2102473", "https://doi.org/10.1016/j.actpsy.2021.103392", "https://doi.org/10.1037/trm0000424", "https://doi.org/10.4103/0975-2870.167993", "https://doi.org/10.1037/npe0000166", "https://doi.org/10.1017/s0140525x20001685", "https://doi.org/10.1038/s41562-019-0561-2", "https://doi.org/10.1089/acm.2020.0415", "https://doi.org/10.1016/j.jrp.2017.10.005", "https://doi.org/10.1186/s12909-020-02232-z", "https://doi.org/10.1037/rev0000318", "https://doi.org/10.1002/jclp.23029", "https://doi.org/10.1016/j.invent.2021.100425", "https://doi.org/10.1037/amp0000742", "https://doi.org/10.1007/s00431-020-03825-y", "https://doi.org/10.1037/dev0001472")
