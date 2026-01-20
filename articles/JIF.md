# JIF

``` r
knitr::opts_chunk$set(cache = TRUE)
```

``` r
library(OAmetrics)
```

    ## Loading required package: data.table

    ## This is OAmetrics 0.5.2

    ## OAmetrics is BETA software! Please report any bugs.

``` r
# Scientific Reports. According to website the JIF is 3.8 in 2023
res <- data.frame()
for (l in c(100, 500, 1000, 5000, 10000)) {
  for (r in 1:20) {
    start <- Sys.time()
    res0 <- get_JIF(issn="2045-2322", year=2023, limit=l, seed=r)
    end <- Sys.time()
    res0$dur <- end-start
    res0$limit <- l
    res <- rbind(res, res0)
    print(res)
    flush.console()
  }
}
```

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##              journal      issn year paper_limit total_citations citable_items
    ## 1 Scientific Reports 2045-2322 2023         100             448           100
    ##    JIF           dur limit
    ## 1 4.48 3.504405 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##              journal      issn year paper_limit total_citations citable_items
    ## 1 Scientific Reports 2045-2322 2023         100             448           100
    ## 2 Scientific Reports 2045-2322 2023         100             415           100
    ##    JIF           dur limit
    ## 1 4.48 3.504405 secs   100
    ## 2 4.15 1.186225 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##              journal      issn year paper_limit total_citations citable_items
    ## 1 Scientific Reports 2045-2322 2023         100             448           100
    ## 2 Scientific Reports 2045-2322 2023         100             415           100
    ## 3 Scientific Reports 2045-2322 2023         100             470           100
    ##    JIF           dur limit
    ## 1 4.48 3.504405 secs   100
    ## 2 4.15 1.186225 secs   100
    ## 3 4.70 1.597603 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##              journal      issn year paper_limit total_citations citable_items
    ## 1 Scientific Reports 2045-2322 2023         100             448           100
    ## 2 Scientific Reports 2045-2322 2023         100             415           100
    ## 3 Scientific Reports 2045-2322 2023         100             470           100
    ## 4 Scientific Reports 2045-2322 2023         100             534           100
    ##    JIF           dur limit
    ## 1 4.48 3.504405 secs   100
    ## 2 4.15 1.186225 secs   100
    ## 3 4.70 1.597603 secs   100
    ## 4 5.34 4.161026 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##              journal      issn year paper_limit total_citations citable_items
    ## 1 Scientific Reports 2045-2322 2023         100             448           100
    ## 2 Scientific Reports 2045-2322 2023         100             415           100
    ## 3 Scientific Reports 2045-2322 2023         100             470           100
    ## 4 Scientific Reports 2045-2322 2023         100             534           100
    ## 5 Scientific Reports 2045-2322 2023         100             460           100
    ##    JIF           dur limit
    ## 1 4.48 3.504405 secs   100
    ## 2 4.15 1.186225 secs   100
    ## 3 4.70 1.597603 secs   100
    ## 4 5.34 4.161026 secs   100
    ## 5 4.60 1.265880 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##              journal      issn year paper_limit total_citations citable_items
    ## 1 Scientific Reports 2045-2322 2023         100             448           100
    ## 2 Scientific Reports 2045-2322 2023         100             415           100
    ## 3 Scientific Reports 2045-2322 2023         100             470           100
    ## 4 Scientific Reports 2045-2322 2023         100             534           100
    ## 5 Scientific Reports 2045-2322 2023         100             460           100
    ## 6 Scientific Reports 2045-2322 2023         100             457           100
    ##    JIF           dur limit
    ## 1 4.48 3.504405 secs   100
    ## 2 4.15 1.186225 secs   100
    ## 3 4.70 1.597603 secs   100
    ## 4 5.34 4.161026 secs   100
    ## 5 4.60 1.265880 secs   100
    ## 6 4.57 1.141317 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##              journal      issn year paper_limit total_citations citable_items
    ## 1 Scientific Reports 2045-2322 2023         100             448           100
    ## 2 Scientific Reports 2045-2322 2023         100             415           100
    ## 3 Scientific Reports 2045-2322 2023         100             470           100
    ## 4 Scientific Reports 2045-2322 2023         100             534           100
    ## 5 Scientific Reports 2045-2322 2023         100             460           100
    ## 6 Scientific Reports 2045-2322 2023         100             457           100
    ## 7 Scientific Reports 2045-2322 2023         100             575           100
    ##    JIF           dur limit
    ## 1 4.48 3.504405 secs   100
    ## 2 4.15 1.186225 secs   100
    ## 3 4.70 1.597603 secs   100
    ## 4 5.34 4.161026 secs   100
    ## 5 4.60 1.265880 secs   100
    ## 6 4.57 1.141317 secs   100
    ## 7 5.75 1.259543 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##              journal      issn year paper_limit total_citations citable_items
    ## 1 Scientific Reports 2045-2322 2023         100             448           100
    ## 2 Scientific Reports 2045-2322 2023         100             415           100
    ## 3 Scientific Reports 2045-2322 2023         100             470           100
    ## 4 Scientific Reports 2045-2322 2023         100             534           100
    ## 5 Scientific Reports 2045-2322 2023         100             460           100
    ## 6 Scientific Reports 2045-2322 2023         100             457           100
    ## 7 Scientific Reports 2045-2322 2023         100             575           100
    ## 8 Scientific Reports 2045-2322 2023         100             470           100
    ##    JIF           dur limit
    ## 1 4.48 3.504405 secs   100
    ## 2 4.15 1.186225 secs   100
    ## 3 4.70 1.597603 secs   100
    ## 4 5.34 4.161026 secs   100
    ## 5 4.60 1.265880 secs   100
    ## 6 4.57 1.141317 secs   100
    ## 7 5.75 1.259543 secs   100
    ## 8 4.70 2.209416 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##              journal      issn year paper_limit total_citations citable_items
    ## 1 Scientific Reports 2045-2322 2023         100             448           100
    ## 2 Scientific Reports 2045-2322 2023         100             415           100
    ## 3 Scientific Reports 2045-2322 2023         100             470           100
    ## 4 Scientific Reports 2045-2322 2023         100             534           100
    ## 5 Scientific Reports 2045-2322 2023         100             460           100
    ## 6 Scientific Reports 2045-2322 2023         100             457           100
    ## 7 Scientific Reports 2045-2322 2023         100             575           100
    ## 8 Scientific Reports 2045-2322 2023         100             470           100
    ## 9 Scientific Reports 2045-2322 2023         100             562           100
    ##    JIF           dur limit
    ## 1 4.48 3.504405 secs   100
    ## 2 4.15 1.186225 secs   100
    ## 3 4.70 1.597603 secs   100
    ## 4 5.34 4.161026 secs   100
    ## 5 4.60 1.265880 secs   100
    ## 6 4.57 1.141317 secs   100
    ## 7 5.75 1.259543 secs   100
    ## 8 4.70 2.209416 secs   100
    ## 9 5.62 1.265974 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100
    ## 12 3.87 1.508438 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100
    ## 12 3.87 1.508438 secs   100
    ## 13 4.52 1.074733 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100
    ## 12 3.87 1.508438 secs   100
    ## 13 4.52 1.074733 secs   100
    ## 14 4.49 1.147448 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100
    ## 12 3.87 1.508438 secs   100
    ## 13 4.52 1.074733 secs   100
    ## 14 4.49 1.147448 secs   100
    ## 15 3.81 1.443572 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100
    ## 12 3.87 1.508438 secs   100
    ## 13 4.52 1.074733 secs   100
    ## 14 4.49 1.147448 secs   100
    ## 15 3.81 1.443572 secs   100
    ## 16 4.54 2.226332 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100
    ## 12 3.87 1.508438 secs   100
    ## 13 4.52 1.074733 secs   100
    ## 14 4.49 1.147448 secs   100
    ## 15 3.81 1.443572 secs   100
    ## 16 4.54 2.226332 secs   100
    ## 17 4.78 1.058276 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100
    ## 12 3.87 1.508438 secs   100
    ## 13 4.52 1.074733 secs   100
    ## 14 4.49 1.147448 secs   100
    ## 15 3.81 1.443572 secs   100
    ## 16 4.54 2.226332 secs   100
    ## 17 4.78 1.058276 secs   100
    ## 18 4.20 1.168020 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100
    ## 12 3.87 1.508438 secs   100
    ## 13 4.52 1.074733 secs   100
    ## 14 4.49 1.147448 secs   100
    ## 15 3.81 1.443572 secs   100
    ## 16 4.54 2.226332 secs   100
    ## 17 4.78 1.058276 secs   100
    ## 18 4.20 1.168020 secs   100
    ## 19 4.37 1.521522 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=100
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ##     JIF           dur limit
    ## 1  4.48 3.504405 secs   100
    ## 2  4.15 1.186225 secs   100
    ## 3  4.70 1.597603 secs   100
    ## 4  5.34 4.161026 secs   100
    ## 5  4.60 1.265880 secs   100
    ## 6  4.57 1.141317 secs   100
    ## 7  5.75 1.259543 secs   100
    ## 8  4.70 2.209416 secs   100
    ## 9  5.62 1.265974 secs   100
    ## 10 4.61 1.104112 secs   100
    ## 11 5.06 1.330426 secs   100
    ## 12 3.87 1.508438 secs   100
    ## 13 4.52 1.074733 secs   100
    ## 14 4.49 1.147448 secs   100
    ## 15 3.81 1.443572 secs   100
    ## 16 4.54 2.226332 secs   100
    ## 17 4.78 1.058276 secs   100
    ## 18 4.20 1.168020 secs   100
    ## 19 4.37 1.521522 secs   100
    ## 20 5.57 7.097642 secs   100

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=500
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000
    ## 52 5.292 5.674263 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000
    ## 52 5.292 5.674263 secs  1000
    ## 53 4.604 4.200588 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000
    ## 52 5.292 5.674263 secs  1000
    ## 53 4.604 4.200588 secs  1000
    ## 54 6.269 4.222737 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000
    ## 52 5.292 5.674263 secs  1000
    ## 53 4.604 4.200588 secs  1000
    ## 54 6.269 4.222737 secs  1000
    ## 55 4.378 4.966535 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000
    ## 52 5.292 5.674263 secs  1000
    ## 53 4.604 4.200588 secs  1000
    ## 54 6.269 4.222737 secs  1000
    ## 55 4.378 4.966535 secs  1000
    ## 56 5.049 4.800193 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000
    ## 52 5.292 5.674263 secs  1000
    ## 53 4.604 4.200588 secs  1000
    ## 54 6.269 4.222737 secs  1000
    ## 55 4.378 4.966535 secs  1000
    ## 56 5.049 4.800193 secs  1000
    ## 57 4.802 4.925235 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000
    ## 52 5.292 5.674263 secs  1000
    ## 53 4.604 4.200588 secs  1000
    ## 54 6.269 4.222737 secs  1000
    ## 55 4.378 4.966535 secs  1000
    ## 56 5.049 4.800193 secs  1000
    ## 57 4.802 4.925235 secs  1000
    ## 58 4.530 5.492424 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000
    ## 52 5.292 5.674263 secs  1000
    ## 53 4.604 4.200588 secs  1000
    ## 54 6.269 4.222737 secs  1000
    ## 55 4.378 4.966535 secs  1000
    ## 56 5.049 4.800193 secs  1000
    ## 57 4.802 4.925235 secs  1000
    ## 58 4.530 5.492424 secs  1000
    ## 59 5.052 4.308994 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=1000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ##      JIF           dur limit
    ## 1  4.480 3.504405 secs   100
    ## 2  4.150 1.186225 secs   100
    ## 3  4.700 1.597603 secs   100
    ## 4  5.340 4.161026 secs   100
    ## 5  4.600 1.265880 secs   100
    ## 6  4.570 1.141317 secs   100
    ## 7  5.750 1.259543 secs   100
    ## 8  4.700 2.209416 secs   100
    ## 9  5.620 1.265974 secs   100
    ## 10 4.610 1.104112 secs   100
    ## 11 5.060 1.330426 secs   100
    ## 12 3.870 1.508438 secs   100
    ## 13 4.520 1.074733 secs   100
    ## 14 4.490 1.147448 secs   100
    ## 15 3.810 1.443572 secs   100
    ## 16 4.540 2.226332 secs   100
    ## 17 4.780 1.058276 secs   100
    ## 18 4.200 1.168020 secs   100
    ## 19 4.370 1.521522 secs   100
    ## 20 5.570 7.097642 secs   100
    ## 21 5.024 2.231331 secs   500
    ## 22 4.522 2.421490 secs   500
    ## 23 4.622 5.640908 secs   500
    ## 24 4.662 2.270812 secs   500
    ## 25 5.222 3.537735 secs   500
    ## 26 5.362 2.434339 secs   500
    ## 27 4.886 3.784517 secs   500
    ## 28 6.330 2.606812 secs   500
    ## 29 5.222 2.302237 secs   500
    ## 30 4.884 2.516571 secs   500
    ## 31 4.752 3.198443 secs   500
    ## 32 5.264 5.310478 secs   500
    ## 33 4.434 4.225231 secs   500
    ## 34 7.636 2.920446 secs   500
    ## 35 4.402 2.432822 secs   500
    ## 36 4.866 3.362033 secs   500
    ## 37 4.816 3.645479 secs   500
    ## 38 4.510 2.902116 secs   500
    ## 39 5.082 2.379958 secs   500
    ## 40 5.136 2.466991 secs   500
    ## 41 5.089 3.757754 secs  1000
    ## 42 4.798 4.713679 secs  1000
    ## 43 4.863 4.250989 secs  1000
    ## 44 4.765 4.395717 secs  1000
    ## 45 5.140 6.544450 secs  1000
    ## 46 5.071 3.706580 secs  1000
    ## 47 4.801 5.083369 secs  1000
    ## 48 5.482 5.167562 secs  1000
    ## 49 5.184 3.709071 secs  1000
    ## 50 4.918 4.828439 secs  1000
    ## 51 4.886 4.066140 secs  1000
    ## 52 5.292 5.674263 secs  1000
    ## 53 4.604 4.200588 secs  1000
    ## 54 6.269 4.222737 secs  1000
    ## 55 4.378 4.966535 secs  1000
    ## 56 5.049 4.800193 secs  1000
    ## 57 4.802 4.925235 secs  1000
    ## 58 4.530 5.492424 secs  1000
    ## 59 5.052 4.308994 secs  1000
    ## 60 4.664 5.170154 secs  1000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ##      JIF            dur limit
    ## 1  4.480  3.504405 secs   100
    ## 2  4.150  1.186225 secs   100
    ## 3  4.700  1.597603 secs   100
    ## 4  5.340  4.161026 secs   100
    ## 5  4.600  1.265880 secs   100
    ## 6  4.570  1.141317 secs   100
    ## 7  5.750  1.259543 secs   100
    ## 8  4.700  2.209416 secs   100
    ## 9  5.620  1.265974 secs   100
    ## 10 4.610  1.104112 secs   100
    ## 11 5.060  1.330426 secs   100
    ## 12 3.870  1.508438 secs   100
    ## 13 4.520  1.074733 secs   100
    ## 14 4.490  1.147448 secs   100
    ## 15 3.810  1.443572 secs   100
    ## 16 4.540  2.226332 secs   100
    ## 17 4.780  1.058276 secs   100
    ## 18 4.200  1.168020 secs   100
    ## 19 4.370  1.521522 secs   100
    ## 20 5.570  7.097642 secs   100
    ## 21 5.024  2.231331 secs   500
    ## 22 4.522  2.421490 secs   500
    ## 23 4.622  5.640908 secs   500
    ## 24 4.662  2.270812 secs   500
    ## 25 5.222  3.537735 secs   500
    ## 26 5.362  2.434339 secs   500
    ## 27 4.886  3.784517 secs   500
    ## 28 6.330  2.606812 secs   500
    ## 29 5.222  2.302237 secs   500
    ## 30 4.884  2.516571 secs   500
    ## 31 4.752  3.198443 secs   500
    ## 32 5.264  5.310478 secs   500
    ## 33 4.434  4.225231 secs   500
    ## 34 7.636  2.920446 secs   500
    ## 35 4.402  2.432822 secs   500
    ## 36 4.866  3.362033 secs   500
    ## 37 4.816  3.645479 secs   500
    ## 38 4.510  2.902116 secs   500
    ## 39 5.082  2.379958 secs   500
    ## 40 5.136  2.466991 secs   500
    ## 41 5.089  3.757754 secs  1000
    ## 42 4.798  4.713679 secs  1000
    ## 43 4.863  4.250989 secs  1000
    ## 44 4.765  4.395717 secs  1000
    ## 45 5.140  6.544450 secs  1000
    ## 46 5.071  3.706580 secs  1000
    ## 47 4.801  5.083369 secs  1000
    ## 48 5.482  5.167562 secs  1000
    ## 49 5.184  3.709071 secs  1000
    ## 50 4.918  4.828439 secs  1000
    ## 51 4.886  4.066140 secs  1000
    ## 52 5.292  5.674263 secs  1000
    ## 53 4.604  4.200588 secs  1000
    ## 54 6.269  4.222737 secs  1000
    ## 55 4.378  4.966535 secs  1000
    ## 56 5.049  4.800193 secs  1000
    ## 57 4.802  4.925235 secs  1000
    ## 58 4.530  5.492424 secs  1000
    ## 59 5.052  4.308994 secs  1000
    ## 60 4.664  5.170154 secs  1000
    ## 61 4.849 21.959305 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=5000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91 Scientific Reports 2045-2322 2023       10000           49758         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000
    ## 91 4.9758 33.700685 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91 Scientific Reports 2045-2322 2023       10000           49758         10000
    ## 92 Scientific Reports 2045-2322 2023       10000           50263         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000
    ## 91 4.9758 33.700685 secs 10000
    ## 92 5.0263 38.238195 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91 Scientific Reports 2045-2322 2023       10000           49758         10000
    ## 92 Scientific Reports 2045-2322 2023       10000           50263         10000
    ## 93 Scientific Reports 2045-2322 2023       10000           49128         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000
    ## 91 4.9758 33.700685 secs 10000
    ## 92 5.0263 38.238195 secs 10000
    ## 93 4.9128 34.181886 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91 Scientific Reports 2045-2322 2023       10000           49758         10000
    ## 92 Scientific Reports 2045-2322 2023       10000           50263         10000
    ## 93 Scientific Reports 2045-2322 2023       10000           49128         10000
    ## 94 Scientific Reports 2045-2322 2023       10000           49159         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000
    ## 91 4.9758 33.700685 secs 10000
    ## 92 5.0263 38.238195 secs 10000
    ## 93 4.9128 34.181886 secs 10000
    ## 94 4.9159 34.581962 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91 Scientific Reports 2045-2322 2023       10000           49758         10000
    ## 92 Scientific Reports 2045-2322 2023       10000           50263         10000
    ## 93 Scientific Reports 2045-2322 2023       10000           49128         10000
    ## 94 Scientific Reports 2045-2322 2023       10000           49159         10000
    ## 95 Scientific Reports 2045-2322 2023       10000           49722         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000
    ## 91 4.9758 33.700685 secs 10000
    ## 92 5.0263 38.238195 secs 10000
    ## 93 4.9128 34.181886 secs 10000
    ## 94 4.9159 34.581962 secs 10000
    ## 95 4.9722 32.729328 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91 Scientific Reports 2045-2322 2023       10000           49758         10000
    ## 92 Scientific Reports 2045-2322 2023       10000           50263         10000
    ## 93 Scientific Reports 2045-2322 2023       10000           49128         10000
    ## 94 Scientific Reports 2045-2322 2023       10000           49159         10000
    ## 95 Scientific Reports 2045-2322 2023       10000           49722         10000
    ## 96 Scientific Reports 2045-2322 2023       10000           49862         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000
    ## 91 4.9758 33.700685 secs 10000
    ## 92 5.0263 38.238195 secs 10000
    ## 93 4.9128 34.181886 secs 10000
    ## 94 4.9159 34.581962 secs 10000
    ## 95 4.9722 32.729328 secs 10000
    ## 96 4.9862 41.330060 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91 Scientific Reports 2045-2322 2023       10000           49758         10000
    ## 92 Scientific Reports 2045-2322 2023       10000           50263         10000
    ## 93 Scientific Reports 2045-2322 2023       10000           49128         10000
    ## 94 Scientific Reports 2045-2322 2023       10000           49159         10000
    ## 95 Scientific Reports 2045-2322 2023       10000           49722         10000
    ## 96 Scientific Reports 2045-2322 2023       10000           49862         10000
    ## 97 Scientific Reports 2045-2322 2023       10000           50252         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000
    ## 91 4.9758 33.700685 secs 10000
    ## 92 5.0263 38.238195 secs 10000
    ## 93 4.9128 34.181886 secs 10000
    ## 94 4.9159 34.581962 secs 10000
    ## 95 4.9722 32.729328 secs 10000
    ## 96 4.9862 41.330060 secs 10000
    ## 97 5.0252 38.702208 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91 Scientific Reports 2045-2322 2023       10000           49758         10000
    ## 92 Scientific Reports 2045-2322 2023       10000           50263         10000
    ## 93 Scientific Reports 2045-2322 2023       10000           49128         10000
    ## 94 Scientific Reports 2045-2322 2023       10000           49159         10000
    ## 95 Scientific Reports 2045-2322 2023       10000           49722         10000
    ## 96 Scientific Reports 2045-2322 2023       10000           49862         10000
    ## 97 Scientific Reports 2045-2322 2023       10000           50252         10000
    ## 98 Scientific Reports 2045-2322 2023       10000           48649         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000
    ## 91 4.9758 33.700685 secs 10000
    ## 92 5.0263 38.238195 secs 10000
    ## 93 4.9128 34.181886 secs 10000
    ## 94 4.9159 34.581962 secs 10000
    ## 95 4.9722 32.729328 secs 10000
    ## 96 4.9862 41.330060 secs 10000
    ## 97 5.0252 38.702208 secs 10000
    ## 98 4.8649 32.527321 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##               journal      issn year paper_limit total_citations citable_items
    ## 1  Scientific Reports 2045-2322 2023         100             448           100
    ## 2  Scientific Reports 2045-2322 2023         100             415           100
    ## 3  Scientific Reports 2045-2322 2023         100             470           100
    ## 4  Scientific Reports 2045-2322 2023         100             534           100
    ## 5  Scientific Reports 2045-2322 2023         100             460           100
    ## 6  Scientific Reports 2045-2322 2023         100             457           100
    ## 7  Scientific Reports 2045-2322 2023         100             575           100
    ## 8  Scientific Reports 2045-2322 2023         100             470           100
    ## 9  Scientific Reports 2045-2322 2023         100             562           100
    ## 10 Scientific Reports 2045-2322 2023         100             461           100
    ## 11 Scientific Reports 2045-2322 2023         100             506           100
    ## 12 Scientific Reports 2045-2322 2023         100             387           100
    ## 13 Scientific Reports 2045-2322 2023         100             452           100
    ## 14 Scientific Reports 2045-2322 2023         100             449           100
    ## 15 Scientific Reports 2045-2322 2023         100             381           100
    ## 16 Scientific Reports 2045-2322 2023         100             454           100
    ## 17 Scientific Reports 2045-2322 2023         100             478           100
    ## 18 Scientific Reports 2045-2322 2023         100             420           100
    ## 19 Scientific Reports 2045-2322 2023         100             437           100
    ## 20 Scientific Reports 2045-2322 2023         100             557           100
    ## 21 Scientific Reports 2045-2322 2023         500            2512           500
    ## 22 Scientific Reports 2045-2322 2023         500            2261           500
    ## 23 Scientific Reports 2045-2322 2023         500            2311           500
    ## 24 Scientific Reports 2045-2322 2023         500            2331           500
    ## 25 Scientific Reports 2045-2322 2023         500            2611           500
    ## 26 Scientific Reports 2045-2322 2023         500            2681           500
    ## 27 Scientific Reports 2045-2322 2023         500            2443           500
    ## 28 Scientific Reports 2045-2322 2023         500            3165           500
    ## 29 Scientific Reports 2045-2322 2023         500            2611           500
    ## 30 Scientific Reports 2045-2322 2023         500            2442           500
    ## 31 Scientific Reports 2045-2322 2023         500            2376           500
    ## 32 Scientific Reports 2045-2322 2023         500            2632           500
    ## 33 Scientific Reports 2045-2322 2023         500            2217           500
    ## 34 Scientific Reports 2045-2322 2023         500            3818           500
    ## 35 Scientific Reports 2045-2322 2023         500            2201           500
    ## 36 Scientific Reports 2045-2322 2023         500            2433           500
    ## 37 Scientific Reports 2045-2322 2023         500            2408           500
    ## 38 Scientific Reports 2045-2322 2023         500            2255           500
    ## 39 Scientific Reports 2045-2322 2023         500            2541           500
    ## 40 Scientific Reports 2045-2322 2023         500            2568           500
    ## 41 Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42 Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43 Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44 Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45 Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46 Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47 Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48 Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49 Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50 Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51 Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52 Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53 Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54 Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55 Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56 Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57 Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58 Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59 Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60 Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61 Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62 Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63 Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64 Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65 Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66 Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67 Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68 Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69 Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70 Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71 Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72 Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73 Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74 Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75 Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76 Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77 Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78 Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79 Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80 Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81 Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82 Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83 Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84 Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85 Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86 Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87 Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88 Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89 Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90 Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91 Scientific Reports 2045-2322 2023       10000           49758         10000
    ## 92 Scientific Reports 2045-2322 2023       10000           50263         10000
    ## 93 Scientific Reports 2045-2322 2023       10000           49128         10000
    ## 94 Scientific Reports 2045-2322 2023       10000           49159         10000
    ## 95 Scientific Reports 2045-2322 2023       10000           49722         10000
    ## 96 Scientific Reports 2045-2322 2023       10000           49862         10000
    ## 97 Scientific Reports 2045-2322 2023       10000           50252         10000
    ## 98 Scientific Reports 2045-2322 2023       10000           48649         10000
    ## 99 Scientific Reports 2045-2322 2023       10000           48225         10000
    ##       JIF            dur limit
    ## 1  4.4800  3.504405 secs   100
    ## 2  4.1500  1.186225 secs   100
    ## 3  4.7000  1.597603 secs   100
    ## 4  5.3400  4.161026 secs   100
    ## 5  4.6000  1.265880 secs   100
    ## 6  4.5700  1.141317 secs   100
    ## 7  5.7500  1.259543 secs   100
    ## 8  4.7000  2.209416 secs   100
    ## 9  5.6200  1.265974 secs   100
    ## 10 4.6100  1.104112 secs   100
    ## 11 5.0600  1.330426 secs   100
    ## 12 3.8700  1.508438 secs   100
    ## 13 4.5200  1.074733 secs   100
    ## 14 4.4900  1.147448 secs   100
    ## 15 3.8100  1.443572 secs   100
    ## 16 4.5400  2.226332 secs   100
    ## 17 4.7800  1.058276 secs   100
    ## 18 4.2000  1.168020 secs   100
    ## 19 4.3700  1.521522 secs   100
    ## 20 5.5700  7.097642 secs   100
    ## 21 5.0240  2.231331 secs   500
    ## 22 4.5220  2.421490 secs   500
    ## 23 4.6220  5.640908 secs   500
    ## 24 4.6620  2.270812 secs   500
    ## 25 5.2220  3.537735 secs   500
    ## 26 5.3620  2.434339 secs   500
    ## 27 4.8860  3.784517 secs   500
    ## 28 6.3300  2.606812 secs   500
    ## 29 5.2220  2.302237 secs   500
    ## 30 4.8840  2.516571 secs   500
    ## 31 4.7520  3.198443 secs   500
    ## 32 5.2640  5.310478 secs   500
    ## 33 4.4340  4.225231 secs   500
    ## 34 7.6360  2.920446 secs   500
    ## 35 4.4020  2.432822 secs   500
    ## 36 4.8660  3.362033 secs   500
    ## 37 4.8160  3.645479 secs   500
    ## 38 4.5100  2.902116 secs   500
    ## 39 5.0820  2.379958 secs   500
    ## 40 5.1360  2.466991 secs   500
    ## 41 5.0890  3.757754 secs  1000
    ## 42 4.7980  4.713679 secs  1000
    ## 43 4.8630  4.250989 secs  1000
    ## 44 4.7650  4.395717 secs  1000
    ## 45 5.1400  6.544450 secs  1000
    ## 46 5.0710  3.706580 secs  1000
    ## 47 4.8010  5.083369 secs  1000
    ## 48 5.4820  5.167562 secs  1000
    ## 49 5.1840  3.709071 secs  1000
    ## 50 4.9180  4.828439 secs  1000
    ## 51 4.8860  4.066140 secs  1000
    ## 52 5.2920  5.674263 secs  1000
    ## 53 4.6040  4.200588 secs  1000
    ## 54 6.2690  4.222737 secs  1000
    ## 55 4.3780  4.966535 secs  1000
    ## 56 5.0490  4.800193 secs  1000
    ## 57 4.8020  4.925235 secs  1000
    ## 58 4.5300  5.492424 secs  1000
    ## 59 5.0520  4.308994 secs  1000
    ## 60 4.6640  5.170154 secs  1000
    ## 61 4.8490 21.959305 secs  5000
    ## 62 4.8408 19.406147 secs  5000
    ## 63 4.8716 21.732027 secs  5000
    ## 64 4.8714 20.695028 secs  5000
    ## 65 4.6598 17.723208 secs  5000
    ## 66 4.8542 25.253352 secs  5000
    ## 67 4.8942 17.096538 secs  5000
    ## 68 4.9438 16.095570 secs  5000
    ## 69 4.9360 16.136204 secs  5000
    ## 70 4.9090 17.089339 secs  5000
    ## 71 5.0006 23.289908 secs  5000
    ## 72 5.2530 19.439789 secs  5000
    ## 73 4.8042 22.873455 secs  5000
    ## 74 5.1056 18.366693 secs  5000
    ## 75 4.8786 25.474264 secs  5000
    ## 76 4.8606 19.144937 secs  5000
    ## 77 5.1048 16.298815 secs  5000
    ## 78 4.8902 23.452951 secs  5000
    ## 79 4.8174 15.385733 secs  5000
    ## 80 5.1316 22.009420 secs  5000
    ## 81 4.9830 44.117283 secs 10000
    ## 82 4.9784 36.314282 secs 10000
    ## 83 4.8397 46.092711 secs 10000
    ## 84 4.9516 35.313026 secs 10000
    ## 85 4.8799 37.733963 secs 10000
    ## 86 4.8922 35.440542 secs 10000
    ## 87 4.7734 40.109752 secs 10000
    ## 88 4.9365 35.205459 secs 10000
    ## 89 4.8195 37.416929 secs 10000
    ## 90 4.9624 34.792254 secs 10000
    ## 91 4.9758 33.700685 secs 10000
    ## 92 5.0263 38.238195 secs 10000
    ## 93 4.9128 34.181886 secs 10000
    ## 94 4.9159 34.581962 secs 10000
    ## 95 4.9722 32.729328 secs 10000
    ## 96 4.9862 41.330060 secs 10000
    ## 97 5.0252 38.702208 secs 10000
    ## 98 4.8649 32.527321 secs 10000
    ## 99 4.8225 34.543853 secs 10000

    ## Warning in get_JIF(issn = "2045-2322", year = 2023, limit = l, seed = r): The
    ## journal published 47182 works in the two-year time window. Limiting to n=10000
    ## random papers.

    ##                journal      issn year paper_limit total_citations citable_items
    ## 1   Scientific Reports 2045-2322 2023         100             448           100
    ## 2   Scientific Reports 2045-2322 2023         100             415           100
    ## 3   Scientific Reports 2045-2322 2023         100             470           100
    ## 4   Scientific Reports 2045-2322 2023         100             534           100
    ## 5   Scientific Reports 2045-2322 2023         100             460           100
    ## 6   Scientific Reports 2045-2322 2023         100             457           100
    ## 7   Scientific Reports 2045-2322 2023         100             575           100
    ## 8   Scientific Reports 2045-2322 2023         100             470           100
    ## 9   Scientific Reports 2045-2322 2023         100             562           100
    ## 10  Scientific Reports 2045-2322 2023         100             461           100
    ## 11  Scientific Reports 2045-2322 2023         100             506           100
    ## 12  Scientific Reports 2045-2322 2023         100             387           100
    ## 13  Scientific Reports 2045-2322 2023         100             452           100
    ## 14  Scientific Reports 2045-2322 2023         100             449           100
    ## 15  Scientific Reports 2045-2322 2023         100             381           100
    ## 16  Scientific Reports 2045-2322 2023         100             454           100
    ## 17  Scientific Reports 2045-2322 2023         100             478           100
    ## 18  Scientific Reports 2045-2322 2023         100             420           100
    ## 19  Scientific Reports 2045-2322 2023         100             437           100
    ## 20  Scientific Reports 2045-2322 2023         100             557           100
    ## 21  Scientific Reports 2045-2322 2023         500            2512           500
    ## 22  Scientific Reports 2045-2322 2023         500            2261           500
    ## 23  Scientific Reports 2045-2322 2023         500            2311           500
    ## 24  Scientific Reports 2045-2322 2023         500            2331           500
    ## 25  Scientific Reports 2045-2322 2023         500            2611           500
    ## 26  Scientific Reports 2045-2322 2023         500            2681           500
    ## 27  Scientific Reports 2045-2322 2023         500            2443           500
    ## 28  Scientific Reports 2045-2322 2023         500            3165           500
    ## 29  Scientific Reports 2045-2322 2023         500            2611           500
    ## 30  Scientific Reports 2045-2322 2023         500            2442           500
    ## 31  Scientific Reports 2045-2322 2023         500            2376           500
    ## 32  Scientific Reports 2045-2322 2023         500            2632           500
    ## 33  Scientific Reports 2045-2322 2023         500            2217           500
    ## 34  Scientific Reports 2045-2322 2023         500            3818           500
    ## 35  Scientific Reports 2045-2322 2023         500            2201           500
    ## 36  Scientific Reports 2045-2322 2023         500            2433           500
    ## 37  Scientific Reports 2045-2322 2023         500            2408           500
    ## 38  Scientific Reports 2045-2322 2023         500            2255           500
    ## 39  Scientific Reports 2045-2322 2023         500            2541           500
    ## 40  Scientific Reports 2045-2322 2023         500            2568           500
    ## 41  Scientific Reports 2045-2322 2023        1000            5089          1000
    ## 42  Scientific Reports 2045-2322 2023        1000            4798          1000
    ## 43  Scientific Reports 2045-2322 2023        1000            4863          1000
    ## 44  Scientific Reports 2045-2322 2023        1000            4765          1000
    ## 45  Scientific Reports 2045-2322 2023        1000            5140          1000
    ## 46  Scientific Reports 2045-2322 2023        1000            5071          1000
    ## 47  Scientific Reports 2045-2322 2023        1000            4801          1000
    ## 48  Scientific Reports 2045-2322 2023        1000            5482          1000
    ## 49  Scientific Reports 2045-2322 2023        1000            5184          1000
    ## 50  Scientific Reports 2045-2322 2023        1000            4918          1000
    ## 51  Scientific Reports 2045-2322 2023        1000            4886          1000
    ## 52  Scientific Reports 2045-2322 2023        1000            5292          1000
    ## 53  Scientific Reports 2045-2322 2023        1000            4604          1000
    ## 54  Scientific Reports 2045-2322 2023        1000            6269          1000
    ## 55  Scientific Reports 2045-2322 2023        1000            4378          1000
    ## 56  Scientific Reports 2045-2322 2023        1000            5049          1000
    ## 57  Scientific Reports 2045-2322 2023        1000            4802          1000
    ## 58  Scientific Reports 2045-2322 2023        1000            4530          1000
    ## 59  Scientific Reports 2045-2322 2023        1000            5052          1000
    ## 60  Scientific Reports 2045-2322 2023        1000            4664          1000
    ## 61  Scientific Reports 2045-2322 2023        5000           24245          5000
    ## 62  Scientific Reports 2045-2322 2023        5000           24204          5000
    ## 63  Scientific Reports 2045-2322 2023        5000           24358          5000
    ## 64  Scientific Reports 2045-2322 2023        5000           24357          5000
    ## 65  Scientific Reports 2045-2322 2023        5000           23299          5000
    ## 66  Scientific Reports 2045-2322 2023        5000           24271          5000
    ## 67  Scientific Reports 2045-2322 2023        5000           24471          5000
    ## 68  Scientific Reports 2045-2322 2023        5000           24719          5000
    ## 69  Scientific Reports 2045-2322 2023        5000           24680          5000
    ## 70  Scientific Reports 2045-2322 2023        5000           24545          5000
    ## 71  Scientific Reports 2045-2322 2023        5000           25003          5000
    ## 72  Scientific Reports 2045-2322 2023        5000           26265          5000
    ## 73  Scientific Reports 2045-2322 2023        5000           24021          5000
    ## 74  Scientific Reports 2045-2322 2023        5000           25528          5000
    ## 75  Scientific Reports 2045-2322 2023        5000           24393          5000
    ## 76  Scientific Reports 2045-2322 2023        5000           24303          5000
    ## 77  Scientific Reports 2045-2322 2023        5000           25524          5000
    ## 78  Scientific Reports 2045-2322 2023        5000           24451          5000
    ## 79  Scientific Reports 2045-2322 2023        5000           24087          5000
    ## 80  Scientific Reports 2045-2322 2023        5000           25658          5000
    ## 81  Scientific Reports 2045-2322 2023       10000           49830         10000
    ## 82  Scientific Reports 2045-2322 2023       10000           49784         10000
    ## 83  Scientific Reports 2045-2322 2023       10000           48397         10000
    ## 84  Scientific Reports 2045-2322 2023       10000           49516         10000
    ## 85  Scientific Reports 2045-2322 2023       10000           48799         10000
    ## 86  Scientific Reports 2045-2322 2023       10000           48922         10000
    ## 87  Scientific Reports 2045-2322 2023       10000           47734         10000
    ## 88  Scientific Reports 2045-2322 2023       10000           49365         10000
    ## 89  Scientific Reports 2045-2322 2023       10000           48195         10000
    ## 90  Scientific Reports 2045-2322 2023       10000           49624         10000
    ## 91  Scientific Reports 2045-2322 2023       10000           49758         10000
    ## 92  Scientific Reports 2045-2322 2023       10000           50263         10000
    ## 93  Scientific Reports 2045-2322 2023       10000           49128         10000
    ## 94  Scientific Reports 2045-2322 2023       10000           49159         10000
    ## 95  Scientific Reports 2045-2322 2023       10000           49722         10000
    ## 96  Scientific Reports 2045-2322 2023       10000           49862         10000
    ## 97  Scientific Reports 2045-2322 2023       10000           50252         10000
    ## 98  Scientific Reports 2045-2322 2023       10000           48649         10000
    ## 99  Scientific Reports 2045-2322 2023       10000           48225         10000
    ## 100 Scientific Reports 2045-2322 2023       10000           50448         10000
    ##        JIF            dur limit
    ## 1   4.4800  3.504405 secs   100
    ## 2   4.1500  1.186225 secs   100
    ## 3   4.7000  1.597603 secs   100
    ## 4   5.3400  4.161026 secs   100
    ## 5   4.6000  1.265880 secs   100
    ## 6   4.5700  1.141317 secs   100
    ## 7   5.7500  1.259543 secs   100
    ## 8   4.7000  2.209416 secs   100
    ## 9   5.6200  1.265974 secs   100
    ## 10  4.6100  1.104112 secs   100
    ## 11  5.0600  1.330426 secs   100
    ## 12  3.8700  1.508438 secs   100
    ## 13  4.5200  1.074733 secs   100
    ## 14  4.4900  1.147448 secs   100
    ## 15  3.8100  1.443572 secs   100
    ## 16  4.5400  2.226332 secs   100
    ## 17  4.7800  1.058276 secs   100
    ## 18  4.2000  1.168020 secs   100
    ## 19  4.3700  1.521522 secs   100
    ## 20  5.5700  7.097642 secs   100
    ## 21  5.0240  2.231331 secs   500
    ## 22  4.5220  2.421490 secs   500
    ## 23  4.6220  5.640908 secs   500
    ## 24  4.6620  2.270812 secs   500
    ## 25  5.2220  3.537735 secs   500
    ## 26  5.3620  2.434339 secs   500
    ## 27  4.8860  3.784517 secs   500
    ## 28  6.3300  2.606812 secs   500
    ## 29  5.2220  2.302237 secs   500
    ## 30  4.8840  2.516571 secs   500
    ## 31  4.7520  3.198443 secs   500
    ## 32  5.2640  5.310478 secs   500
    ## 33  4.4340  4.225231 secs   500
    ## 34  7.6360  2.920446 secs   500
    ## 35  4.4020  2.432822 secs   500
    ## 36  4.8660  3.362033 secs   500
    ## 37  4.8160  3.645479 secs   500
    ## 38  4.5100  2.902116 secs   500
    ## 39  5.0820  2.379958 secs   500
    ## 40  5.1360  2.466991 secs   500
    ## 41  5.0890  3.757754 secs  1000
    ## 42  4.7980  4.713679 secs  1000
    ## 43  4.8630  4.250989 secs  1000
    ## 44  4.7650  4.395717 secs  1000
    ## 45  5.1400  6.544450 secs  1000
    ## 46  5.0710  3.706580 secs  1000
    ## 47  4.8010  5.083369 secs  1000
    ## 48  5.4820  5.167562 secs  1000
    ## 49  5.1840  3.709071 secs  1000
    ## 50  4.9180  4.828439 secs  1000
    ## 51  4.8860  4.066140 secs  1000
    ## 52  5.2920  5.674263 secs  1000
    ## 53  4.6040  4.200588 secs  1000
    ## 54  6.2690  4.222737 secs  1000
    ## 55  4.3780  4.966535 secs  1000
    ## 56  5.0490  4.800193 secs  1000
    ## 57  4.8020  4.925235 secs  1000
    ## 58  4.5300  5.492424 secs  1000
    ## 59  5.0520  4.308994 secs  1000
    ## 60  4.6640  5.170154 secs  1000
    ## 61  4.8490 21.959305 secs  5000
    ## 62  4.8408 19.406147 secs  5000
    ## 63  4.8716 21.732027 secs  5000
    ## 64  4.8714 20.695028 secs  5000
    ## 65  4.6598 17.723208 secs  5000
    ## 66  4.8542 25.253352 secs  5000
    ## 67  4.8942 17.096538 secs  5000
    ## 68  4.9438 16.095570 secs  5000
    ## 69  4.9360 16.136204 secs  5000
    ## 70  4.9090 17.089339 secs  5000
    ## 71  5.0006 23.289908 secs  5000
    ## 72  5.2530 19.439789 secs  5000
    ## 73  4.8042 22.873455 secs  5000
    ## 74  5.1056 18.366693 secs  5000
    ## 75  4.8786 25.474264 secs  5000
    ## 76  4.8606 19.144937 secs  5000
    ## 77  5.1048 16.298815 secs  5000
    ## 78  4.8902 23.452951 secs  5000
    ## 79  4.8174 15.385733 secs  5000
    ## 80  5.1316 22.009420 secs  5000
    ## 81  4.9830 44.117283 secs 10000
    ## 82  4.9784 36.314282 secs 10000
    ## 83  4.8397 46.092711 secs 10000
    ## 84  4.9516 35.313026 secs 10000
    ## 85  4.8799 37.733963 secs 10000
    ## 86  4.8922 35.440542 secs 10000
    ## 87  4.7734 40.109752 secs 10000
    ## 88  4.9365 35.205459 secs 10000
    ## 89  4.8195 37.416929 secs 10000
    ## 90  4.9624 34.792254 secs 10000
    ## 91  4.9758 33.700685 secs 10000
    ## 92  5.0263 38.238195 secs 10000
    ## 93  4.9128 34.181886 secs 10000
    ## 94  4.9159 34.581962 secs 10000
    ## 95  4.9722 32.729328 secs 10000
    ## 96  4.9862 41.330060 secs 10000
    ## 97  5.0252 38.702208 secs 10000
    ## 98  4.8649 32.527321 secs 10000
    ## 99  4.8225 34.543853 secs 10000
    ## 100 5.0448 38.600753 secs 10000

``` r
library(ggplot2)
ggplot(res, aes(x=limit, y=JIF)) + geom_point()
```

![](JIF_files/figure-html/unnamed-chunk-2-1.png)
