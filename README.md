# get citation counts for a specific paper.
paper <- oa_fetch(entity = "works", doi = "https://doi.org/10.1177/2515245917745629")
paper$cited_by_count
