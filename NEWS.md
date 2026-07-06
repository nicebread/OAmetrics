# OAmetrics 0.5.3

## Bug fixes: 

Properly handle duplicate dois (e.g., when RESQUER has multi-study papers with the same DOI)

* Preserve the original DOI order and duplicate entries in `FNCS()` after fetching works from OpenAlex.
* Preserve the original DOI order and duplicate entries in `get_BIP()` after fetching scores from the BIP API.
* When `get_network()` receives pre-fetched works, reduce them to unique papers by DOI before analysis.

# OAmetrics 0.5.1

Make get_network() robust if more than 1 author.id is provided.

# OAmetrics 0.5.0

Adapting to the breaking changes of openalexR >= 2.0.0


# OAmetrics 0.2.0

## Major changes

* Add `pub_sim`:, A function that computes scores measuring the distance of a work to the core of an authors publications. This is used, for example, to filter out publications that have erroneously been attributed to an author by OpenAlex.

## Minor improvements and bug fixes

* Made `h-index()` more robust.
