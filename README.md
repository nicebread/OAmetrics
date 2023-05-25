# OAmetrics

An R package for computing several bibliometric indices by calling the OpenAlex database API.

You can install the developer version of `OAmetrics` from GitHub with:

```
install.packages("remotes")
remotes::install_github("nicebread/OAmetrics")
```


## Known issues / TODOs

- use `options(select = ...)` to reduce API load
- FNCS: If a paper is assigned to multiple fields, this should be an weighted average
- get_reference_set:  around 20% of sampled document are duplicates. Ensure that only unique documents are sampled (use paging?)
- 
