# Helper function: Get reference set from files

The `get_reference_set` function can save intermediate files on a drive.
In case that the download or the function aborts with an error, all
existing files in a given path can be read combined.

## Usage

``` r
get_reference_set_from_files(path)
```

## Arguments

- path:

  The relative or absolute path of the files

## Value

Returns a data frame with a reference set.
