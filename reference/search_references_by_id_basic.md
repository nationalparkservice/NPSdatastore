# Search NPS DataStore references by ID

Retrieve only basic information about each reference. To return more
detailed information about references, use
[`search_references_by_id()`](https://doi-nps.github.io/NPSdatastore/reference/search_references_by_id.md)
instead.

## Usage

``` r
search_references_by_id_basic(reference_ids, nps_internal = FALSE, dev = FALSE)
```

## Arguments

- reference_ids:

  Numeric vector of reference IDs.

- nps_internal:

  Logical. NPS users on the internal network can set to TRUE to
  authenticate and view non-public data and create or modify references.
  All other users can ignore this argument and allow it to default to
  FALSE.

- dev:

  Logical. Set to TRUE to use the development & testing version of the
  API. If this means nothing to you, ignore and allow it to default to
  FALSE.

## Value

A tibble containing basic information about each reference.

## Examples

``` r
if (FALSE) { # \dontrun{
ids <- example_ref_ids(n = 2, seed = 2025)
ref_profiles <- search_references_by_id_basic(reference_ids = ids,
                                        nps_internal = FALSE,
                                        dev = FALSE)
} # }
```
