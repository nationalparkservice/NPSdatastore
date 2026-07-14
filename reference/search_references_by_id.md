# Search NPS DataStore references by their ID

Search NPS DataStore references by their ID

## Usage

``` r
search_references_by_id(
  reference_ids,
  nps_internal = FALSE,
  dev = FALSE,
  verbose = FALSE
)
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

- verbose:

  Logical. Leave this option as FALSE unless you have reported a bug and
  are asked to provide diagnostic info.

## Value

A named list containing detailed information about each reference, where
the names are the reference IDs.

## Examples

``` r
if (FALSE) { # \dontrun{
ids <- example_ref_ids(n = 2, seed = 2025)
ref_profiles <- search_references_by_id(reference_ids = ids,
                                        nps_internal = FALSE,
                                        dev = FALSE)
} # }
```
