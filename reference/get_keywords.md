# Retrieve the keywords for a DataStore reference

Retrieve the keywords for a DataStore reference

## Usage

``` r
get_keywords(reference_id, nps_internal = FALSE, dev = FALSE, verbose = FALSE)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- nps_internal:

  Logical. NPS users on the internal network can set to TRUE to
  authenticate and view non-public data and create or modify references.
  All other users can ignore this argument and allow it to default to
  FALSE.

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- verbose:

  Logical. Leave this option as FALSE unless you have reported a bug and
  are asked to provide diagnostic info.

## Value

A character vector of keywords.

## Examples

``` r
owners <- get_keywords(reference_id = 652358)
```
