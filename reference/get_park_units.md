# Retrieve the park units from a DataStore reference

Retrieve the park units from a DataStore reference

## Usage

``` r
get_park_units(reference_id, nps_internal = FALSE, dev = FALSE)
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

## Value

A tibble of park codes and park names.

## Examples

``` r
if (FALSE) { # \dontrun{
park_units <- get_park_units(reference_id = 2305911)
} # }
```
