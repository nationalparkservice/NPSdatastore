# Determine if a reference was created by/for the NPS

Determine if a reference was created by/for the NPS

## Usage

``` r
get_by_for_nps(
  reference_id,
  nps_internal = FALSE,
  dev = FALSE,
  verbose = FALSE
)
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

TRUE if the reference was created by or for the NPS, FALSE if not.

## Examples

``` r
if (FALSE) { # \dontrun{
  by_for_nps <- get_by_for_nps(reference_id = 652358)
} # }
```
