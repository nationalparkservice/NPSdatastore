# Set the flag that indicates whether a reference was created by or for NPS

Set the flag that indicates whether a reference was created by or for
NPS

## Usage

``` r
set_by_for_nps(
  reference_id,
  by_for_nps,
  dev = TRUE,
  interactive = TRUE,
  verbose = FALSE
)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- by_for_nps:

  TRUE or FALSE: was this reference created by or for NPS?

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- interactive:

  Logical. Prompt for user confirmation before uploading?

- verbose:

  Logical. Leave this option as FALSE unless you have reported a bug and
  are asked to provide diagnostic info.

## Value

A list representing the full updated bibliography.

## Examples

``` r
if (FALSE) { # \dontrun{
new_bib <- set_by_for_nps(reference_id = 000000, by_for_nps = TRUE)
new_bib <- set_by_for_nps(reference_id = 000000, by_for_nps = TRUE, dev = FALSE)
} # }
```
