# List the owners of a DataStore reference

Only available for NPS users on the internal network.

## Usage

``` r
get_reference_owners(reference_id, dev = FALSE)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

## Value

A tibble with the username, last name, first name, and email of each
reference owner.

## Examples

``` r
if (FALSE) { # \dontrun{
owners <- get_reference_owners(reference_id = 652358)
} # }
```
