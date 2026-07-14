# Retrieve the current lifecycle status of a reference. Only works for internal NPS users.

Retrieve the current lifecycle status of a reference. Only works for
internal NPS users.

## Usage

``` r
get_lifecycle_info(reference_id, dev = FALSE, verbose = FALSE)
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

- verbose:

  Logical. Leave this option as FALSE unless you have reported a bug and
  are asked to provide diagnostic info.

## Value

A list of lifecycle information for the reference

## Examples

``` r
if (FALSE) { # \dontrun{
  lifecycle_info <- get_lifecycle_info(reference_id = 652358)
  lifecycle = lifecycle_info$lifecycle
} # }
```
