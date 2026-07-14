# Get a list of valid contact types

Get a list of valid contact types

## Usage

``` r
get_contact_types(reference_type, dev = FALSE, verbose = FALSE)
```

## Arguments

- reference_type:

  Optional. A valid reference type, as found in the `code` column
  returned by
  [`get_reference_types()`](https://doi-nps.github.io/NPSdatastore/reference/get_reference_types.md).
  If omitted, will return contact types for every reference type.

- dev:

  Logical. Set to TRUE to use the development & testing version of the
  API. If this means nothing to you, ignore and allow it to default to
  FALSE.

- verbose:

  Logical. Leave this option as FALSE unless you have reported a bug and
  are asked to provide diagnostic info.

## Value

A tibble with columns for reference code, key, label, and description

## Examples

``` r
if (FALSE) { # \dontrun{
valid_contact_types <- get_contact_types()
} # }
```
