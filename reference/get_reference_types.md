# Get a list of valid reference types

Get a list of valid reference types

## Usage

``` r
get_reference_types(dev = FALSE)
```

## Arguments

- dev:

  Logical. Set to TRUE to use the development & testing version of the
  API. If this means nothing to you, ignore and allow it to default to
  FALSE.

## Value

A tibble with columns for reference code, label, description, and group
code

## Examples

``` r
if (FALSE) { # \dontrun{
valid_ref_types <- get_reference_types()
} # }
```
