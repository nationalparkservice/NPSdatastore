# Get a list of valid reference types

Get a list of valid reference types

## Usage

``` r
get_reference_types(dev = FALSE, verbose = FALSE)
```

## Arguments

- dev:

  Logical. Set to TRUE to use the development & testing version of the
  API. If this means nothing to you, ignore and allow it to default to
  FALSE.

- verbose:

  Logical. Leave this option as FALSE unless you have reported a bug and
  are asked to provide diagnostic info.

## Value

A tibble with columns for reference code, label, description, and group
code

## Examples

``` r
if (FALSE) { # \dontrun{
valid_ref_types <- get_reference_types()
} # }
```
