# Get a list of valid values for date precision

Get a list of valid values for date precision

## Usage

``` r
get_date_precision(dev = FALSE, verbose = FALSE)
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

A tibble with columns for date precision code and label

## Examples

``` r
if (FALSE) { # \dontrun{
valid_precisions <- get_date_precision()
} # }
```
