# Get a list of valid values for date precision

Get a list of valid values for date precision

## Usage

``` r
get_date_precision(dev = FALSE)
```

## Arguments

- dev:

  Logical. Set to TRUE to use the development & testing version of the
  API. If this means nothing to you, ignore and allow it to default to
  FALSE.

## Value

A tibble with columns for date precision code and label

## Examples

``` r
if (FALSE) { # \dontrun{
valid_precisions <- get_date_precision()
} # }
```
