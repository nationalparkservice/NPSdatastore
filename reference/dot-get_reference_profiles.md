# Perform a single request to the Profile endpoint and tidy the data a little

This endpoint only returns 25 results at a time; this helper function is
used inside of a loop or apply fxn to support retrieval of \>25 profiles
at a time

## Usage

``` r
.get_reference_profiles(reference_ids, is_secure, is_dev, verbose = FALSE)
```

## Arguments

- reference_ids:

  numeric vector of \<=25 reference IDs

- is_secure:

  Retrieve the secure version of the API base URL?

- is_dev:

  Retrieve the dev version of the API base URL?

## Value

List of reference profiles
