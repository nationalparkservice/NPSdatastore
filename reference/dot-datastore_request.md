# Create httr2 request for DataStore API

Create httr2 request for DataStore API

## Usage

``` r
.datastore_request(is_secure, is_dev, suppress_errors = TRUE)
```

## Arguments

- is_secure:

  Retrieve the secure version of the API base URL?

- is_dev:

  Retrieve the dev version of the API base URL?

- suppress_errors:

  Suppress HTTP errors? Set to TRUE if using
  [`.validate_resp()`](https://doi-nps.github.io/NPSdatastore/reference/dot-validate_resp.md)

## Value

A httr2 request object with curl options set to allow authentication for
NPS users (if using secure API)
