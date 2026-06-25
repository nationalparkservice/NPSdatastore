# Get the right base URL for the DataStore API

Get the right base URL for the DataStore API

## Usage

``` r
.get_base_url(is_secure, is_dev)
```

## Arguments

- is_secure:

  Retrieve the secure version of the API base URL?

- is_dev:

  Retrieve the dev version of the API base URL?

## Value

One of four base URLs for the DataStore API (public, secure, public+dev,
secure+dev)
