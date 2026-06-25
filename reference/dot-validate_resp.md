# Check HTTP response and throw a helpful error if needed

Check HTTP response and throw a helpful error if needed

## Usage

``` r
.validate_resp(
  resp,
  nice_msg_400,
  nice_msg_500,
  details,
  call = rlang::caller_env()
)
```

## Arguments

- resp:

  HTTP response as returned by
  [`httr2::req_perform()`](https://rdrr.io/pkg/httr2/man/req_perform.html)

- nice_msg_400:

  Optional. Custom message for errors in the 400 range

- nice_msg_500:

  Optional. Custom message for errors in the 500 range

- details:

  Optional. Name of JSON field in response body that contains more info
  about the error

- call:

  Caller environment
