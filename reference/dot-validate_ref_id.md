# Validate reference ID arguments

Validate reference ID arguments

## Usage

``` r
.validate_ref_id(
  ref_id,
  multiple_ok = FALSE,
  arg = rlang::caller_arg(ref_id),
  call = rlang::caller_env()
)
```

## Arguments

- ref_id:

  The reference ID argument to validate

- multiple_ok:

  Can ref_id be a vector of multiple IDs?

- arg:

  Used to get the actual name of the argument in the calling function.
  See `?rlang::`topic-error-call“

- call:

  The caller environment, for more helpful error messages. See
  `?rlang::`topic-error-call“
