# Validate TRUE/FALSE arguments

Validate TRUE/FALSE arguments

## Usage

``` r
.validate_truefalse(
  bool,
  arg = rlang::caller_arg(bool),
  call = rlang::caller_env()
)
```

## Arguments

- bool:

  Value to check

- arg:

  Used to get the actual name of the argument in the calling function.
  See `?rlang::`topic-error-call“

- call:

  The caller environment, for more helpful error messages. See
  `?rlang::`topic-error-call“
