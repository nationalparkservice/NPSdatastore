# Activate a reference

Use with caution! It's usually a better idea to review the reference on
the DataStore website and manually activate it.

## Usage

``` r
set_lifecycle_active(
  reference_id,
  dev = TRUE,
  interactive = TRUE,
  verbose = FALSE
)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- interactive:

  Logical. Prompt for user confirmation before uploading?

- verbose:

  Logical. Leave this option as FALSE unless you have reported a bug and
  are asked to provide diagnostic info.

## Value

Invisibly returns the current lifecycle information for the reference

## Examples

``` r
if (FALSE) { # \dontrun{
  lifecycle_info <- set_lifecycle_active(reference_id = 652358)
} # }
```
