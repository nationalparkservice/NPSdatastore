# Set file information

Set file information

## Usage

``` r
set_file_info(
  reference_id,
  file_id,
  description,
  is_508,
  dev = TRUE,
  interactive = TRUE
)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- file_id:

  The unique identifier for the file. Also referred to as the resource
  ID.

- description:

  A short description of the file

- is_508:

  Is the file 508 compliant?

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- interactive:

  Logical. Prompt for user confirmation before uploading?

## Value

A tibble containing the updated file information.

## Examples

``` r
if (FALSE) { # \dontrun{
  set_file_info(2313985, file_id = 731902, description = "testing 123")
} # }
```
