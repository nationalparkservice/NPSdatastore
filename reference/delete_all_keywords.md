# Delete all keywords from a DataStore reference

Delete all keywords from a DataStore reference

## Usage

``` r
delete_all_keywords(reference_id, dev = TRUE, interactive = TRUE)
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

## Value

Invisibly returns the reference ID

## Examples

``` r
if (FALSE) { # \dontrun{
delete_all_keywords(reference_id = 00000, dev = TRUE)
} # }
```
