# Set the license for a DataStore reference

Any existing license will be overwritten.

## Usage

``` r
set_license(reference_id, license_type_id, dev = TRUE, interactive = TRUE)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- license_type_id:

  The numeric ID of the license you wish to apply.

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- interactive:

  Logical. Prompt for user confirmation before uploading?

## Value

A list of the referenceId and the licenseTypeId

## Examples

``` r
if (FALSE) { # \dontrun{
new_bib <- add_license(000000,
                       license_id = 1)  # CC0 license
} # }
```
