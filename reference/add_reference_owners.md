# Add owners to a DataStore reference

Add owners to a DataStore reference

## Usage

``` r
add_reference_owners(
  reference_id,
  upns,
  emails,
  dev = TRUE,
  interactive = TRUE
)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- upns:

  A character vector of UPNs

- emails:

  A character vector of email addresses

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- interactive:

  Logical. Prompt for user confirmation before uploading?

## Value

A tibble of all reference owners with columns userCode, lastName,
firstName, and email

## Examples

``` r
if (FALSE) { # \dontrun{
upns <- c("gmwright@nps.gov", "ymexia@nps.gov")
emails <- c("enid_michael@nps.gov", "edward_abbey@nps.gov")

all_owners <- add_reference_owners(reference_id = 00000, upns = upns, emails = emails, dev = TRUE)

} # }
```
