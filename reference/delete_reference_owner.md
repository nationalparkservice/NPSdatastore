# Delete an owner from a DataStore reference

Delete an owner from a DataStore reference

## Usage

``` r
delete_reference_owner(
  reference_id,
  upn,
  email,
  dev = TRUE,
  interactive = TRUE,
  verbose = FALSE
)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- upn:

  A single user's UPN. Must specify either `upn` or `email` but not
  both.

- email:

  A single user's email. Must specify either `upn` or `email` but not
  both.

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

A tibble of all reference owners with columns userCode, lastName,
firstName, and email

## Examples

``` r
if (FALSE) { # \dontrun{

all_owners <- delete_reference_owner(reference_id = 00000,
                                     email = "edward_abbey@nps.gov",
                                     dev = TRUE)
all_owners <- delete_reference_owner(reference_id = 00000,
                                     upn = "gmwright@nps.gov",
                                     dev = TRUE)
} # }
```
