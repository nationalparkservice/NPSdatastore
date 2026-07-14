# Add external links to a DataStore reference

Add external links to a DataStore reference

## Usage

``` r
add_external_link(
  reference_id,
  url,
  description,
  last_verified = format(Sys.Date(), "%Y-%m-%d"),
  dev = TRUE,
  interactive = TRUE,
  verbose = FALSE
)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- url:

  The full URL you wish to add as an external link. Web URLs must start
  with "https://".

- description:

  A description of the external link.

- last_verified:

  Character. The date in ISO 8601 format (e.g. "2025-07-21") when the
  URL was last verified to be correct and working. You can almost always
  allow this to default to the current date.

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

A list of information about the link that was added

## Examples

``` r
if (FALSE) { # \dontrun{
link_added <- add_external_link(reference_id = 00000,
url = "https://www.nps.gov/im",
description = "I&M homepage",
dev = TRUE)
} # }
```
