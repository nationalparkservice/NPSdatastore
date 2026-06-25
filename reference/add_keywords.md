# Add keywords to a DataStore reference

Add keywords to a DataStore reference

## Usage

``` r
add_keywords(reference_id, keywords, dev = TRUE, interactive = TRUE)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- keywords:

  A character vector of keywords

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- interactive:

  Logical. Prompt for user confirmation before uploading?

## Value

A character vector of all keywords for the reference

## Examples

``` r
if (FALSE) { # \dontrun{
my_keywords <- c("bison", "human-wildlife conflict", "visitor injuries")
all_keywords <- add_keywords(reference_id = 00000, keywords = my_keywords, dev = TRUE)
} # }
```
