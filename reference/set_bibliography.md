# Replace the bibliography in a DataStore reference

To update specific elements of the bibliography, use the "set\_"
functions for those elements instead.

## Usage

``` r
set_bibliography(reference_id, bibliography, dev = TRUE, interactive = TRUE)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- bibliography:

  A list representing a DataStore bibliography. It is recommended that
  you retrieve the current bibliography using
  [`get_bibliography()`](https://doi-nps.github.io/NPSdatastore/reference/get_bibliography.md)
  and then modify it as needed.

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- interactive:

  Logical. Prompt for user confirmation before uploading?

## Value

A list representing the new bibliography.

## Examples

``` r
if (FALSE) { # \dontrun{
bib <- get_bibliography(reference_id = 00000)
bib$description <- "This is a new description for this reference"
bib$notes <- "This reference is for testing purposes only"
new_bib <- set_bibliography(reference_id = 00000, bibliography = bib, dev = TRUE)
} # }
```
