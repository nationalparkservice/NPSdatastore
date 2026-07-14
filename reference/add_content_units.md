# Add content units to a DataStore reference

Add content units to a DataStore reference

## Usage

``` r
add_content_units(
  reference_id,
  parks,
  include_linked_units = FALSE,
  add_bounding_box = TRUE,
  dev = TRUE,
  interactive = TRUE,
  verbose = FALSE
)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- parks:

  A character vector of park/unit codes

- include_linked_units:

  For unit codes that contain other units (e.g. an I&M network or
  region), should the contained units also be added?

- add_bounding_box:

  Add the unit's bounding box to the reference's geographic coverage?

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

A character vector of all unit codes for the reference

## Examples

``` r
if (FALSE) { # \dontrun{
my_content_units <- c("LAKE", "DEVA", "MOJA", "JOTR")
all_content_units <- add_content_units(reference_id = 00000, parks = my_content_units, dev = TRUE)
} # }
```
