# Create a new draft reference

Create a new draft reference

## Usage

``` r
create_draft_reference(
  title,
  reference_type_code,
  date_published,
  date_precision_code,
  dev = TRUE
)
```

## Arguments

- title:

  The title of the reference

- reference_type_code:

  The code indicating the type of reference. See
  [`get_reference_types()`](https://doi-nps.github.io/NPSdatastore/reference/get_reference_types.md)
  for a full list of valid reference types.

- date_published:

  Text string with the date of publication. MUST be in "YYYY-MM-DD",
  "YYYY-MM", or "YYYY" format. Often the current date, but may be
  earlier if the resource was previously published outside of DataStore
  (often the case with journal articles, newsletters, etc.).

- date_precision_code:

  Optional. If `date_published` only specifies the year, you may
  optionally use this to specify a season or quarter. See
  [`get_date_precision()`](https://doi-nps.github.io/NPSdatastore/reference/get_date_precision.md)
  for valid options.

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

## Value

A list containing information about the uploaded reference, including
reference profile URL and reference ID.

## Examples

``` r
if (FALSE) { # \dontrun{
  new_ref <- create_draft_reference(title = "2024 Oakridge Goose Inventory",
                                    reference_type_code = "Datapackage",
                                    date_published = "2025-02-14",
                                    dev = TRUE)

  another_new_ref <- create_draft_reference(title = "Oakridge Wildlife Hazards Update: Spring 2025",
                                            reference_type_code = "NewsletterArticle",
                                            date_published = "2025",
                                            date_precision_code = "Spring",
                                            dev = TRUE)
} # }

```
