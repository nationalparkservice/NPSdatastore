# Add DataStore reference(s) to a Project reference

Add DataStore reference(s) to a Project reference

## Usage

``` r
add_to_project(
  project_id,
  reference_ids,
  dev = TRUE,
  interactive = TRUE,
  verbose = FALSE
)
```

## Arguments

- project_id:

  The reference ID of the Project

- reference_ids:

  Numeric vector of reference IDs.

- dev:

  Logical. Set to TRUE to use the development & testing version of the
  API. If this means nothing to you, ignore and allow it to default to
  FALSE.

- interactive:

  Logical. Prompt for user confirmation before uploading?

- verbose:

  Logical. Leave this option as FALSE unless you have reported a bug and
  are asked to provide diagnostic info.

## Value

A character vector of all keywords for the reference

## Examples

``` r
if (FALSE) { # \dontrun{
  proj_id <- 1111111
  ref_ids <- c(000000, 222222)
  add_to_project(project_id = proj_id,
                 reference_ids = ref_ids,
                 dev = TRUE)
} # }
```
