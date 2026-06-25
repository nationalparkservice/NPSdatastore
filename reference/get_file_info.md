# Retrieve information about files associated with a DataStore reference

Retrieve information about files associated with a DataStore reference

## Usage

``` r
get_file_info(reference_id, file_id, nps_internal = FALSE, dev = FALSE)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- file_id:

  Optional. Omit to retrieve info for all files in the reference. Unique
  ID (sometimes referred to as "resource ID" in DataStore) corresponding
  to the file you would like information about.

- nps_internal:

  Logical. NPS users on the internal network can set to TRUE to
  authenticate and view non-public data and create or modify references.
  All other users can ignore this argument and allow it to default to
  FALSE.

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

## Value

A tibble with columns userSort, resourceId, lastUpdate, description,
fileName, fileSize_kb, extension, mimeType, downloadLink, and
is508Compliant. Returns a zero-length tibble if no files exist.

## Examples

``` r
if (FALSE) { # \dontrun{
all_files <- get_file_info(reference_id = 652358)
specific_file <- get_file_info(reference_id = 2305163, file_id = 706913)
} # }
```
