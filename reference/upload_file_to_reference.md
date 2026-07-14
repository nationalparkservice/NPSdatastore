# Upload file to an existing DataStore reference

This function is only available to NPS users on the internal network.
Under the hood, files are broken into multiple chunks and uploaded one
chunk at a time. This decreases the likelihood of failure for large
files on slow networks.

## Usage

``` r
upload_file_to_reference(
  reference_id,
  file_path,
  is_508 = FALSE,
  description,
  dev = TRUE,
  interactive = TRUE,
  chunk_size_mb = 1,
  retry = 1,
  verbose = FALSE
)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- file_path:

  The path to the file that you want to upload.

- is_508:

  Is the file 508 compliant?

- description:

  A short description of the file

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- interactive:

  Logical. Prompt for user confirmation before uploading?

- chunk_size_mb:

  The "chunk" size to break the file into for upload. If your network is
  slow and your uploads are failing, try decreasing this number (e.g.
  0.5 or 0.25).

- retry:

  How many times to retry uploading a file chunk if it fails on the
  first try.

- verbose:

  Logical. Leave this option as FALSE unless you have reported a bug and
  are asked to provide diagnostic info.

## Value

A tibble containing information about the uploaded file.

## Examples

``` r
if (FALSE) { # \dontrun{
id <- 12345  # The ID of a reference you have edit permissions to
upload <- upload_file_to_reference(reference_id = id,
                                   file_path = here::here("data", "my_file.csv"),
                                   is_508 = TRUE,
                                   dev = TRUE,
                                   interactive = TRUE,
                                   chunk_size_mb = 1)
} # }
```
