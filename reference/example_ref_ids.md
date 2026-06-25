# Retrieve some valid DataStore reference IDs

For example and testing purposes. See
`?NPSdatastore::public_refs and ?NPSdatastore::internal_refs` for more
information.

## Usage

``` r
example_ref_ids(visibility = c("public", "internal", "both"), n, seed)
```

## Arguments

- visibility:

  Return public-facing IDs, internal-facing IDs, or a mix of both?

- n:

  Optional. Number of IDs to return. Will randomly sample `n` IDs. If
  `visibility == 'both'`, there is no guarantee that the sample of `n`
  IDs will return both public and internal-facing IDs. There are 48
  public-facing IDs and 45 internal-facing.

- seed:

  Optional. Set this if you need your sample of IDs to come back the
  same every time.

## Value

An integer vector of DataStore reference IDs.

## Examples

``` r
public_ids <- example_ref_ids(visibility = "public", n = 5)
all_ids <- example_ref_ids(visibility = "both")
```
