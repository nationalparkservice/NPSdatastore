# Convert lists in a reference profile to tibbles

Use for reference profile elements like taxa and units that are returned
as lists but would be more appropriately stored as tibbles

## Usage

``` r
.lists2tibbles(parent_list, child_list_names)
```

## Arguments

- parent_list:

  The list representing the reference profile

- child_list_names:

  The elements of the reference profile that should be converted to
  tibbles

## Value

The tidied reference profile
