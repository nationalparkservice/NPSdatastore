# Set contact(s) for a reference

Under the hood, DataStore references have up to three "contact" lists,
which map to authors, creators, contacts, etc. depending on the
reference type. Each contact list can contain multiple individuals or
organizations.

## Usage

``` r
set_contacts(reference_id, contacts, dev = TRUE, interactive = TRUE)
```

## Arguments

- reference_id:

  Numeric reference ID. You must have the appropriate permissions to
  edit this reference.

- contacts:

  A dataframe with the following columns:

  - `contactTypeKey`: Required. The key corresponding to the contact
    type for this reference. Use
    [`get_contact_types()`](https://doi-nps.github.io/NPSdatastore/reference/get_contact_types.md)
    to look up the appropriate key for a given reference type.

  - `contactType`: Optional. This column is ignored, but allowed for
    code clarity and for compatibility with contacts table returned by
    [`get_bibliography()`](https://doi-nps.github.io/NPSdatastore/reference/get_bibliography.md)

  - `primaryName`: Required. Last name/family name. Also used for the
    name of an organization, corporation, or other entity.

  - `firstName`: First name/given name. Leave blank for
    organizations/corporations.

  - `middleName`: Middle name or middle initial. Leave blank for
    organizations/corporations.

  - `affiliation`: Employer or organization the contact belonged to when
    they were involved in the project

  - `orcid`: 16-digit unique persistent identifier for individuals
    engaging in research, scholarship, and innovation. If you're a
    contact on a DataStore reference, you should probably have an
    [ORCID](https://orcid.org)! You can omit this column or set it to NA
    if not applicable.

  - `isCorporate`: Required. Set to TRUE if contact is an
    organization/corporation. Otherwise set to FALSE.

  Note that in DataStore, the lists for contact type keys 1, 2, and 3
  each map to a specific *type* of contact, which depends on the
  reference type. Not every reference uses all three contact fields. Use
  [`get_contact_types()`](https://doi-nps.github.io/NPSdatastore/reference/get_contact_types.md)
  if you're not sure what each contact field corresponds to.

- dev:

  Logical. Defaults to TRUE because it's best to attempt to modify
  references on the development & testing version of DataStore first.
  When everything is working, change to `dev = FALSE` and run again to
  edit the real reference.

- interactive:

  Logical. Prompt for user confirmation before uploading?

## Value

A list representing the full updated bibliography.

## Details

This function is meant to be called by reference-type-specific wrapper
functions, which are more user-friendly. It's best to use those instead.

When modifying existing contacts for a reference, it's best to retrieve
the contacts list using
[`get_bibliography()`](https://doi-nps.github.io/NPSdatastore/reference/get_bibliography.md)
and modify as needed.

## Examples

``` r
if (FALSE) { # \dontrun{
new_bib <- set_by_for_nps(reference_id = 000000, by_for_nps = TRUE)
new_bib <- set_by_for_nps(reference_id = 000000, by_for_nps = TRUE, dev = FALSE)
} # }
```
