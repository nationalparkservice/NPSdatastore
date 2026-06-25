# Package index

## Retrieve Data & Metadata

### Publicly accessible

Functions for retrieving files and metadata from DataStore. If you are
not an internal NPS user, you can still use these functions to access
publicly available information on DataStore. NPS users can set
`nps_internal = TRUE` to access restricted references as well.

- [`search_references_by_id()`](https://doi-nps.github.io/NPSdatastore/reference/search_references_by_id.md)
  : Search NPS DataStore references by their ID
- [`search_references_by_id_basic()`](https://doi-nps.github.io/NPSdatastore/reference/search_references_by_id_basic.md)
  : Search NPS DataStore references by ID
- [`get_by_for_nps()`](https://doi-nps.github.io/NPSdatastore/reference/get_by_for_nps.md)
  : Determine if a reference was created by/for the NPS
- [`get_contact_types()`](https://doi-nps.github.io/NPSdatastore/reference/get_contact_types.md)
  : Get a list of valid contact types
- [`get_date_precision()`](https://doi-nps.github.io/NPSdatastore/reference/get_date_precision.md)
  : Get a list of valid values for date precision
- [`get_external_links()`](https://doi-nps.github.io/NPSdatastore/reference/get_external_links.md)
  : Retrieve the external links from a DataStore reference
- [`get_file_info()`](https://doi-nps.github.io/NPSdatastore/reference/get_file_info.md)
  : Retrieve information about files associated with a DataStore
  reference
- [`get_keywords()`](https://doi-nps.github.io/NPSdatastore/reference/get_keywords.md)
  : Retrieve the keywords for a DataStore reference
- [`get_park_units()`](https://doi-nps.github.io/NPSdatastore/reference/get_park_units.md)
  : Retrieve the park units from a DataStore reference
- [`get_reference_owners()`](https://doi-nps.github.io/NPSdatastore/reference/get_reference_owners.md)
  : List the owners of a DataStore reference
- [`get_reference_types()`](https://doi-nps.github.io/NPSdatastore/reference/get_reference_types.md)
  : Get a list of valid reference types

### Internal only

- [`get_lifecycle_info()`](https://doi-nps.github.io/NPSdatastore/reference/get_lifecycle_info.md)
  : Retrieve the current lifecycle status of a reference. Only works for
  internal NPS users.
- [`get_bibliography()`](https://doi-nps.github.io/NPSdatastore/reference/get_bibliography.md)
  : Retrieve the bibliographic information from a DataStore reference

## Create and Modify References

### Internal only

Functions for creating and modifying references in DataStore. These
functions only work for internal NPS users. Note that by default, these
functions will use the development and testing version of DataStore to
avoid accidental changes to real references. Always test your code
before switching to `dev = FALSE`! When in doubt, contact the DataStore
team (<NRSS_datastore@nps.gov>) for help.

- [`create_draft_reference()`](https://doi-nps.github.io/NPSdatastore/reference/create_draft_reference.md)
  : Create a new draft reference
- [`upload_file_to_reference()`](https://doi-nps.github.io/NPSdatastore/reference/upload_file_to_reference.md)
  : Upload file to an existing DataStore reference
- [`add_content_units()`](https://doi-nps.github.io/NPSdatastore/reference/add_content_units.md)
  : Add content units to a DataStore reference
- [`add_external_link()`](https://doi-nps.github.io/NPSdatastore/reference/add_external_link.md)
  : Add external links to a DataStore reference
- [`add_keywords()`](https://doi-nps.github.io/NPSdatastore/reference/add_keywords.md)
  : Add keywords to a DataStore reference
- [`add_reference_owners()`](https://doi-nps.github.io/NPSdatastore/reference/add_reference_owners.md)
  : Add owners to a DataStore reference
- [`add_to_project()`](https://doi-nps.github.io/NPSdatastore/reference/add_to_project.md)
  : Add DataStore reference(s) to a Project reference
- [`set_bibliography()`](https://doi-nps.github.io/NPSdatastore/reference/set_bibliography.md)
  : Replace the bibliography in a DataStore reference
- [`set_by_for_nps()`](https://doi-nps.github.io/NPSdatastore/reference/set_by_for_nps.md)
  : Set the flag that indicates whether a reference was created by or
  for NPS
- [`set_contacts()`](https://doi-nps.github.io/NPSdatastore/reference/set_contacts.md)
  : Set contact(s) for a reference
- [`set_file_info()`](https://doi-nps.github.io/NPSdatastore/reference/set_file_info.md)
  : Set file information
- [`set_license()`](https://doi-nps.github.io/NPSdatastore/reference/set_license.md)
  : Set the license for a DataStore reference
- [`set_lifecycle_active()`](https://doi-nps.github.io/NPSdatastore/reference/set_lifecycle_active.md)
  : Activate a reference
- [`set_lifecycle_draft()`](https://doi-nps.github.io/NPSdatastore/reference/set_lifecycle_draft.md)
  : Set a reference to draft mode
- [`delete_all_keywords()`](https://doi-nps.github.io/NPSdatastore/reference/delete_all_keywords.md)
  : Delete all keywords from a DataStore reference
- [`delete_reference_owner()`](https://doi-nps.github.io/NPSdatastore/reference/delete_reference_owner.md)
  : Delete an owner from a DataStore reference

## Examples

- [`example_ref_ids()`](https://doi-nps.github.io/NPSdatastore/reference/example_ref_ids.md)
  : Retrieve some valid DataStore reference IDs
- [`public_refs`](https://doi-nps.github.io/NPSdatastore/reference/public_refs.md)
  : Reference IDs for public-facing references
- [`internal_refs`](https://doi-nps.github.io/NPSdatastore/reference/internal_refs.md)
  : Reference IDs for internal-facing references

## Miscellaneous

### Internal only

- [`active_directory_lookup()`](https://doi-nps.github.io/NPSdatastore/reference/active_directory_lookup.md)
  : Look up emails and/or UPNs in Active Directory
