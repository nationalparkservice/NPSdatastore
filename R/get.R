#' Search NPS DataStore references by their ID
#'
#' @param reference_ids Numeric vector of reference IDs.
#' @param nps_internal Logical. NPS users on the internal network can set to TRUE to authenticate and view non-public data and create or modify references. All other users can ignore this argument and allow it to default to FALSE.
#' @param dev Logical. Set to TRUE to use the development & testing version of the API. If this means nothing to you, ignore and allow it to default to FALSE.
#'
#' @returns A named list containing detailed information about each reference, where the names are the reference IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' ids <- example_ref_ids(n = 2, seed = 2025)
#' ref_profiles <- search_references_by_id(reference_ids = ids,
#'                                         nps_internal = FALSE,
#'                                         dev = FALSE)
#' }
#'
search_references_by_id <- function(reference_ids, nps_internal = FALSE, dev = FALSE) {

  reference_ids <- unique(reference_ids)  # Make sure there aren't duplicate IDs

  .validate_ref_id(reference_ids, multiple_ok = TRUE)

  # The DataStore API only returns reference profiles for a maximum of 25 IDs,
  # so we have to make multiple requests if there are more than 25
  n_requests <- ceiling(length(reference_ids)/25)

  references <- list()
  for (i in 1:n_requests) {
    start <- (i - 1) * 25 + 1
    end <- min(i * 25, length(reference_ids))
    ref_ids <- reference_ids[start:end]

    references <- c(references,
                    .get_reference_profiles(ref_ids,
                                            is_secure = nps_internal,
                                            is_dev = dev))
  }

  if (length(references) == 0) {
    cli::cli_abort("Could not retrieve information for any of the requested references.")
  } else if (length(references) < length(reference_ids)) {
    ids_returned <- sapply(references, function(refs) {refs$referenceId})
    ids_missing <- reference_ids[!(reference_ids %in% ids_returned)]
    cli::cli_warn("Could not retrieve information for the following reference IDs: {.val {ids_missing}}.")
  }

  return(references)
}

#' Search NPS DataStore references by ID
#'
#' Retrieve only basic information about each reference. To return more detailed information about references, use `search_references_by_id()` instead.
#'
#' @inheritParams search_references_by_id
#'
#' @returns A tibble containing basic information about each reference.
#' @export
#'
#' @examples
#' \dontrun{
#' ids <- example_ref_ids(n = 2, seed = 2025)
#' ref_profiles <- search_references_by_id_basic(reference_ids = ids,
#'                                         nps_internal = FALSE,
#'                                         dev = FALSE)
#' }
#'
search_references_by_id_basic <- function(reference_ids, nps_internal = FALSE, dev = FALSE) {

  reference_ids <- unique(reference_ids)  # Make sure there aren't duplicate IDs

  .validate_ref_id(reference_ids, multiple_ok = TRUE)

  # Perform the request
  request <- .datastore_request(is_secure = nps_internal, is_dev = dev) |>
    httr2::req_url_path_append("ReferenceCodeSearch") |>
    httr2::req_url_query(q = reference_ids, .multi = "comma") |>
    httr2::req_perform()

  .validate_resp(request)

  references <- httr2::resp_body_json(request)

  # Convert nested list to single dataframe
  references <- suppressWarnings(data.table::rbindlist(references, use.names = TRUE, fill = TRUE))
  references <- tibble::as_tibble(references)

  if (nrow(references) == 0) {
    cli::cli_abort("Could not retrieve information for any of the requested references.")
  } else if (nrow(references) < length(reference_ids)) {
    ids_missing <- reference_ids[!(reference_ids %in% references$referenceId)]
    cli::cli_warn("Could not retrieve information for the following reference IDs: {.val {ids_missing}}.")
  }

  return(references)
}

#' Get a list of valid reference types
#'
#' @inheritParams search_references_by_id
#'
#' @returns A tibble with columns for reference code, label, description, and group code
#' @export
#'
#' @examples
#' valid_ref_types <- get_reference_types()
#'
get_reference_types <- function(dev = FALSE) {
  # Get the full list of reference types
  ref_types <- .datastore_request(is_secure = FALSE, is_dev = dev) |>
    httr2::req_url_path_append("FixedList/ReferenceTypes") |>
    httr2::req_perform()

  .validate_resp(ref_types)

  ref_types <- httr2::resp_body_json(ref_types)

  ref_types <- dplyr::bind_rows(ref_types) |>
    dplyr::rename(code = key)

  ref_types$ref_group_code <- NA

  # Get reference type groupings
  ref_groups <- .datastore_request(is_secure = FALSE, is_dev = dev) |>
    httr2::req_url_path_append("FixedList/ReferenceTypeGroups") |>
    httr2::req_perform()

  .validate_resp(ref_groups)

  ref_groups <- httr2::resp_body_json(ref_groups)

  lapply(ref_groups, function(group) {
    group_code <- group$key
    group_ref_types <- stringr::str_split(group$description, ", ")
    group_ref_types <- group_ref_types[[1]]

    ref_types[ref_types$code %in% group_ref_types, "ref_group_code"] <<- group_code
  })

  ref_types <- dplyr::arrange(ref_types, ref_group_code)

  return(ref_types)
}

#' Get a list of valid values for date precision
#'
#' @inheritParams search_references_by_id
#'
#' @returns A tibble with columns for date precision code and label
#' @export
#'
#' @examples
#' valid_precisions <- get_date_precision()
#'
get_date_precision <- function(dev = FALSE) {
  # Get the full list of date precision keywords
  precisions <- .datastore_request(is_secure = FALSE, is_dev = dev) |>
    httr2::req_url_path_append("FixedList/DatePrecisions") |>
    httr2::req_perform()

  .validate_resp(precisions)

  precisions <- httr2::resp_body_json(precisions)

  precisions <- dplyr::bind_rows(precisions) |>
    dplyr::rename(code = key)

  return(precisions)
}

#' List the owners of a DataStore reference
#'
#' Only available for NPS users on the internal network.
#'
#' @inheritParams upload_file_to_reference
#' @inheritParams search_references_by_id
#'
#' @returns A tibble with the username, last name, first name, and email of each reference owner.
#' @export
#'
#' @examples
#' owners <- get_reference_owners(reference_id = 652358)
#'
get_reference_owners <- function(reference_id, dev = FALSE) {

  .validate_ref_id(reference_id)

  owners <- .datastore_request(is_secure = TRUE, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "Owners") |>
    httr2::req_perform()

  .validate_resp(owners)

  owners <- httr2::resp_body_json(owners)

  owners <- suppressWarnings(data.table::rbindlist(owners, use.names = TRUE, fill = TRUE))
  owners <- tibble::as_tibble(owners)

  return(owners)
}

#' Retrieve the keywords for a DataStore reference
#'
#' @inheritParams upload_file_to_reference
#' @inheritParams search_references_by_id
#'
#' @returns A character vector of keywords.
#' @export
#'
#' @examples
#' owners <- get_keywords(reference_id = 652358)
#'
get_keywords <- function(reference_id, nps_internal = FALSE, dev = FALSE) {

  .validate_ref_id(reference_id)

  keywords <- .datastore_request(is_secure = nps_internal, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "Keywords") |>
    httr2::req_perform()

  .validate_resp(keywords)

  keywords <- httr2::resp_body_json(keywords)

  keywords <- unlist(keywords)
  keywords <- trimws(keywords, which = "both")

  return(keywords)
}

#' Retrieve the external links from a DataStore reference
#'
#' @inheritParams upload_file_to_reference
#' @inheritParams search_references_by_id
#'
#' @returns A tibble with columns userSort, resourceId, lastUpdate, description, uri, and lastVerified.
#' @export
#'
#' @examples
#' links <- get_external_links(reference_id = 652358)
#'
get_external_links <- function(reference_id, nps_internal = FALSE, dev = FALSE) {

  .validate_ref_id(reference_id)

  links <- .datastore_request(is_secure = nps_internal, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "ExternalLinks") |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  .validate_resp(links)

  links <- httr2::resp_body_json(links)

  links <- suppressWarnings(data.table::rbindlist(links, use.names = TRUE, fill = TRUE))
  links <- tibble::as_tibble(links)

  return(links)
}


#' Retrieve the bibliographic information from a DataStore reference
#'
#' @inheritParams upload_file_to_reference
#' @inheritParams search_references_by_id
#'
#' @returns A list containing bibliographic information.
#' @export
#'
#' @examples
#' bib <- get_bibliography(reference_id = 652358)
#'
get_bibliography <- function(reference_id, nps_internal = FALSE, dev = FALSE) {

  .validate_ref_id(reference_id)

  bib <- .datastore_request(is_secure = nps_internal, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "Bibliography") |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  .validate_resp(bib)

  bib <- httr2::resp_body_json(bib)
  names(bib) <- stringr::str_replace(names(bib), pattern = "^abstract$", "description")

  return(bib)
}
