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

  # Perform the request
  request <- .datastore_request(is_secure = nps_internal, is_dev = dev) |>
    httr2::req_url_path_append("ReferenceCodeSearch") |>
    httr2::req_url_query(q = reference_ids, .multi = "comma") |>
    httr2::req_perform()

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

