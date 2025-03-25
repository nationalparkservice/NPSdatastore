#' Search NPS DataStore references by their ID
#'
#' @param reference_ids Numeric vector of reference IDs. The API only allows up to 25 reference IDs at a time.
#' @param nps_internal Logical. NPS users on the internal network can set to TRUE to authenticate and view non-public data and create or modify references. All other users can ignore this argument and allow it to default to FALSE.
#' @param dev Logical. Set to TRUE to use the development & testing version of the API. If this means nothing to you, ignore and allow it to default to FALSE.
#'
#' @returns fill in later
#' @export
#'
#' @examples
#'
#'
search_references_by_id <- function(reference_ids, nps_internal = FALSE, dev = FALSE) {

  reference_ids <- unique(reference_ids)

  if (length(reference_ids) > 25) {
    cli::cli_abort("You may only provide 25 reference IDs at a time.")
  }

  request <- .datastore_request(is_secure = nps_internal, is_dev = dev) |>
    httr2::req_url_path_append("Profile") |>
    httr2::req_url_query(q = reference_ids, .multi = "comma")

  response <- httr2::req_perform(request)

  references <- httr2::resp_body_json(response)

  if (length(references) < length(reference_ids)) {
    ids_returned <- sapply(references, function(refs) {refs$referenceId})
    ids_missing <- reference_ids[!(reference_ids %in% ids_returned)]
    cli::cli_warn("Could not retrieve information for the following reference IDs: {.val {ids_missing}}.")
  }

  return(references)
}
