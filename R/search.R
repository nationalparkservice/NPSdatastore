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
                                      nps_internal = nps_internal,
                                      dev = dev))
  }

  if (length(references) < length(reference_ids)) {
    ids_returned <- sapply(references, function(refs) {refs$referenceId})
    ids_missing <- reference_ids[!(reference_ids %in% ids_returned)]
    cli::cli_warn("Could not retrieve information for the following reference IDs: {.val {ids_missing}}.")
  }

  return(references)
}

