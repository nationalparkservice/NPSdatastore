#' Delete an owner from a DataStore reference
#'
#' @param upn A single user's UPN. Must specify either `upn` or `email` but not both.
#' @param email A single user's email. Must specify either `upn` or `email` but not both.
#' @inheritParams upload_file_to_reference
#'
#' @returns A tibble of all reference owners with columns userCode, lastName, firstName, and email
#' @export
#'
#' @examples
#' \dontrun{
#'
#' all_owners <- delete_reference_owner(reference_id = 00000,
#'                                      email = "edward_abbey@nps.gov",
#'                                      dev = TRUE)
#' all_owners <- delete_reference_owner(reference_id = 00000,
#'                                      upn = "gmwright@nps.gov",
#'                                      dev = TRUE)
#' }
#'
delete_reference_owner <- function(reference_id, upn, email, dev = TRUE, interactive = TRUE) {

  .validate_ref_id(reference_id)

  # Enforce deleting one user at a time
  n_users <- length(rlang::maybe_missing(upn, default = c())) +
    length(rlang::maybe_missing(email, default = c()))
  if (n_users > 1) {
    cli::cli_abort("Only one user at a time may be deleted.")
  }

  # Verify email or UPN with the AD verification API
  user_info <- active_directory_lookup(upns = upn, emails = email)

  # Validate user identifier
  # list users not found
  not_found <- dplyr::filter(user_info, !found)
  if (nrow(not_found) > 0) {
    cli::cli_abort("User identifier does not exist: {not_found$searchTerm}")
  }

  # Verify that we're modifying the right reference
  if (interactive) {
    .user_validate_ref_title(ref_id = reference_id,
                             is_secure = TRUE,
                             is_dev = dev)
  }

  # Actually delete the owners
  delete_owners <- .datastore_request(is_secure = TRUE, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "Owners") |>
    httr2::req_url_query(userCode = user_info$userPrincipalName) |>
    httr2::req_method("DELETE") |>
    httr2::req_perform()

  .validate_resp(delete_owners, nice_msg_500 = paste0("Could not remove ", user_info$userPrincipalName,
                                                      " from reference ", reference_id,
                                                      ". If they are the only owner of the reference, you must add another owner before you can remove them."))

  all_owners <- httr2::resp_body_json(delete_owners)
  all_owners <- suppressWarnings(data.table::rbindlist(all_owners, use.names = TRUE, fill = TRUE))
  all_owners <- tibble::as_tibble(all_owners)

  return(all_owners)
}


#' Delete all keywords from a DataStore reference
#'
#' @inheritParams upload_file_to_reference
#'
#' @returns Invisibly returns the reference ID
#' @export
#'
#' @examples
#' \dontrun{
#' delete_all_keywords(reference_id = 00000, dev = TRUE)
#' }
#'
delete_all_keywords <- function(reference_id, dev = TRUE, interactive = TRUE) {
  .validate_ref_id(reference_id)

  # Verify that we're modifying the right reference
  if (interactive) {
    .user_validate_ref_title(ref_id = reference_id,
                             is_secure = TRUE,
                             is_dev = dev)
  }

  # Add the keywords
  delete_keywords <- .datastore_request(is_secure = TRUE, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "Keywords") |>
    httr2::req_method("DELETE") |>
    httr2::req_perform()

  .validate_resp(delete_keywords)

  invisible(reference_id)
}
