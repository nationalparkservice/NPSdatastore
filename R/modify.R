#' Add owners to a DataStore reference
#'
#' @inheritParams active_directory_lookup
#' @inheritParams upload_file_to_reference
#'
#' @returns A tibble of all reference owners with columns userCode, lastName, firstName, and email
#' @export
#'
#' @examples
#' \dontrun{
#' upns <- c("gmwright@nps.gov", "ymexia@nps.gov")
#' emails <- c("enid_michael@nps.gov", "edward_abbey@nps.gov")
#'
#' all_owners <- set_reference_owners(reference_id = 00000, upns = upns, emails = emails, dev = TRUE)
#'
#' }
#'
set_reference_owners <- function(reference_id, upns, emails, dev = TRUE, interactive = TRUE) {
  # Verify emails and UPNs with the AD verification API
  user_info <- active_directory_lookup(upns = upns, emails = emails)
  # Filter valid users
  valid_users <- dplyr::filter(user_info, found & !disabled)

  # Validate user identifiers
  if (nrow(user_info) > nrow(valid_users)) {

    # list users not found
    not_found <- dplyr::filter(user_info, !found)
    if (nrow(not_found) > 0) {
      not_found <- not_found %>%
        dplyr::pull(searchTerm) %>%
        stringr::str_flatten_comma(last = ", and ")
      not_found <- paste("user identifier(s) do not exist: ", not_found)
    } else {
      not_found <- NULL
    }

    # list deactivated users
    disabled <- dplyr::filter(user_info, disabled)
    if (nrow(disabled) > 0) {
      disabled <- disabled %>%
        dplyr::mutate(id_and_name = paste0(searchTerm, " (", cn, ")")) %>%
        dplyr::pull(id_and_name) %>%
        stringr::str_flatten_comma(last = ", and ")
      disabled <- paste("user(s) exist but are deactivated:", disabled)
    } else {
      disabled <- NULL
    }

    msg <- c(disabled, not_found)

    if (nrow(valid_users) > 0) {
      names(msg) <- rep("!", length(msg))
      cli::cli_warn(c("The following users could not be added as owners because... ",
                      msg))  # If there are some valid users, just throw a warning
    } else {
      names(msg) <- rep("x", length(msg))
      cli::cli_abort(c("No users could be added as owners because... ",
                       msg))  # If there are no valid users, throw an error
    }
  }

  # Verify that we're modifying the right reference
  if (interactive) {
    .user_validate_ref_title(ref_id = reference_id,
                             is_secure = TRUE,
                             is_dev = dev)
  }

  # Actually add the owners, finally

  # Get userCode (UPN), lastName, firstName, and email
  valid_users <- dplyr::select(valid_users,
                               userCode = userPrincipalName,
                               lastName = sn,
                               firstName = givenName,
                               email = mail)
  valid_users <- apply(valid_users, MARGIN = 1, FUN = as.list)

  added_owners <- .datastore_request(is_secure = TRUE, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "Owners") |>
    httr2::req_body_json(valid_users) |>
    httr2::req_perform()

  all_owners <- httr2::resp_body_json(added_owners)
  all_owners <- suppressWarnings(data.table::rbindlist(all_owners, use.names = TRUE, fill = TRUE))
  all_owners <- tibble::as_tibble(all_owners)

  return(all_owners)
}

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
    httr2::req_error(is_error = \(resp) FALSE) |>
    httr2::req_perform()

  .validate_resp(delete_owners, nice_msg_500 = paste0("Could not remove ", user_info$userPrincipalName,
                                                      " from reference ", reference_id,
                                                      ". If they are the only owner of the reference, you must add another owner before you can remove them."))

  all_owners <- httr2::resp_body_json(delete_owners)
  all_owners <- suppressWarnings(data.table::rbindlist(all_owners, use.names = TRUE, fill = TRUE))
  all_owners <- tibble::as_tibble(all_owners)

  return(all_owners)
}
