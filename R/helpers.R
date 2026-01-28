## assign global package variables

# initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent = emptyenv())

# datastore API base URL:
assign("ds_public_api",
       "https://irmaservices.nps.gov/datastore/v8/rest",
       envir = .pkgglobalenv
)

# datastore secure API base URL:
assign("ds_secure_api",
       "https://irmaservices.nps.gov/datastore-secure/v8/rest",
       envir = .pkgglobalenv
)

# datastore secure dev api
assign("ds_dev_secure_api",
       "https://irmadevservices.nps.gov/datastore-secure/v8/rest",
       envir = .pkgglobalenv
)

# datastore dev API
assign("ds_dev_public_api",
       "https://irmadevservices.nps.gov/datastore/v8/rest",
       envir = .pkgglobalenv
)

# datastore reference URL
assign("ds_reference_url",
       "https://irma.nps.gov/DataStore/Reference/Profile",
       envir = .pkgglobalenv
)

# datastore dev/testing reference URL
assign("ds_dev_reference_url",
       "https://irmadev.nps.gov/DataStore/Reference/Profile",
       envir = .pkgglobalenv
)

#this gets rid of the "no visible binding for global variable 'x'" error in build checks:
globalVariables(c("public_refs",
                  "internal_refs",
                  "found",
                  "key",
                  "ref_group_code",
                  "searchTerm",
                  "cn",
                  "id_and_name",
                  "userPrincipalName",
                  "sn",
                  "givenName",
                  "mail",
                  "lastUpdate",
                  "userSort",
                  "resourceId",
                  "description",
                  "fileName",
                  "fileSize",
                  "extension",
                  "mimeType",
                  "downloadLink",
                  "is508Compliant",
                  "fileSize_kb"))


#' Get the right base URL for the DataStore API
#'
#' @param is_secure Retrieve the secure version of the API base URL?
#' @param is_dev Retrieve the dev version of the API base URL?
#'
#' @returns One of four base URLs for the DataStore API (public, secure, public+dev, secure+dev)
#'
.get_base_url <- function(is_secure, is_dev) {
  datastore_url <- dplyr::case_when(
    !is_secure && !is_dev ~ get("ds_public_api", envir = .pkgglobalenv),  # Public API
    !is_secure && is_dev ~ get("ds_dev_public_api", envir = .pkgglobalenv),  # Public testing API (probably limited use cases, but it exists)
    is_secure && !is_dev ~ get("ds_secure_api", envir = .pkgglobalenv),  # Secure API
    is_secure && is_dev ~ get("ds_dev_secure_api", envir = .pkgglobalenv)  # Secure testing API
  )

  return(datastore_url)
}

#' Given a reference ID, construct the URL to its profile page
#'
#' @param ref_id A reference ID
#' @inheritParams .get_base_url
#'
#' @returns The url to the reference profile page
#'
.get_ref_profile_url <- function(ref_id, is_dev) {
  ref_profile_url <- dplyr::case_when(
    is_dev ~ get("ds_dev_reference_url", envir = .pkgglobalenv),
    !is_dev ~ get("ds_reference_url", envir = .pkgglobalenv)
  )
  ref_profile_url <- paste(ref_profile_url, ref_id, sep = "/")

  return(ref_profile_url)
}

#' Create httr2 request for DataStore API
#'
#' @inheritParams .get_base_url
#' @param suppress_errors Suppress HTTP errors? Set to TRUE if using `.validate_resp()`
#'
#' @returns A httr2 request object with curl options set to allow authentication for NPS users (if using secure API)
#'
.datastore_request <- function(is_secure, is_dev, suppress_errors = TRUE) {
  base_url <- .get_base_url(is_secure = is_secure, is_dev = is_dev)

  request <- httr2::request(base_url)

  if (is_secure) {
    request <- httr2::req_options(request, httpauth = 4L, userpwd = ":::")
  }

  if (suppress_errors) {
    request <- httr2::req_error(request, is_error = \(resp) FALSE)
  }

  return(request)
}

#' Perform a single request to the Profile endpoint and tidy the data a little
#'
#' This endpoint only returns 25 results at a time; this helper function is used
#' inside of a loop or apply fxn to support retrieval of >25 profiles at a time
#'
#' @param reference_ids numeric vector of <=25 reference IDs
#' @inheritParams .get_base_url
#'
#' @returns List of reference profiles
#'
.get_reference_profiles <- function(reference_ids, is_secure, is_dev) {
  request <- .datastore_request(is_secure = is_secure, is_dev = is_dev) |>
    httr2::req_url_path_append("Profile") |>
    httr2::req_url_query(q = reference_ids, .multi = "comma") |>
    httr2::req_perform()

  .validate_resp(request)

  response <- httr2::resp_body_json(request)

  # Clean up reference profiles - simplify some lists to vectors and some to tibbles
  to_vectors <- c("keywords", "subjects")
  to_tibbles <- c("taxa", "units", "contentProducerUnits", "filesAndLinks")

  response <- lapply(response, function(ref) {
    ref <- .lists2vectors(ref, to_vectors)
    ref <- .lists2tibbles(ref, to_tibbles)
    names(ref$bibliography) <- stringr::str_replace(names(ref$bibliography), pattern = "^abstract$", "description")  # rename "abstract" to "description"
    return(ref)
  })


  ids <- sapply(response, function(ref) {ref$referenceId})
  names(response) <- ids

  return(response)
}

#' Convert lists in a reference profile to vectors
#'
#' Use for elements like keywords and subjects that are returned as lists of single-element lists
#'
#' @param parent_list The list representing the reference profile
#' @param child_list_names The elements of the reference profile that should be converted to vectors
#'
#' @returns The tidied reference profile
#'
.lists2vectors <- function(parent_list, child_list_names) {
  for (child_list in child_list_names) {
    if (!is.null(parent_list[[child_list]])) {
      parent_list[[child_list]] <- unlist(parent_list[[child_list]], use.names = FALSE)
    }
  }
  return(parent_list)
}

#' Convert lists in a reference profile to tibbles
#'
#' Use for reference profile elements like taxa and units that are returned as
#' lists but would be more appropriately stored as tibbles
#'
#' @param parent_list The list representing the reference profile
#' @param child_list_names The elements of the reference profile that should be converted to tibbles
#'
#' @returns The tidied reference profile
#'
.lists2tibbles <- function(parent_list, child_list_names) {
  for (child_list in child_list_names) {
    if (!is.null(parent_list[[child_list]])) {
      parent_list[[child_list]] <- dplyr::bind_rows(parent_list[[child_list]])
    }
  }
  return(parent_list)
}

#' Retrieve some valid DataStore reference IDs
#'
#' For example and testing purposes. See `?NPSdatastore::public_refs and ?NPSdatastore::internal_refs` for more information.
#'
#' @param visibility Return public-facing IDs, internal-facing IDs, or a mix of both?
#' @param n Optional. Number of IDs to return. Will randomly sample `n` IDs. If `visibility == 'both'`, there is no guarantee that the sample of `n` IDs will return both public and internal-facing IDs. There are 48 public-facing IDs and 45 internal-facing.
#' @param seed Optional. Set this if you need your sample of IDs to come back the same every time.
#'
#' @returns An integer vector of DataStore reference IDs.
#' @export
#'
#' @examples
#' public_ids <- example_ref_ids(visibility = "public", n = 5)
#' all_ids <- example_ref_ids(visibility = "both")
#'
example_ref_ids <- function(visibility = c("public", "internal", "both"), n, seed) {
  visibility <- match.arg(visibility, several.ok = FALSE)

  if (visibility == "public") {
    refs <- public_refs
  } else if (visibility == "internal") {
    refs <- internal_refs
  } else if (visibility == "both") {
    refs <- c(public_refs, internal_refs)
  }

  if (missing(n)) {
    n <- length(refs)
  } else if (n > length(refs)) {
    cli::cli_warn("{.arg n} exceeds total number of example reference IDs. Returning all example reference IDs.")
  }

  if (!missing(seed)) {
    set.seed(seed)
  }

  refs <- sample(refs, min(n, length(refs)), replace = FALSE)

  return(refs)
}

#' Validate reference ID arguments
#'
#' @param ref_id The reference ID argument to validate
#' @param multiple_ok Can ref_id be a vector of multiple IDs?
#' @param arg Used to get the actual name of the argument in the calling function. See `?rlang::`topic-error-call``
#' @param call The caller environment, for more helpful error messages. See `?rlang::`topic-error-call``
#'
.validate_ref_id <- function(ref_id, multiple_ok = FALSE,
                 arg = rlang::caller_arg(ref_id),
                 call = rlang::caller_env()) {
  # Enforce single reference ID
  if (!multiple_ok) {
    if (length(ref_id) > 1) {
      cli::cli_abort("You may only provide one reference ID at a time.",
                     call = call)
    }
  }

  # Enforce numeric reference ID
  if (!all(is.numeric(ref_id))) {
    cli::cli_abort("{.arg {arg}} is invalid. Reference IDs must be numeric. Check for typos, and ensure that reference IDs aren't surrounded by quotation marks.",
                   call = call)
  }

  # Enforce integer reference ID
  if (!all(ref_id == floor(ref_id))) {
    cli::cli_abort("{.arg {arg}} is invalid. Reference IDs must be whole number(s). Check for typos.",
                   call = call)
  }

}

.validate_lifecycle <- function(ref_id, expected_lifecycle = c("Draft", "Active"), is_dev,
                                call = rlang::caller_env()) {

  expected_lifecycle <- match.arg(expected_lifecycle, several.ok = FALSE)

  # Get actual lifecycle
  actual_lifecycle <- get_lifecycle_info(reference_id = ref_id, dev = is_dev)
  actual_lifecycle <- actual_lifecycle$lifecycle

  # Enforce is file, not folder
  if (actual_lifecycle != expected_lifecycle) {
    cli::cli_abort("Lifecycle for reference {ref_id} must be set to {expected_lifecycle}. It is currently set to {actual_lifecycle}.",
                   call = call)
  }
}

.validate_file_path <- function(file_path,
                                arg = rlang::caller_arg(file_path),
                                call = rlang::caller_env()) {
  # Enforce path exists
  if (!file.exists(file_path)) {
    cli::cli_abort("{.arg {arg}} is not a valid file location. Check for typos!",
                   call = call)
  }
  # Enforce is file, not folder
  if (dir.exists(file_path)) {
    cli::cli_abort("{.arg {arg}} must include a filename (e.g. \"data.csv\"). It looks like you provided the path to a folder instead.",
                   call = call)
  }
}

#' Validate TRUE/FALSE arguments
#'
#' @param bool Value to check
#' @param arg Used to get the actual name of the argument in the calling function. See `?rlang::`topic-error-call``
#' @param call The caller environment, for more helpful error messages. See `?rlang::`topic-error-call``
#'
.validate_truefalse <- function(bool,
                             arg = rlang::caller_arg(bool),
                             call = rlang::caller_env()) {

  # Enforce a TRUE/FALSE value
  if (!is.logical(bool) || is.na(bool)) {
    cli::cli_abort("{.arg {arg}} is invalid. Must be logical (`TRUE` or `FALSE`).",
                   call = call)
  }

}

.validate_retry <- function(retry,
                            arg = rlang::caller_arg(retry),
                            call = rlang::caller_env()) {
  if (retry %% 1 != 0) {
    cli::cli_abort("{.arg {arg}} must be an integer.",
                   call = call)
  }
  if (retry < 0) {
    cli::cli_abort("{.arg {arg}} cannot be less than zero.",
                   call = call)
  }
}

.validate_resp <- function(resp,
                           nice_msg_400,
                           nice_msg_500,
                           call = rlang::caller_env()) {

  if (httr2::resp_is_error(resp)) {
    if (missing(nice_msg_400)) {
      nice_msg_400 <- c("i" = "There's a problem with your API request. Check reference IDs, search terms, etc. for typos.",
                        "i" = "If you are an NPS user attempting to edit a reference or access non-public data, verify that you are on the NPS network, have the appropriate permissions, and have set {.arg nps_internal = TRUE} if applicable.",
                        "i" = "Verify that {.arg dev} is set correctly.")
    }
    if (missing(nice_msg_500)) {
      nice_msg_500 <- c("i" = "Something seems to have gone wrong on DataStore's end. For troubleshooting help, take a screenshot of this error and contact the package maintainer or the DataStore helpdesk.")
    }

    status_num <- httr2::resp_status(resp)

    if (floor(status_num/100) == 5) {
      nice_msg <- nice_msg_500
    } else if (floor(status_num/100) == 4) {
      nice_msg <- nice_msg_400
    }

    http_err <- glue::glue("HTTP {status_num}: {httr2::resp_status_desc(resp)}")
    cli::cli_abort(c(http_err, nice_msg), call = call)
  }
}

.user_validate_ref_title <- function(ref_id, is_secure, is_dev,
                                     call = rlang::caller_env()) {
  # Get reference title
  ref <- search_references_by_id_basic(reference_ids = ref_id, nps_internal = is_secure, dev = is_dev)
  ref_title <- ref$title

  # Get user input
  cli::cli_alert("You are about to modify the following reference: {ref_title}. Do you wish to continue?\n", wrap = TRUE)
  answer <- readline("(Y/N): ")
  if (!(tolower(answer) %in% c("y", "yes"))) {
    cli::cli_abort("Operation aborted by user.", call = call)
  }
}

#' Look up emails and/or UPNs in Active Directory
#'
#' Only works for NPS users on the internal network. Accepts a vector of UPNs, a vector of emails, or both.
#'
#' @param upns A character vector of UPNs
#' @param emails A character vector of email addresses
#'
#' @returns A dataframe of user information with a row for each UPN and email address searched, regardless of whether it exists in the system. If `found` is `TRUE`, the user exists. If `disabled` is `FALSE`, the user exists but has been deactivated.
#' @export
#'
#' @examples
#' \dontrun{
#' upns <- c("gmwright@nps.gov", "ymexia@nps.gov")
#' emails <- c("enid_michael@nps.gov", "edward_abbey@nps.gov")
#'
#' all_users <- active_directory_lookup(upns, emails)
#' emails_only <- active_directory_lookup(emails = emails)
#' }
#'
active_directory_lookup <- function(upns, emails) {
  base_url <- "https://irmaservices.nps.gov/adverification/v1/rest"

  user_info <- tibble::tibble()

  if (!missing(upns)) {
    upns <- paste0('"', upns, '"', collapse = ", ")  # collapse into a single string, quoted and comma separated
    upns <- paste0("[", upns, "]")  # surround with square brackets
    upn_lookup <- httr2::request(base_url) |>
      httr2::req_url_path_append("lookup", "upn") |>
      httr2::req_body_raw(upns) |>
      httr2::req_headers(Accept = "application/json",
                         `Content-Type` = "application/json") |>
      httr2::req_options(httpauth = 4L, userpwd = ":::") |>
      httr2::req_perform()

    upn_info <- httr2::resp_body_json(upn_lookup)

    upn_info <- suppressWarnings(data.table::rbindlist(upn_info, use.names = TRUE, fill = TRUE))
    user_info <- tibble::as_tibble(upn_info)
  }

  if (!missing(emails)) {
    emails <- paste0('"', emails, '"', collapse = ", ")  # collapse into a single string, quoted and comma separated
    emails <- paste0("[", emails, "]")  # surround with square brackets
    email_lookup <- httr2::request(base_url) |>
      httr2::req_url_path_append("lookup", "email") |>
      httr2::req_body_raw(emails) |>
      httr2::req_headers(Accept = "application/json",
                         `Content-Type` = "application/json") |>
      httr2::req_options(httpauth = 4L, userpwd = ":::") |>
      httr2::req_perform()

    email_info <- httr2::resp_body_json(email_lookup)

    email_info <- suppressWarnings(data.table::rbindlist(email_info, use.names = TRUE, fill = TRUE))
    email_info <- tibble::as_tibble(email_info)
    user_info <- rbind(user_info, email_info)
  }

  hyphen_regex <- "(\u002D|\u02D7|\u2010|\u2011|\u2012|\u2013|\u2014|\u2212|\uFE58|\uFE63|\uFF0D)"  # allow for all kinds of hyphens in orcid
  orcid_regex <- paste(rep("[0-9]{4}", 4), collapse = hyphen_regex)  # create regex to match an orcid: 16 digits with a hyphen every 4 digits
  user_info <- user_info |>
    dplyr::rename(orcid = extensionAttribute2) |>  # extensionAttribute2 is user's orcid
    dplyr::mutate(orcid = stringr::str_extract(orcid, orcid_regex))  # orcids can be formatted inconsistently, so strip out just the 16-digit ID

  return(user_info)
}
