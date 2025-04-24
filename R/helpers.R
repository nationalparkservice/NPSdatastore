## assign global package variables

# initiate new environment accessible from within package:
.pkgglobalenv <- new.env(parent = emptyenv())

# data_store API base URL:
assign("ds_public_api",
       "https://irmaservices.nps.gov/datastore/v7/rest",
       envir = .pkgglobalenv
)

# data_store secure API base URL:
assign("ds_secure_api",
       "https://irmaservices.nps.gov/datastore-secure/v7/rest",
       envir = .pkgglobalenv
)

# data_store secure dev api
assign("ds_dev_secure_api",
       "https://irmadevservices.nps.gov/datastore-secure/v7/rest",
       envir = .pkgglobalenv
)

# data_store dev API
assign("ds_dev_public_api",
       "https://irmadevservices.nps.gov/datastore/v7/rest",
       envir = .pkgglobalenv
)

#this gets rid of the "no visible binding for global variable 'x'" error in build checks:
globalVariables(c("public_refs",
                  "internal_refs"))


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

#' Create httr2 request for DataStore API
#'
#' @inheritParams .get_base_url
#'
#' @returns A httr2 request object with curl options set to allow authentication for NPS users (if using secure API)
#'
.datastore_request <- function(is_secure, is_dev) {
  base_url <- .get_base_url(is_secure = is_secure, is_dev = is_dev)

  request <- httr2::request(base_url)

  if (is_secure) {
    request <- httr2::req_options(request, httpauth = 4L, userpwd = ":::")
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

  response <- httr2::resp_body_json(request)

  # Clean up reference profiles - simplify some lists to vectors and some to tibbles
  to_vectors <- c("keywords", "subjects")
  to_tibbles <- c("taxa", "units", "contentProducerUnits", "filesAndLinks")

  response <- lapply(response, function(ref) {
    ref <- .lists2vectors(ref, to_vectors)
    ref <- .lists2tibbles(ref, to_tibbles)

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
