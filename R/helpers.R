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

#' Retrieve some valid DataStore reference IDs
#'
#' For example and testing purposes. See `?NPSdatastore::public_refs and ?NPSdatastore::internal_refs` for more information.
#'
#' @param visiblity Return public-facing IDs, internal-facing IDs, or a mix of both?
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
example_ref_ids <- function(visiblity = c("public", "internal", "both"), n, seed) {
  visibility <- match.arg(visiblity, several.ok = FALSE)

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
