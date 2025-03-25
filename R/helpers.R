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

