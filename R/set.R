#' Create a new draft reference
#'
#' @param title The title of the reference
#' @param reference_type_code The code indicating the type of reference. See `get_reference_types()` for a full list of valid reference types.
#' @param date_published Text string with the date of publication. MUST be in "YYYY-MM-DD", "YYYY-MM", or "YYYY" format. Often the current date, but may be earlier if the resource was previously published outside of DataStore (often the case with journal articles, newsletters, etc.).
#' @param date_precision_code Optional. If `date_published` only specifies the year, you may optionally use this to specify a season or quarter. See `get_date_precision()` for valid options.
#' @inheritParams upload_file_to_reference
#' @returns A list containing information about the uploaded reference, including reference profile URL and reference ID.
#' @export
#'
#' @examples
#' \dontrun{
#'   new_ref <- create_draft_reference(title = "2024 Oakridge Goose Inventory",
#'                                     reference_type_code = "Datapackage",
#'                                     date_published = "2025-02-14",
#'                                     dev = TRUE)
#'
#'   another_new_ref <- create_draft_reference(title = "Oakridge Wildlife Hazards Update: Spring 2025",
#'                                             reference_type_code = "NewsletterArticle",
#'                                             date_published = "2025",
#'                                             date_precision_code = "Spring",
#'                                             dev = TRUE)
#' }
#'
#'
create_draft_reference <- function(title, reference_type_code, date_published, date_precision_code, dev = TRUE) {
  # Validate inputs
  rlang::arg_match(reference_type_code, get_reference_types()$code)
  if (!missing(date_precision_code)) {
    rlang::arg_match(date_precision_code, get_date_precision()$code)
  }

  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date_published)) {
    date_published <- list(year = lubridate::year(date_published),
                           month = lubridate::month(date_published),
                           day = lubridate::day(date_published))
  } else if (grepl("^[0-9]{4}-[0-9]{2}$", date_published)) {
    date_published <- paste0(date_published, "-01")  # add a dummy day so that lubridate parses it
    date_published <- list(year = lubridate::year(date_published),
                           month = lubridate::month(date_published))
  } else if (grepl("^[0-9]{4}$", date_published)) {
    date_published <- list(year = as.numeric(date_published))
    if (!missing(date_precision_code)) {
      date_published$precision <- date_precision_code
    }
  } else {
    cli::cli_abort("{.arg date_issued} must be a text string in the format YYYY-MM-DD, YYYY-MM, or YYYY.")
  }

  nps_internal <- TRUE

  request_body <- list(referenceTypeId = reference_type_code,
                       title = title,
                       issuedDate = date_published)

  new_ref <- .datastore_request(is_secure = nps_internal, is_dev = dev) |>
    httr2::req_url_path_append("Reference", "CreateDraft") |>
    httr2::req_body_json(request_body,
                         type = "application/json") |>
    httr2::req_perform()

  .validate_resp(new_ref,
                 nice_msg_400 = "Could not create new reference. This usually happens because you are not connected to the DOI/NPS network."
  )

  new_ref <- httr2::resp_body_json(new_ref)

  new_ref <- c(referenceUrl = .get_ref_profile_url(new_ref$referenceCode, dev),
               new_ref)

  return(new_ref)
}


#' Upload file to an existing DataStore reference
#'
#' This function is only available to NPS users on the internal network. Under
#' the hood, files are broken into multiple chunks and uploaded one chunk at a
#' time. This decreases the likelihood of failure for large files on slow
#' networks.
#'
#' @param reference_id Numeric reference ID. You must have the appropriate permissions to edit this reference.
#' @param file_path The path to the file that you want to upload.
#' @param is_508 Is the file 508 compliant?
#' @param dev Logical. Defaults to TRUE because it's best to attempt to modify references on the development & testing version of DataStore first. When everything is working, change to `dev = FALSE` and run again to edit the real reference.
#' @param interactive Logical. Prompt for user confirmation before uploading?
#' @param chunk_size_mb The "chunk" size to break the file into for upload. If your network is slow and your uploads are failing, try decreasing this number (e.g. 0.5 or 0.25).
#' @param retry How many times to retry uploading a file chunk if it fails on the first try.
#'
#' @returns A list containing the download URL and file ID for the uploaded file.
#' @export
#'
#' @examples
#' \dontrun{
#' id <- 12345  # The ID of a reference you have edit permissions to
#' upload <- upload_file_to_reference(reference_id = id,
#'                                    file_path = here::here("data", "my_file.csv"),
#'                                    is_508 = TRUE,
#'                                    dev = TRUE,
#'                                    interactive = TRUE,
#'                                    chunk_size_mb = 1)
#' }
#'
upload_file_to_reference <- function(reference_id, file_path, is_508 = FALSE, dev = TRUE, interactive = TRUE, chunk_size_mb = 1, retry = 1) {

  # Validate arguments
  .validate_ref_id(ref_id = reference_id, multiple_ok = FALSE)
  .validate_file_path(file_path)
  .validate_retry(retry)

  # Set values
  nps_internal <- TRUE
  is_508 <- dplyr::case_when(is_508 ~ 'true',
                             .default = 'false')  # convert is_508 to a string for the API
  file_name <- basename(file_path)  # Get just the filename

  # TODO: Allow user to pass in existing token?

  # Verify that we're modifying the right reference
  if (interactive) {
    .user_validate_ref_title(ref_id = reference_id,
                             is_secure = TRUE,
                             is_dev = dev)
  }

  # Get a token, which we need for a multi-chunk upload
  upload_token <- .datastore_request(is_secure = nps_internal, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "UploadFile", "TokenRequest") |>
    httr2::req_body_json(list(Name = file_name,
                              Is508Compliant = is_508),
                         type = "application/json") |>
    httr2::req_perform()

  .validate_resp(upload_token,
                 nice_msg_400 = "Could not retrieve upload token. This usually happens if you don't have permissions to edit the reference or if you are not connected to the DOI/NPS network."
  )

  upload_url <- upload_token$headers$Location

  # Get file size, set chunk size, determine number of chunks
  file_size_bytes <- file.size(file_path)
  chunk_size_bytes <- round(chunk_size_mb * 1024 * 1024)
  n_chunks <- ceiling(file_size_bytes/chunk_size_bytes)

  # Open file connection in binary mode
  file_con <- file(file_path, "rb")

  # Initialize variables and progress bar to track upload progress
  status <- NA
  total_bytes <- 0
  cli::cli_progress_bar("Uploading file", total = n_chunks)

  # Upload one chunk at a time
  for (i in 0:(n_chunks - 1)) {

    # Starting byte and ending byte for this chunk
    start <- i * chunk_size_bytes
    end <- start + chunk_size_bytes - 1

    # If we've exceeded the file size, reset ending byte
    if (end >= file_size_bytes) {
      end <- file_size_bytes - 1
    }

    n_bytes <- length(start:end)  # this should be chunk_size_bytes except on the last iteration
    total_bytes <- total_bytes + n_bytes  # total bytes uploaded so far

    # Reset the number of retries for each new chunk
    n_retries <- retry

    # Upload a single chunk. Potentially try again if it fails (retry > 0)
    while (n_retries >= 0) {
      upload_resp <- httr2::request(upload_url) |>
        httr2::req_method("PUT") |>
        httr2::req_headers(`Content-Length` = n_bytes,
                           `Content-Range` = glue::glue("bytes {start}-{end}/{file_size_bytes}")) |>
        httr2::req_body_raw(readBin(file_con, raw(), n = n_bytes)) |>
        httr2::req_options(httpauth = 4L, userpwd = ":::") |>
        httr2::req_error(is_error = \(resp) FALSE) |>
        httr2::req_perform()

      if (!httr2::resp_is_error(upload_resp) || httr2::resp_status(upload_resp) == 410) {
        # If upload is successful, or if error is due to token problem, don't retry
        n_retries <- -1
      } else {
        # Decrement retries remaining
        n_retries <- n_retries - 1
      }
    }

    # Throw an error if the chunk ultimately fails
    if (httr2::resp_status(upload_resp) == 410) {
      err_msg <- "Your upload token is invalid or has expired. Please try again. If the problem persists, contact the package maintainer or the DataStore helpdesk."
    } else {
      err_msg <- "File upload was unsuccessful. Please try again. If the problem persists, contact the package maintainer or the DataStore helpdesk."
    }
    .validate_resp(upload_resp,
                   nice_msg_400 = err_msg)


    cli::cli_progress_update()
  }
  cli::cli_progress_done()

  close(file_con)

  # TODO: Return details on uploaded file
  file_info <- list(url = upload_resp$headers$Location,
                    file_id = httr2::resp_body_json(upload_resp))

  return(file_info)
}

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
#' all_owners <- add_reference_owners(reference_id = 00000, upns = upns, emails = emails, dev = TRUE)
#'
#' }
#'
add_reference_owners <- function(reference_id, upns, emails, dev = TRUE, interactive = TRUE) {

  .validate_ref_id(reference_id)

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

  .validate_resp(added_owners)

  all_owners <- httr2::resp_body_json(added_owners)
  all_owners <- suppressWarnings(data.table::rbindlist(all_owners, use.names = TRUE, fill = TRUE))
  all_owners <- tibble::as_tibble(all_owners)

  return(all_owners)
}


#' Add keywords to a DataStore reference
#'
#' @param keywords A character vector of keywords
#' @inheritParams upload_file_to_reference
#'
#' @returns A character vector of all keywords for the reference
#' @export
#'
#' @examples
#' \dontrun{
#' my_keywords <- c("bison", "human-wildlife conflict", "visitor injuries")
#' all_keywords <- add_keywords(reference_id = 00000, keywords = my_keywords, dev = TRUE)
#' }
#'
add_keywords <- function(reference_id, keywords, dev = TRUE, interactive = TRUE) {
  .validate_ref_id(reference_id)

  # Verify that we're modifying the right reference
  if (interactive) {
    .user_validate_ref_title(ref_id = reference_id,
                             is_secure = TRUE,
                             is_dev = dev)
  }

  # Add the keywords
  added_keywords <- .datastore_request(is_secure = TRUE, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "Keywords") |>
    httr2::req_body_json(as.list(keywords)) |>
    httr2::req_method("POST") |>
    httr2::req_perform()

  .validate_resp(added_keywords)

  all_keywords <- httr2::resp_body_json(added_keywords)
  added_keywords <- unlist(all_keywords)

  return(added_keywords)
}


#' Add external links to a DataStore reference
#'
#' @param url The full URL you wish to add as an external link. Web URLs must start with "https://".
#' @param description A description of the external link.
#' @param last_verified Character. The date in ISO 8601 format (e.g. "2025-07-21") when the URL was last verified to be correct and working. You can almost always allow this to default to the current date.
#' @inheritParams upload_file_to_reference
#'
#' @returns A list of information about the link that was added
#' @export
#'
#' @examples
#' \dontrun{
#' link_added <- add_external_link(reference_id = 00000,
#' url = "https://www.nps.gov/im",
#' description = "I&M homepage",
#' dev = TRUE)
#' }
#'
add_external_link <- function(reference_id, url, description, last_verified = format(Sys.Date(), "%Y-%m-%d"), dev = TRUE, interactive = TRUE) {

  .validate_ref_id(reference_id)

  # Verify that we're modifying the right reference
  if (interactive) {
    .user_validate_ref_title(ref_id = reference_id,
                             is_secure = TRUE,
                             is_dev = dev)
  }

  links <- list(resourceId = 0,
                          userSort = 0,
                          description = description,
                          uri = url,
                          lastVerified = last_verified)

  added_link <- .datastore_request(is_secure = TRUE, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "ExternalLinks") |>
    httr2::req_body_json(links) |>
    httr2::req_method("POST") |>
    httr2::req_perform()

  .validate_resp(added_link, nice_msg_500 = httr2::resp_body_json(added_link)$exceptionMessage)

  link_info <- httr2::resp_body_json(added_link)

  return(link_info)
}

#' Replace the bibliography in a DataStore reference
#'
#' @param bibliography A list representing a DataStore bibliography. It is recommended that you retrieve the current bibliography using `get_bibliography()` and then modify it as needed.
#' @inheritParams upload_file_to_reference
#'
#' @returns A list representing the new bibliography.
#' @export
#'
#' @examples
#' \dontrun{
#' bib <- get_bibliography(reference_id = 00000)
#' bib$description <- "This is a new description for this reference"
#' bib$notes <- "This reference is for testing purposes only"
#' new_bib <- add_bibliography(reference_id = 00000, bibliography = bib, dev = TRUE)
#' }
#'
add_bibliography <- function(reference_id, bibliography, dev = TRUE, interactive = TRUE) {

  .validate_ref_id(reference_id)

  # Verify that we're modifying the right reference
  if (interactive) {
    .user_validate_ref_title(ref_id = reference_id,
                             is_secure = TRUE,
                             is_dev = dev)
  }

  names(bibliography) <- stringr::str_replace(names(bibliography), pattern = "^description$", "abstract")

  bib <- .datastore_request(is_secure = TRUE, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "Bibliography") |>
    httr2::req_body_json(bibliography) |>
    httr2::req_method("PUT") |>
    httr2::req_perform()

  .validate_resp(bib)

  bib <- httr2::resp_body_json(bib)
  bib <- stringr::str_replace(names(bib), pattern = "^abstract$", "description")

  return(bib)
}

#' Add DataStore reference(s) to a Project reference
#'
#' @param project_id The reference ID of the Project
#' @inheritParams search_references_by_id
#'
#' @returns A character vector of all keywords for the reference
#' @export
#'
#' @examples
#' \dontrun{
#'   proj_id <- 1111111
#'   ref_ids <- c(000000, 222222)
#'   add_to_project(project_id = proj_id,
#'                  reference_ids = ref_ids,
#'                  dev = TRUE)
#' }
#'
add_to_project <- function(project_id, reference_ids, dev = TRUE, interactive = TRUE) {
  .validate_ref_id(project_id)
  .validate_ref_id(reference_ids, multiple_ok = TRUE)

  # Verify that we're modifying the right reference
  if (interactive) {
    .user_validate_ref_title(ref_id = project_id,
                             is_secure = TRUE,
                             is_dev = dev)
  }

  # Add the references to the project
  added_refs <- .datastore_request(is_secure = TRUE, is_dev = dev) |>
    httr2::req_url_path_append("Reference", project_id, "ProductReference") |>
    httr2::req_body_json(as.list(reference_ids)) |>
    httr2::req_method("POST") |>
    httr2::req_perform()

  .validate_resp(added_refs)

  added_refs <- httr2::resp_body_json(added_refs)

  return(added_refs)
}
