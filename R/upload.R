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
#' @param dev Logical. Defaults to TRUE because it's best to test out uploads on the development & testing version of DataStore first. Once you've verified that you're uploading the right things to the right reference, change to `dev = FALSE` and run once more to upload files to the "real" DataStore.
#' @param interactive Logical. Prompt for user confirmation before uploading?
#' @param chunk_size_mb The "chunk" size to break the file into for upload. If your network is slow and your uploads are failing, try decreasing this number (e.g. 0.5 or 0.25).
#' @param retry How many times to retry uploading a file chunk if it fails on the first try.
#'
#' @returns A list of metadata for the uploaded file(s)
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

  # Get a token, which we need for a multi-chunk upload
  upload_token <- .datastore_request(is_secure = nps_internal, is_dev = dev) |>
    httr2::req_url_path_append("Reference", reference_id, "UploadFile", "TokenRequest") |>
    httr2::req_body_json(list(Name = file_name,
                              Is508Compliant = is_508),
                         type = "application/json") |>
    httr2::req_error(is_error = \(resp) FALSE) |>
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

  return(upload_resp)
}

