#' Get a list of valid reference types
#'
#' @param dev Use the testing/development version of the API? You can almost always allow this to default to TRUE, except in specific beta testing scenarios.
#'
#' @returns A tibble with columns for reference code, label, description, and group code
#' @export
#'
#' @examples
#' valid_ref_types <- get_reference_types()
#' View(valid_ref_types)
#'
get_reference_types <- function(dev = FALSE) {
  # Get the full list of reference types
  ref_types <- .datastore_request(is_secure = FALSE, is_dev = dev) |>
    httr2::req_url_path_append("FixedList/ReferenceTypes") |>
    httr2::req_perform()

  ref_types <- httr2::resp_body_json(ref_types)

  ref_types <- dplyr::bind_rows(ref_types) |>
    dplyr::rename(code = key)

  ref_types$ref_group_code <- NA

  # Get reference type groupings
  ref_groups <- .datastore_request(is_secure = FALSE, is_dev = dev) |>
    httr2::req_url_path_append("FixedList/ReferenceTypeGroups") |>
    httr2::req_perform()

  ref_groups <- httr2::resp_body_json(ref_groups)

  lapply(ref_groups, function(group) {
    group_code <- group$key
    group_ref_types <- stringr::str_split(group$description, ", ")
    group_ref_types <- group_ref_types[[1]]

    ref_types[ref_types$code %in% group_ref_types, "ref_group_code"] <<- group_code
  })

  ref_types <- dplyr::arrange(ref_types, ref_group_code)

  return(ref_types)
}

#' Get a list of valid values for date precision
#'
#' @param dev Use the testing/development version of the API? You can almost always allow this to default to TRUE, except in specific beta testing scenarios.
#'
#' @returns A tibble with columns for date precision code and label
#' @export
#'
#' @examples
#' valid_precisions <- get_date_precision()
#' View(valid_precisions)
#'
get_date_precision <- function(dev = FALSE) {
  # Get the full list of date precision keywords
  precisions <- .datastore_request(is_secure = FALSE, is_dev = dev) |>
    httr2::req_url_path_append("FixedList/DatePrecisions") |>
    httr2::req_perform()

  precisions <- httr2::resp_body_json(precisions)

  precisions <- dplyr::bind_rows(precisions) |>
    dplyr::rename(code = key)

  return(precisions)
}

