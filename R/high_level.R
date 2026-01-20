#high level functions perform some specialized function that uses the API to perform a specific task. This may include iterating over lists, performing input validation, or stringing two or more API calls together to meet a specific need.

#' Sets the "by or for NPS" flag for multiple references
#'
#' This function allows the user to change the "by or for NPS" flag for one or more references. It also gives the user the option of making all the references active, putting them all in a draft status, or leaving the status of each reference the way it originally was.
#'
#' This function includes relatively little error checking and gives no opportunity for user input once it has started. The idea is that the user has done all the vetting prior to using the function. For instance, if a reference cannot be activated for some reason, the function will end in an error.
#'
#' Common errors include: supplying the wrong reference id number, not being connected to the VPN or on an NPS network, not having the correct permissions to make changes to the reference (e.g. you are not an owner of the reference) and, if attempting to activate a reference not having supplied all of the information necessary to activate the reference (perhaps there are no files attached or perhaps you have not specified any authors).
#'
#' The function returns a dataframe with the reference id, the current (after running the function) status of the "by or for NPS", the initial and final lifecycle status of each reference, and a hyperlink to the reference profile.
#'
#'
#'
#' @param reference_id Integer. Typically a seven-digit code
#' @param by_for_nps Logical. Defaults to TRUE
#' @param life_cycle String. Defaults to "Status Quo". Acceptable alternatives are "Active" and "Draft".
#' @param nps_internal Logical. Defaults to TRUE. Must be TRUE for most write actions.
#' @param dev Logical. Target the development server? Defaults to TRUE. False will target the production server.
#'
#' @returns a dataframe
#' @export
#'
#' @examples
#' \dontrun{
#'  nps_status <- set_by_for_nps_list(reference_id = c(1234567,
#'                                                     7654321,
#'                                                     0000000),
#'                                    by_for_nps = TRUE,
#'                                    life_cycle = "Status Quo",
#'                                    nps_internal = TRUE,
#'                                    dev = TRUE)
#'                                    }
set_by_for_nps_list <- function(reference_id,
                       by_for_nps = TRUE,
                       life_cycle = c("Status Quo", "Active", "Draft"),
                       nps_internal = TRUE,
                       dev = TRUE) {
  life_cycle <- stringr::str_to_title(life_cycle)
  life_cycle <- match.arg(life_cycle)

  #setup a dataframe to return relevant info:
  dat1 <- data.frame(matrix(ncol = 4, nrow = 0))

   for (i in 1:length(seq_along(reference_id))) {
     msg1 <- paste0("Working on Reference ",
                    as.character(reference_id[i]),
                    ".\n")
     cli::cli_inform(msg1)
      # return the current lifecycle status of the reference:
      lifecycle_info <- get_lifecycle_info(reference_id[i],
                                           nps_internal = nps_internal,
                                           dev = dev)
      lifecycle_status_initial <- lifecycle_info$lifecycle

      # reference must be set to draft to update by_for_nps
      if (lifecycle_status_initial != "Draft") {
        set_lifecycle_draft(reference_id[i],
                            dev = dev)
      }

      set_by_for_nps(reference_id[i],
                     by_for_nps = by_for_nps,
                     dev = dev,
                     interactive = FALSE)

      # set the lifecycle to user input

      # retain the original lifecycle status based on lack of user input:
      if (life_cycle == "Status Quo") {
        if(lifecycle_status_initial == "Active") {
          set_lifecycle_active(reference_id[i], dev = dev, interactive = FALSE)
        }
      } else {
        if(life_cycle == "Active") {
          active_error <- tryCatch({
            set_lifecycle_active(reference_id[i], dev = dev, interactive = FALSE)
          },
          error = function(e) {
            msg <- paste0("Reference ", reference_id[i], " could not be set ",
                          "to Active. Please make sure you have the proper ",
                          "permissions and the reference meets all of the ",
                          "requirements to be activated.")
            cli::cli_inform(msg)
          })
        }
      }

      #get current/new lifecycle status:
      lifecycle_info <- get_lifecycle_info(reference_id[i],
                                           nps_internal = nps_internal,
                                           dev = dev)
      lifecycle_status_final <- lifecycle_info$lifecycle

      For_NPS <- get_by_for_nps(reference_id[i],
                                nps_internal = TRUE,
                                dev = dev)

      #reference profile url:
      ref_url <- .get_ref_profile_url(reference_id[i], is_dev = dev)

      #create and populate df to be returned:
      dat2 <- data.frame(reference_id[i],
                         For_NPS,
                         lifecycle_status_initial,
                         lifecycle_status_final,
                         ref_url)
      dat1 <- rbind(dat1, dat2)
   }
  #colnames(dat1) <- c('reference_id', 'For_NPS', 'Lifecycle_initial', 'Lifecycle_final', "reference_url")
  colnames(dat1) <- c('reference_id', 'For_NPS', 'Lifecycle_initial', 'Lifecycle_final', 'url')
  return(dat1)
}
