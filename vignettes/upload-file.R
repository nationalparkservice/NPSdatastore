## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NPSdatastore)

## ----args, eval = FALSE-------------------------------------------------------
# file_to_upload <- "C:/your/file/location/here.csv"
# reference_id <- 1234567  # Replace this with your reference ID
# dev <- TRUE  # Change this to FALSE when you are done testing
# is_508_compliant <- FALSE  # If you know your file is 508 compliant, change to TRUE
# interactive <- TRUE  # If TRUE, you will be prompted to confirm file upload

## ----upload, eval = FALSE-----------------------------------------------------
# uploaded_file_info <- upload_file_to_reference(reference_id = reference_id,
#                                                file_path = file_to_upload,
#                                                is_508 = is_508_compliant,
#                                                interactive = interactive,
#                                                dev = dev)

## ----list_files, eval = FALSE-------------------------------------------------
# my_folder <- "scratchpad"  # The folder containing the files you want to upload
# pattern <- "\\.csv$"  # Look for files ending in ".csv"
# my_files <- list.files(my_folder, pattern = pattern, ignore.case = TRUE, full.names = TRUE)

## ----batch_upload, eval = FALSE-----------------------------------------------
# interactive <- FALSE  # Turn off the interactive confirmation prompt
# dev <- TRUE  # It's definitely good to test this out on irmadev first
# reference_id <- 1234567  # Replace this with your reference ID
# 
# # Iterate through each file and upload it
# all_file_info <- lapply(my_files, function(file) {
#   file_info <- upload_file_to_reference(reference_id = reference_id,
#                                         file_path = file,
#                                         is_508 = is_508_compliant,
#                                         interactive = interactive,
#                                         dev = dev)
#   return(tibble::tibble(original_file = file,
#                         url = file_info$url,
#                         file_id = file_info$file_id))
# })
# 
# # Collapse uploaded file info into a single dataframe
# all_file_info <- dplyr::bind_rows(all_file_info)

