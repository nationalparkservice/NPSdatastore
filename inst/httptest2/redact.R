redact_datastore <- function(x) {
  gsub_response(x, "https\\://irmaservices.nps.gov/datastore/v7/rest/", "publicapi/") |>
    gsub_response("https\\://irmadevservices.nps.gov/datastore/v7/rest/", "publicdevapi/") |>
    gsub_response("https\\://irmaservices.nps.gov/datastore-secure/v7/rest/", "secureapi/") |>
    gsub_response("https\\://irmadevservices.nps.gov/datastore-secure/v7/rest/", "securedevapi/") |>
    redact_cookies()
}

set_redactor(redact_datastore)
