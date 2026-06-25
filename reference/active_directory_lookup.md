# Look up emails and/or UPNs in Active Directory

Only works for NPS users on the internal network. Accepts a vector of
UPNs, a vector of emails, or both.

## Usage

``` r
active_directory_lookup(upns, emails)
```

## Arguments

- upns:

  A character vector of UPNs

- emails:

  A character vector of email addresses

## Value

A dataframe of user information with a row for each UPN and email
address searched, regardless of whether it exists in the system. If
`found` is `TRUE`, the user exists. If `disabled` is `FALSE`, the user
exists but has been deactivated.

## Examples

``` r
if (FALSE) { # \dontrun{
upns <- c("gmwright@nps.gov", "ymexia@nps.gov")
emails <- c("enid_michael@nps.gov", "edward_abbey@nps.gov")

all_users <- active_directory_lookup(upns, emails)
emails_only <- active_directory_lookup(emails = emails)
} # }
```
