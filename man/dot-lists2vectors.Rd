% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{.lists2vectors}
\alias{.lists2vectors}
\title{Convert lists in a reference profile to vectors}
\usage{
.lists2vectors(parent_list, child_list_names)
}
\arguments{
\item{parent_list}{The list representing the reference profile}

\item{child_list_names}{The elements of the reference profile that should be converted to vectors}
}
\value{
The tidied reference profile
}
\description{
Use for elements like keywords and subjects that are returned as lists of single-element lists
}
