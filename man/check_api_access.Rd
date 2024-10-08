% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_api_access.R
\name{check_api_access}
\alias{check_api_access}
\title{Check access to https://api.finna.fi/api/v1/?openapi}
\usage{
check_api_access()
}
\value{
a logical indicating if the API is accessible (\code{TRUE}) or not (\code{FALSE}).
}
\description{
Check if R has access to resources at https://api.finna.fi/api/v1/?openapi.
This function tests whether R can successfully connect to the Finna API by attempting to download from the API's OpenAPI specification.
}
\examples{
\dontrun{
  # Check if the API is accessible
  access <- check_api_access()
  if (access) {
    message("Finna API is accessible")
  } else {
    message("Finna API is not accessible")
  }
}
}
