#' @title Check access to https://api.finna.fi/api/v1/?openapi
#' @description Check if R has access to resources at https://api.finna.fi/api/v1/?openapi.
#' This function tests whether R can successfully connect to the Finna API by attempting to download from the API's OpenAPI specification.
#' @export
#' @return a logical indicating if the API is accessible (`TRUE`) or not (`FALSE`).
#'
#' @importFrom httr status_code
#' @importFrom curl curl_download
#'
#' @examples
#' \dontrun{
#'   # Check if the API is accessible
#'   access <- check_api_access()
#'   if (access) {
#'     message("Finna API is accessible")
#'   } else {
#'     message("Finna API is not accessible")
#'   }
#' }

check_api_access <- function() {
  temp <- tempfile()
  http_url <- "https://api.finna.fi/api/v1/?openapi"

  suppressWarnings(
    try(
      curl::curl_download(http_url, temp, quiet = TRUE),
      silent = TRUE
    )
  )

  if (is.na(file.info(temp)$size)) {
    FALSE
  } else {
    TRUE
  }
}
