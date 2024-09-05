#' @title Check access to https://api.finna.fi/api/v1/?openapi
#' @description Check if R has access to resources at https://api.finna.fi/api/v1/?openapi
#' @export
#' @return a logical.
#'
#' @importFrom httr status_code
#' @importFrom curl curl_download
#'
#' @examples
#'  \dontrun{
#'    check_api_access()
#'  }

check_api_access <- function(){

  temp <- tempfile()
  http_url <- "https://api.finna.fi/api/v1/?openapi"

  suppressWarnings(
    try(
      curl::curl_download(http_url, temp, quiet = TRUE),
      silent = TRUE)
  )
  if (is.na(file.info(temp)$size)) {
    FALSE
  }
  else{
    TRUE
  }
}
