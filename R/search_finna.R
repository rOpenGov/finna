#' Finna Index Search with Advanced Options
#'
#' This function performs a search on the Finna index with extended options, allowing for a wide range of search types, filters, facets, and sorting methods.
#'
#' @name search_finna
#' @param lookfor A string containing the search terms. Boolean operators (AND, OR, NOT) can be included.
#' @param type A string specifying the type of search. Options include "AllFields", "Title", "Author", "Subject". Defaults to "AllFields".
#' @param fields A vector of fields to be returned in the search results. Defaults to NULL, which returns a standard set of fields.
#' @param filters A vector of filter queries to refine the search. Defaults to NULL.
#' @param facets A vector specifying which facets to return in the results. Defaults to NULL.
#' @param facetFilters A vector of regular expressions to filter facets. Defaults to NULL.
#' @param sort A string defining the sort order of the results. Options include:
#' \itemize{
#'   \item "relevance,id asc" (default)
#'   \item "main_date_str desc" (Year, newest first)
#'   \item "main_date_str asc" (Year, oldest first)
#'   \item "last_indexed desc" (Last modified)
#'   \item "first_indexed desc" (Last added)
#'   \item "callnumber,id asc" (Classmark)
#'   \item "author,id asc" (Author)
#'   \item "title,id asc" (Title)
#' }
#' @param page An integer indicating the page number of results to retrieve. Defaults to 1.
#' @param limit An integer specifying the number of records to return per page. Defaults to 20.
#' @param lng A string for the language of returned translated strings. Options are "fi", "en-gb", "sv", "se". Defaults to "fi".
#' @param prettyPrint A logical value indicating whether to pretty-print the JSON response. Useful for debugging. Defaults to FALSE.
#' @return A tibble containing the search results with relevant fields extracted. Returns NULL if the search fails or if the input is invalid.
#' @examples
#' search_results <- search_finna("sibelius", sort = "main_date_str desc")
#' print(search_results)
#' @export
search_finna <- function(lookfor,
                         type = "AllFields",
                         fields = NULL,
                         filters = NULL,
                         facets = NULL,
                         facetFilters = NULL,
                         sort = "relevance,id asc",
                         page = 1,
                         limit = 100,
                         lng = "fi",
                         prettyPrint = FALSE) {

  # Handle empty search queries
  if (lookfor == "" || is.null(lookfor)) {
    warning("Error: Empty search query provided.")
    return(NULL)
  }

  # Define the base URL for the search API
  base_url <- "https://api.finna.fi/v1/search"

  # Construct the query parameters
  query_params <- list(
    lookfor = lookfor,
    type = type,
    `field[]` = fields,
    `filter[]` = filters,
    `facet[]` = facets,
    `facetFilter[]` = facetFilters,
    sort = sort,
    page = page,
    limit = limit,
    lng = lng,
    prettyPrint = prettyPrint
  )

  # Execute the GET request and handle potential errors
  response <- tryCatch(
    httr::GET(base_url, query = query_params),
    error = function(e) {
      warning("Error: Failed to make the request.")
      return(NULL)
    }
  )

  # Process the response based on the status code
  if (httr::status_code(response) == 200) {
    # Parse the JSON content of the response
    search_results <- httr::content(response, "parsed")

    # Extract and structure relevant data from the search results
    records <- search_results$records
    data <- lapply(records, function(record) {
      list(
        Title = record$title %||% NA,
        Author = if (!is.null(record$nonPresenterAuthors) && length(record$nonPresenterAuthors) > 0) {
          paste(sapply(record$nonPresenterAuthors, function(author) author$name), collapse = ", ")
        } else {
          NA
        },
        Year = record$year %||% NA,
        Language = if (!is.null(record$languages) && length(record$languages) > 0) record$languages[[1]] else NA,
        Formats = if (!is.null(record$formats) && length(record$formats) > 0) {
          paste(sapply(record$formats, function(format) format$translated), collapse = ", ")
        } else {
          NA
        },
        Subjects = if (!is.null(record$subjects) && length(record$subjects) > 0) {
          paste(sapply(record$subjects, function(subject) paste(subject, collapse = ", ")), collapse = "; ")
        } else {
          NA
        },
        Library = if (!is.null(record$buildings) && length(record$buildings) > 0) {
          paste(sapply(record$buildings, function(building) building$translated), collapse = ", ")
        } else {
          NA
        },
        Series = tryCatch({
          if (!is.null(record$series)) {
            if (is.list(record$series)) {
              if (length(record$series) > 0) {
                paste(sapply(record$series, function(series) series$name %||% NA), collapse = ", ")
              } else {
                NA
              }
            } else if (is.atomic(record$series)) {
              as.character(record$series)
            } else {
              NA
            }
          } else {
            NA
          }
        }, error = function(e) NA)
      )
    })

    # Convert the extracted data into a tibble for easy analysis
    tibble_results <- tibble::as_tibble(do.call(rbind, lapply(data, function(x) unlist(x, recursive = FALSE))))

    # Attach the language attribute to the tibble
    attr(tibble_results, "language") <- lng

    return(tibble_results)

  } else {
    # Handle API errors with detailed messages
    error_message <- sprintf("Failed to perform the search. Status code: %d - Response: %s",
                             httr::status_code(response), httr::content(response, "text"))
    warning(error_message)
    return(NULL)
  }
}
