#' Finna Index Search with Total Limit Option
#'
#' This function retrieves records from the Finna index with an option to limit the total number of records returned.
#' The function paginates through the results, fetching records until the specified total limit is reached.
#'
#' @name search_finna
#' @param query description
#' @param type A string specifying the type of search. Options include "AllFields", "Title", "Author", "Subject". Defaults to "AllFields".
#' @param fields A vector of fields to be returned in the search results. Defaults to NULL, which returns a standard set of fields.
#' @param filters A vector of filter queries to refine the search. Defaults to NULL.
#' @param facets A vector specifying which facets to return in the results. Defaults to NULL.
#' @param facetFilters A vector of regular expressions to filter facets. Defaults to NULL.
#' @param sort A string defining the sort order of the results. Options include:
#'   \itemize{
#'     \item "relevance,id asc" (default)
#'     \item "main_date_str desc" (Year, newest first)
#'     \item "main_date_str asc" (Year, oldest first)
#'     \item "last_indexed desc" (Last modified)
#'     \item "first_indexed desc" (Last added)
#'     \item "callnumber,id asc" (Classmark)
#'     \item "author,id asc" (Author)
#'     \item "title,id asc" (Title)
#'   }
#' @param limit An integer specifying the total number of records to return across multiple pages.
#' @param lng A string for the language of returned translated strings. Options are "fi", "en-gb", "sv", "se". Defaults to "fi".
#' @param prettyPrint A logical value indicating whether to pretty-print the JSON response. Useful for debugging. Defaults to FALSE.
#' @return A tibble containing the search results with relevant fields extracted and provenance information.
#' @examples
#' search_results <- search_finna("sibelius", sort = "main_date_str desc", limit = 100)
#' print(search_results)
#' @export
search_finna <- function(query = NULL,#lookfor
                         type = "AllFields",
                         fields = NULL,
                         filters = NULL,
                         facets = NULL,
                         facetFilters = NULL,
                         sort = "relevance,id asc",
                         limit = 100,
                         lng = "fi",
                         prettyPrint = FALSE) {

  # Handle empty search queries
  # if (query == "" || is.null(query)) {
  #   warning("Error: Empty search query provided.")
  #   return(NULL)
  # }

  # Define the base URL for the search API
  base_url <- "https://api.finna.fi/v1/search"

  # Initialize empty list to store all records
  all_data <- list()

  # Define the pagination parameters
  total_fetched <- 0
  page <- 1
  records_per_page <- 100  # Fetch 100 records per page for efficiency

  while (total_fetched < limit) {
    # Calculate the remaining number of records to fetch
    remaining_to_fetch <- min(records_per_page, limit - total_fetched)

    # Construct the query parameters for the current page
    query_params <- list(
      lookfor = query,
      type = type,
      `field[]` = fields,
      `filter[]` = filters,
      `facet[]` = facets,
      `facetFilter[]` = facetFilters,
      sort = sort,
      page = page,
      limit = remaining_to_fetch,  # Set the page limit to fetch only the remaining records
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
      if (is.null(records) || length(records) == 0) {
        break  # No more records, stop fetching
      }

      # Add the records to the all_data list
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

      all_data <- c(all_data, data)

      # Update the total number of fetched records
      total_fetched <- total_fetched + length(records)
      page <- page + 1

    } else {
      # Handle API errors with detailed messages
      error_message <- sprintf("Failed to perform the search. Status code: %d - Response: %s",
                               httr::status_code(response), httr::content(response, "text"))
      warning(error_message)
      return(NULL)
    }
  }

  # Convert the list of extracted data into a tibble for easy analysis
  tibble_results <- tibble::as_tibble(do.call(rbind, lapply(all_data, function(x) unlist(x, recursive = FALSE))))

  # Attach the language attribute to the tibble
  attr(tibble_results, "language") <- lng
  #cat("Data retrieved from Finna API (https://www.finna.fi) - metadata licensed under CC0.\n")
  return(tibble_results)
}
