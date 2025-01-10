#' @title Search Finna using text from a file (extended version)
#'
#' @description
#' Reads text from a file, processes it, and searches it in Finna using the `search_finna` function.
#'
#' @param file_path The path to the text file.
#' @param limit The number of results to return from Finna. Defaults to 10.
#' @param lng Language for returned translated strings. Defaults to "fi".
#' @param query_limit The maximum length of the query string allowed for the API. Defaults to 500 characters.
#' @return A tibble containing the Finna search results.
#' @export
search_finna_from_file <- function(file_path, limit = 10, lng = "fi", query_limit = 500) {
  # Read the content of the file
  file_content <- tryCatch({
    readLines(file_path, warn = FALSE)
  }, error = function(e) {
    stop("Failed to read the file: ", e$message, call. = FALSE)
  }, warning = function(w) {
    stop("Failed to read the file: ", w$message, call. = FALSE)
  })

  # Concatenate the lines into a single string (in case it's multi-line)
  full_text <- paste(file_content, collapse = " ")

  # Preprocess the text (e.g., remove non-alphanumeric characters, excessive whitespace)
  clean_text <- gsub("[^[:alnum:][:space:]]", "", full_text)
  clean_text <- gsub("\\s+", " ", clean_text)

  # Split the text into chunks that fit within the query limit
  text_chunks <- strsplit(clean_text, "(?<=.{500})\\s", perl = TRUE)[[1]]

  # Initialize an empty tibble to store results
  all_results <- tibble::tibble()

  # Loop through the chunks and perform a search for each
  for (chunk in text_chunks) {
    query <- substr(chunk, 1, query_limit)  # Truncate to query limit if necessary
    cat("Performing search with query:", query, "\n")  # For logging

    # Perform a search on Finna using the content from the chunk
    results <- tryCatch({
      search_finna(query, limit = limit, lng = lng)
    }, error = function(e) {
      warning("Search failed for query: ", query, ". Error: ", e$message, call. = FALSE)
      return(tibble::tibble())
    })

    # Append valid results to all_results
    if (nrow(results) > 0) {
      all_results <- dplyr::bind_rows(all_results, results)
    }
  }

  # Return all combined results or throw an error if no results found
  if (nrow(all_results) == 0) {
    stop("No results found for the given file content.", call. = FALSE)
  } else {
    return(all_results)
  }
}


# Example usage:
# Assuming the file contains the search query
#file_path <- "/Users/akasia/textfile.txt"
#finna_results <- search_finna_from_file(file_path, limit = 20, lng = "en-gb")
#print(finna_results)
