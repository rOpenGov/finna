#' Interactive function for Finna Search
#'
#' @description
#' A simple interactive function, that helps with searching and downloading Finna data.
#'
#' @param query A search query string for finding the dataset of interest.
#' @seealso [search_finna()]
#'
#' @importFrom utils capture.output
#' @importFrom utils menu
#' @export
finna_interactive <- function(query = NULL){

  # Language selection
  lang_selection <- switch(
    menu(c("Finnish", "English", "Swedish"),
         title = "Select language") + 1,
    return(invisible()),
    "fi",
    "en",
    "sv"
  )

  # Prompt user for search query if not provided
  if (is.null(query)) {
    search_query <- readline(prompt = "Enter search query for the data: ")
  } else {
    search_query <- query
  }

  # Execute search with search_finna function
  results <- search_finna(search_query, limit = 10, lng = lang_selection)

  if (nrow(results) > 0) {
    # Show titles and prompt user to select the dataset
    titles <- results$Title
    selection <- menu(choices = c(titles, "No"),
                      title = "Select the dataset you are interested in:") + 1

    if (selection > length(titles)) {
      return(invisible())  # Exit if 'No' is selected
    }

    selected_data <- results[selection,]
    selected_id <- selected_data$id  # Capture the selected id

    # Confirm dataset choice
    confirm_choice <- switch(
      menu(choices = c("Yes", "No"), title = "Is this the right dataset?") + 1,
      return(invisible()),
      TRUE,
      FALSE
    )

    if (!confirm_choice) {
      return(invisible())
    }

    # Display the selected metadata
    print(selected_data)

  } else {
    stop(paste0("\nNo data found with the given search query: "), search_query)
  }

  # Download the dataset
  download_selection <- switch(
    menu(choices = c("Yes", "No"),
         title = "Download the dataset?") + 1,
    return(invisible()),
    TRUE,
    FALSE
  )

  manual_selection <- FALSE

  if (download_selection) {
    manual_selection <- switch(
      menu(choices = c("Default", "Manually selected"),
           title = "Would you like to use default arguments or manually select them?") + 1,
      return(invisible()),
      FALSE,
      TRUE
    )

    if (manual_selection) {
      years_selection_begin <- readline(prompt = "Enter the beginning year for the data: ")
      years_selection_end <- readline(prompt = "Enter the ending year for the data: ")

      years_selection_begin <- as.integer(years_selection_begin)
      years_selection_end <- as.integer(years_selection_end)

      filters <- c(paste0("search_daterange_mv:\"[", years_selection_begin, " TO ", years_selection_end, "]\""))

      finna_data <- search_finna(query = search_query, filters = filters, limit = 100, lng = lang_selection)
    } else {
      finna_data <- search_finna(query = search_query, limit = 100, lng = lang_selection)
    }

    if (nrow(finna_data) > 0) {
      cat("Data downloaded successfully.")
    } else {
      cat("No data found for the given parameters.")
      return(invisible())
    }
  }

  tempfile_for_sink <- tempfile()

  # Print citation if requested
  print_citation <- switch(
    menu(choices = c("Yes", "No"),
         title = "Print dataset citation?") + 1,
    return(invisible()),
    TRUE,
    FALSE
  )

  if (print_citation) {
    # Use the captured id from the selected dataset
    citation <- finna_cite(id = selected_id, lang = lang_selection)
    capture.output(cat("#### DATASET CITATION: \n\n"),
                   file = tempfile_for_sink, append = TRUE)
    capture.output(print(citation),
                   file = tempfile_for_sink, append = TRUE)
    capture.output(cat("\n"),
                   file = tempfile_for_sink, append = TRUE)
  }

  # Print code if requested
  print_code <- switch(
    menu(choices = c("Yes", "No"),
         title = "Print the code for downloading dataset?") + 1,
    return(invisible()),
    TRUE,
    FALSE
  )

  if (print_code) {
    capture.output(cat("#### DOWNLOAD PARAMETERS: \n\n"),
                   file = tempfile_for_sink, append = TRUE)

    if (manual_selection) {
      capture.output(print(
        paste0("search_finna(query = '", search_query,
               "', filters = 'search_daterange_mv:\"[", years_selection_begin, " TO ", years_selection_end, "]\"', ",
               "limit = 100, lng = '", lang_selection, "')")
      ), file = tempfile_for_sink, append = TRUE)
    } else {
      capture.output(print(
        paste0("search_finna(query = '", search_query,
               "', limit = 100, lng = '", lang_selection, "')")
      ), file = tempfile_for_sink, append = TRUE)
    }

    capture.output(cat("\n"), file = tempfile_for_sink, append = TRUE)
  }

  if (exists("finna_data")) {
    # Show results
    cat(readLines(tempfile_for_sink), sep = "\n")
    return(finna_data)
  }

}
