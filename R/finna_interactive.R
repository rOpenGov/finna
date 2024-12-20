#' Interactive Finna Search and Data Download
#'
#' @description
#' Provides an interactive interface to search, select, and download datasets from Finna API.
#'
#' @seealso [search_finna()], [fetch_finna_collection()], [finna_cite()]
#' @return A dataframe containing the selected dataset or downloaded data.
#' @importFrom utils menu
#' @importFrom dplyr select arrange desc
#' @importFrom glue glue
#' @export
finna_interactive <- function() {
  # Collection selection options
  collection_options <- c(
    "Finnan oma collections (All content) - ''",
    "Dublin Core collections (All content) - 'oai_dc'",
    "Mostly library catalogs - 'marc21'",
    "Archival material (old version) - 'oai_ead'",
    "Archival material (new version) - 'oai_ead3'",
    "Material of the National Audiovisual Institute - 'oai_forward'",
    "Museums - 'record_format:lido'",
    "Repositories, thesis, library material - 'oai_qdc'"
  )

  # Map collection options to IDs
  collection_ids <- c("", "oai_dc", "marc21", "oai_ead", "oai_ead3",
                      "oai_forward", "record_format:lido", "oai_qdc")

  # Prompt user to select a collection
  collection_index <- menu(collection_options, title = "Select a Collection")
  if (collection_index == 0) {
    message("No collection selected. Exiting.")
    return(invisible())
  }

  # Match selected collection ID
  selected_collection <- collection_ids[collection_index]
  message(glue("You selected: {collection_options[collection_index]}"))

  # Automatically use the selected collection ID as the query
  query <- selected_collection

  # Fetch initial search results with the selected collection
  results <- fetch_finna_collection(query = query, lng = selected_collection)

  if (nrow(results) > 0) {
    # Display results
    print(glue("Total results found: {nrow(results)}"))
    formatted_results <- results %>%
      dplyr::select(value, translated, count, href) %>%
      dplyr::arrange(desc(count))
    print(formatted_results)

    # Allow user to select a dataset
    dataset_titles <- results$translated
    dataset_index <- menu(c(dataset_titles, "No"), title = "Select a dataset of interest:")
    if (dataset_index == length(dataset_titles) + 1) {
      message("No dataset selected. Exiting.")
      return(invisible())
    }

    # Confirm user's choice
    selected_data <- results[dataset_index, ]
    confirm_choice <- menu(c("Yes", "No"), title = "Is this the correct dataset?")
    if (confirm_choice == 2) {
      message("Dataset not confirmed. Exiting.")
      return(invisible())
    }

    # Generate and print citation using dataset index
    print_citation <- menu(c("Yes", "No"), title = "Print dataset citation?")
    if (print_citation == 1) {
      finna_cite(result = results, index = dataset_index)  # Use numeric index
    }

    # Download dataset
    download_selection <- menu(c("Yes", "No"), title = "Download the dataset?")
    if (download_selection == 1) {
      dataset_href <- selected_data$href
      url_params <- sub(".*\\?", "", dataset_href)
      param_list <- strsplit(url_params, "&")[[1]]

      # Extract parameters
      params <- list()
      for (param in param_list) {
        key_value <- strsplit(param, "=")[[1]]
        params[[key_value[1]]] <- URLdecode(key_value[2])
      }

      # Use search_finna to download data
      finna_data <- search_finna(
        query = params[["lookfor"]],
        type = params[["type"]],
        filters = params[["filter[]"]],
        limit = 100 # Optional: Adjust as needed
      )

      if (nrow(finna_data) > 0) {
        message("Data downloaded successfully.")
        return(finna_data)
      } else {
        message("No data found for the given parameters.")
        return(invisible())
      }
    }
  } else {
    stop(glue("No data found for the selected collection: {query}"))
  }
}
