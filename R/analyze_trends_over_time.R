#' Analyze Trends Over Time with Binned Years (Decades)
#'
#' This function analyzes how search results for a given query have trended over time, binned by decades.
#' It plots the number of records found for each decade, allowing users to see trends over larger time intervals.
#'
#' @param data A tibble containing Finna search results with a `Year` column (as character or numeric).
#' @param query A search query string (optional) to label the plot.
#' @return A ggplot2 plot showing the trend of records over time.
#' @importFrom dplyr mutate filter group_by summarise arrange
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @export
#' @examples
#' finna_data <- search_finna("Sibelius")
#' trends <- analyze_trends_over_time_binned(finna_data, "Sibelius")
#' print(trends)
analyze_trends_over_time_binned <- function(data, query = "Records Over Time") {
  if (!"Year" %in% names(data)) {
    stop("The data must contain a 'Year' column for time analysis.")
  }

  # Clean and convert the Year column to numeric (from character if necessary)
  data <- data %>%
    filter(!is.na(Year) & Year != "") %>%  # Filter out missing or empty years
    filter(grepl("^\\d{4}$", Year)) %>%    # Keep only rows where Year is a 4-digit number
    mutate(Year = as.numeric(Year)) %>%    # Convert Year to numeric
    filter(!is.na(Year) & Year > 0)        # Filter out invalid years (e.g., after coercion to numeric)

  # Create a new column for decade
  data <- data %>%
    mutate(Decade = floor(Year / 10) * 10) %>%
    filter(!is.na(Decade)) # Remove rows without valid years

  # Group by Decade and count the number of records per decade
  decade_counts <- data %>%
    group_by(Decade) %>%
    summarise(Count = n()) %>%
    arrange(Decade)

  # Plot the trend over time (by decades)
  ggplot2::ggplot(decade_counts, ggplot2::aes(x = Decade, y = Count)) +
    ggplot2::geom_line(color = "blue") +
    ggplot2::labs(
      title = paste("Trends in Records Over Time for", query),
      x = "Decade",
      y = "Number of Records"
    ) +
    ggplot2::theme_minimal()
}

