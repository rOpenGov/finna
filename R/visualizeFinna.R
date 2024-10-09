#' Visualize Year Distribution
#'
#' Creates a bar plot of the publication year distribution.
#'
#' @param year_data A tibble containing year distribution data (Year, count).
#' @return A ggplot2 object representing the bar plot.
#' @import ggplot2
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' analysis_results <- analyze_metadata(refined_data)
#' visualize_year_distribution(analysis_results$year_distribution)
visualize_year_distribution <- function(year_data) {
  ggplot(year_data, aes(x = Year, y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    theme_minimal() +
    labs(
      title = "Publication Distribution by Year",
      x = "Year",
      y = "Count"
    )
}

#' Visualize Top-20 Titles by Count
#'
#' Creates a bar plot showing the top-20 most frequent titles and their counts.
#'
#' @param metadata A tibble containing refined Finna metadata, with a "Title" column.
#' @return A ggplot2 object showing the bar plot of the top-20 titles.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_top_20_titles(refined_data)
visualize_top_20_titles <- function(metadata) {
  # Convert titles to lowercase for case-insensitive comparison
  metadata <- metadata %>%
    mutate(Title = tolower(Title))

  # Calculate the top-20 titles
  top_titles <- metadata %>%
    count(Title, sort = TRUE) %>%
    top_n(20, n)

  # Plot the top-20 titles
  ggplot(top_titles, aes(x = reorder(Title, n), y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +  # Flip to make the titles more readable
    theme_minimal() +
    labs(
      title = "Top 20 Titles by Count",
      x = "Title",
      y = "Count"
    )
}


#' Visualize Distribution by Formats
#'
#' Creates a bar plot showing the distribution of records by their formats.
#'
#' @param metadata A tibble containing refined Finna metadata, with a "Formats" column.
#' @return A ggplot2 object showing the distribution of records by formats.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_format_distribution(refined_data)
visualize_format_distribution <- function(metadata) {
  # Convert formats to lowercase for case-insensitive comparison
  metadata <- metadata %>%
    mutate(Formats = tolower(Formats))

  # Count the number of records by format
  format_distribution <- metadata %>%
    count(Formats, sort = TRUE)

  # Plot the distribution by formats
  ggplot(format_distribution, aes(x = reorder(Formats, n), y = n)) +
    geom_bar(stat = "identity", fill = "darkorange") +
    coord_flip() +  # Flip for readability
    theme_minimal() +
    labs(
      title = "Distribution by Formats",
      x = "Formats",
      y = "Count"
    )
}


#' Visualize Distribution by Libraries
#'
#' Creates a bar plot showing the distribution of records by libraries.
#'
#' @param metadata A tibble containing refined Finna metadata, with a "Library" column.
#' @return A ggplot2 object showing the distribution of records by libraries.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_library_distribution(refined_data)
visualize_library_distribution <- function(metadata) {
  # Convert libraries to lowercase for case-insensitive comparison
  metadata <- metadata %>%
    mutate(Library = tolower(Library))

  # Count the number of records by library and get the top 20
  top_libraries <- metadata %>%
    count(Library, sort = TRUE) %>%
    top_n(20, n)

  # Create a function to limit library names to two words
  limit_words <- function(name) {
    words <- unlist(strsplit(name, " "))
    return(paste(words[1:min(2, length(words))], collapse = " "))
  }

  # Apply the function to truncate the library names to two words
  top_libraries <- top_libraries %>%
    mutate(Library = sapply(Library, limit_words))

  # Plot the top-20 libraries
  ggplot(top_libraries, aes(x = reorder(Library, n), y = n)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    coord_flip() +  # Flip for readability
    theme_minimal() +
    labs(
      title = "Top 20 Libraries by Record Count",
      x = "Library",
      y = "Count"
    )
}

#' Visualize Distribution by Authors
#'
#' Creates a bar plot showing the distribution of records by authors.
#'
#' @param metadata A tibble containing refined Finna metadata, with an "Author" column.
#' @return A ggplot2 object showing the distribution of records by authors.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_author_distribution(refined_data)
visualize_author_distribution <- function(metadata) {
  # Convert authors to lowercase for case-insensitive comparison
  metadata <- metadata %>%
    mutate(Author = tolower(Author))

  # Count the number of records by author and get the top 20
  top_authors <- metadata %>%
    count(Author, sort = TRUE) %>%
    top_n(20, n)

  # Create a function to limit author names to two words
  limit_words <- function(name) {
    words <- unlist(strsplit(name, " "))
    return(paste(words[1:min(2, length(words))], collapse = " "))
  }

  # Apply the function to truncate the author names to two words
  top_authors <- top_authors %>%
    mutate(Author = sapply(Author, limit_words))

  # Plot the top-20 authors
  ggplot(top_authors, aes(x = reorder(Author, n), y = n)) +
    geom_bar(stat = "identity", fill = "purple") +
    coord_flip() +  # Flip for readability
    theme_minimal() +
    labs(
      title = "Top 20 Authors by Record Count",
      x = "Author",
      y = "Count"
    )
}

#' Visualize Distribution by Subjects
#'
#' Creates a bar plot showing the distribution of records by subjects.
#'
#' @param metadata A tibble containing refined Finna metadata, with a "Subjects" column.
#' @return A ggplot2 object showing the distribution of records by subjects.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_subject_distribution(refined_data)
visualize_subject_distribution <- function(metadata) {
  # Convert subjects to lowercase for case-insensitive comparison
  metadata <- metadata %>%
    mutate(Subjects = tolower(Subjects))

  # Count the number of records by subject and get the top 20
  top_subjects <- metadata %>%
    count(Subjects, sort = TRUE) %>%
    top_n(20, n)

  # Create a function to limit subject names to two words
  limit_words <- function(name) {
    words <- unlist(strsplit(name, " "))
    return(paste(words[1:min(2, length(words))], collapse = " "))
  }

  # Apply the function to truncate the subject names to two words
  top_subjects <- top_subjects %>%
    mutate(Subjects = sapply(Subjects, limit_words))

  # Plot the top-20 subjects
  ggplot(top_subjects, aes(x = reorder(Subjects, n), y = n)) +
    geom_bar(stat = "identity", fill = "darkred") +
    coord_flip() +  # Flip for readability
    theme_minimal() +
    labs(
      title = "Top 20 Subjects by Record Count",
      x = "Subjects",
      y = "Count"
    )
}



