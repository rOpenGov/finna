#' @title Visualize Year Distribution
#'
#' @description
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

#' Visualize Year Distribution (Line Plot)
#'
#' Creates a line plot showing the distribution of records by year.
#'
#' @param metadata A tibble containing refined Finna metadata, with a "Year" column.
#' @return A ggplot2 object representing the line plot.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_year_distribution_line(refined_data)
visualize_year_distribution_line <- function(metadata) {
  # Convert the Year to numeric
  metadata <- metadata %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(!is.na(Year))

  # Count the number of records by year
  year_distribution <- metadata %>%
    count(Year, sort = TRUE)

  # Plot the year distribution as a line plot
  ggplot(year_distribution, aes(x = Year, y = n)) +
    geom_line(color = "steelblue", linewidth = 1) +
    theme_minimal() +
    labs(
      title = "Yearly Distribution of Records",
      x = "Year",
      y = "Number of Records"
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
#' @import stringr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_top_20_titles(refined_data)
visualize_top_20_titles <- function(metadata) {
  # Clean the titles: Convert to lowercase, remove punctuation, and trim whitespace
  metadata <- metadata %>%
    mutate(Title = tolower(Title),  # Convert to lowercase
           Title = stringr::str_trim(Title),  # Trim leading and trailing whitespace
           Title = stringr::str_squish(Title),  # Remove extra spaces between words
           Title = stringr::str_replace_all(Title, "[[:punct:]]", ""))  # Remove punctuation

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

#' Visualize Heatmap of Titles by Year
#'
#' Creates a heatmap showing the most frequent titles and their occurrence over time.
#'
#' @param metadata A tibble containing refined Finna metadata, with "Title" and "Year" columns.
#' @return A ggplot2 object showing the heatmap of title frequency by year.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_title_year_heatmap(refined_data)
visualize_title_year_heatmap <- function(metadata) {
  # Clean the data
  metadata <- metadata %>%
    mutate(Title = tolower(Title),  # Convert to lowercase
           Title = stringr::str_trim(Title),  # Trim leading/trailing whitespace
           Year = as.numeric(Year)) %>% # Convert year to numeric
    filter(!is.na(Year), !is.na(Title)) %>%
    count(Title, Year)

  # Filter top 20 titles by total count
  top_titles <- metadata %>%
    group_by(Title) %>%
    summarise(total = sum(n)) %>%
    top_n(20, total) %>%
    pull(Title)

  # Filter metadata to include only top titles
  metadata_filtered <- metadata %>% filter(Title %in% top_titles)

  # Plot the heatmap
  ggplot(metadata_filtered, aes(x = Year, y = reorder(Title, n), fill = n)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal() +
    labs(
      title = "Heatmap of Top 20 Titles by Year",
      x = "Year",
      y = "Title",
      fill = "Count"
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

#' Visualize Correlation Between Formats and Libraries
#'
#' Creates a heatmap showing the correlation between formats and libraries.
#'
#' @param metadata A tibble containing refined Finna metadata, with "Formats" and "Library" columns.
#' @return A ggplot2 object showing the heatmap of format-library correlation.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_format_library_correlation(refined_data)
visualize_format_library_correlation <- function(metadata) {
  # Clean and count format-library combinations
  format_library_dist <- metadata %>%
    mutate(Formats = tolower(Formats),
           Library = tolower(Library)) %>%
    count(Formats, Library) %>%
    filter(!is.na(Formats), !is.na(Library))

  # Plot the heatmap
  ggplot(format_library_dist, aes(x = Formats, y = Library, fill = n)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "darkorange") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels
    labs(
      title = "Correlation Between Formats and Libraries",
      x = "Format",
      y = "Library",
      fill = "Count"
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

#' Visualize Word Cloud of Titles or Subjects with Stop Words Removal
#'
#' Creates a word cloud showing the frequency of words in titles or subjects,
#' removing common Finnish, Swedish, and English stop words.
#'
#' @param metadata A tibble containing refined Finna metadata, with "Title" or "Subjects" column.
#' @param column The column to visualize as a word cloud (e.g., "Title" or "Subjects").
#' @return A word cloud plot of the most frequent words.
#' @import wordcloud2
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @import tm
#' @export
#' @examples
#' library(finna)
#' music_data <- search_finna("music")
#' refined_data <- refine_metadata(music_data)
#' visualize_word_cloud(refined_data, "Title")
visualize_word_cloud <- function(metadata, column = "Title") {

  # Convert the column name to symbol
  column <- rlang::ensym(column)

  # Extract the text data from the specified column
  text_data <- metadata %>%
    select(!!column) %>%
    pull(!!column)

  # Create a text corpus
  corpus <- Corpus(VectorSource(text_data))

  # List of common Finnish, Swedish, and English stop words
  finnish_stopwords <- stopwords::stopwords("fi")
  swedish_stopwords <- stopwords::stopwords("sv")
  english_stopwords <- stopwords::stopwords("en")

  # Clean the text: lowercase, remove punctuation, numbers, extra whitespace, and stopwords
  corpus <- tm_map(corpus, content_transformer(tolower))             # Convert to lowercase
  corpus <- tm_map(corpus, removePunctuation)                        # Remove punctuation
  corpus <- tm_map(corpus, removeNumbers)                            # Remove numbers
  corpus <- tm_map(corpus, stripWhitespace)                          # Remove extra whitespace
  corpus <- tm_map(corpus, removeWords, c(finnish_stopwords,         # Remove stop words (Finnish, Swedish, English)
                                          swedish_stopwords,
                                          english_stopwords))

  # Create a term-document matrix
  tdm <- TermDocumentMatrix(corpus)

  # Convert the term-document matrix to a matrix
  tdm_matrix <- as.matrix(tdm)

  # Sum the word frequencies
  word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)

  # Convert to a data frame
  word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

  # Generate word cloud using wordcloud2
  wordcloud2::wordcloud2(data = word_freq_df)
}



#' Visualize Format Distribution as Pie Chart
#'
#' Creates a pie chart showing the distribution of records by formats.
#'
#' @param metadata A tibble containing refined Finna metadata, with a "Formats" column.
#' @return A ggplot2 object showing the pie chart of format distribution.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' visualize_format_distribution_pie(refined_data)
visualize_format_distribution_pie <- function(metadata) {
  # Clean and count the format distribution
  format_distribution <- metadata %>%
    mutate(Formats = tolower(Formats)) %>%
    count(Formats, sort = TRUE)

  # Plot the pie chart
  ggplot(format_distribution, aes(x = "", y = n, fill = Formats)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +  # Remove background and axis
    labs(
      title = "Distribution of Formats",
      fill = "Formats"
    )
}




