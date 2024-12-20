# Define mock data for testing
mock_year_data <- tibble::tibble(
  Year = c(2000, 2001, 2002, 2003, 2004),
  n = c(5, 10, 15, 20, 25)
)

mock_metadata <- tibble::tibble(
  Year = c("2000", "2001", "2002", NA, "2003"),
  Title = c("Title One", "Title Two", "Title Three", "Title Four", "Title Five"),
  Formats = c("Book", "E-Book", "DVD", "Book", "DVD"),
  Library = c("Library A", "Library B", "Library C", "Library A", "Library B"),
  Subjects = c("Music", "Art", "Science", "History", "Music"),
  Author = c("Author One", "Author Two", "Author One", "Author Three", "Author Four")
)

mock_clean_metadata <- mock_metadata %>%
  filter(!is.na(Title) & Title != "")


# Unit Tests
test_that("visualize_year_distribution creates a bar plot for year distribution", {
  plot <- visualize_year_distribution(mock_year_data)
  expect_s3_class(plot, "gg")
  expect_true("Year" %in% names(mock_year_data))
  expect_true("n" %in% names(mock_year_data))
})

test_that("visualize_year_distribution_line creates a line plot for year distribution", {
  plot <- visualize_year_distribution_line(mock_metadata)
  expect_s3_class(plot, "gg")
  expect_true("Year" %in% names(mock_metadata))
})

test_that("visualize_top_20_titles creates a bar plot for top titles", {
  plot <- visualize_top_20_titles(mock_metadata)
  expect_s3_class(plot, "gg")
  expect_true("Title" %in% names(mock_metadata))
})

test_that("visualize_title_year_heatmap creates a heatmap for titles by year", {
  plot <- visualize_title_year_heatmap(mock_metadata)
  expect_s3_class(plot, "gg")
  expect_true("Title" %in% names(mock_metadata))
  expect_true("Year" %in% names(mock_metadata))
})

test_that("visualize_format_distribution creates a bar plot for formats", {
  plot <- visualize_format_distribution(mock_metadata)
  expect_s3_class(plot, "gg")
  expect_true("Formats" %in% names(mock_metadata))
})

test_that("visualize_format_library_correlation creates a heatmap for format-library correlation", {
  plot <- visualize_format_library_correlation(mock_metadata)
  expect_s3_class(plot, "gg")
  expect_true("Formats" %in% names(mock_metadata))
  expect_true("Library" %in% names(mock_metadata))
})

test_that("visualize_library_distribution creates a bar plot for libraries", {
  plot <- visualize_library_distribution(mock_metadata)
  expect_s3_class(plot, "gg")
  expect_true("Library" %in% names(mock_metadata))
})

test_that("visualize_author_distribution creates a bar plot for authors", {
  plot <- visualize_author_distribution(mock_metadata)
  expect_s3_class(plot, "gg")
  expect_true("Author" %in% names(mock_metadata))
})

test_that("visualize_subject_distribution creates a bar plot for subjects", {
  plot <- visualize_subject_distribution(mock_metadata)
  expect_s3_class(plot, "gg")
  expect_true("Subjects" %in% names(mock_metadata))
})

# test_that("visualize_word_cloud generates a word cloud", {
#   plot <- visualize_word_cloud(mock_clean_metadata, "Title")
#   expect_s3_class(plot, "htmlwidget")
# })

test_that("visualize_format_distribution_pie creates a pie chart for formats", {
  plot <- visualize_format_distribution_pie(mock_metadata)
  expect_s3_class(plot, "gg")
  expect_true("Formats" %in% names(mock_metadata))
})
