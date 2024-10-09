
test_that("analyze_metadata returns correct format, year, and author distributions", {
  # Example input tibble
  input_data <- tibble::tibble(
    Formats = c("Book", "Sheet Music", "Book"),
    Year = c("1865", "1893", "1865"),
    Author = c("Jean Sibelius", "Jean Sibelius", "Jean Sibelius")
  )

  # Call the function
  analysis_results <- analyze_metadata(input_data)

  # Check that the result is a list with correct elements
  expect_type(analysis_results, "list")
  expect_named(analysis_results, c("format_distribution", "year_distribution", "author_distribution"))

  # Check format distribution
  expect_equal(analysis_results$format_distribution$Formats[1], "Book")
  expect_equal(analysis_results$format_distribution$n[1], 2)

  # Check year distribution
  expect_equal(analysis_results$year_distribution$Year[1], "1865")
  expect_equal(analysis_results$year_distribution$n[1], 2)

  # Check author distribution
  expect_equal(analysis_results$author_distribution$Author[1], "Jean Sibelius")
  expect_equal(analysis_results$author_distribution$n[1], 3)
})
