test_that("search_finna_from_file processes file content and performs search correctly", {
  # Create a temporary file with test content
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  writeLines(c("This is a test query", "with multiple lines"), temp_file)

  # Perform the search
  results <- suppressWarnings(search_finna_from_file(temp_file, limit = 5, lng = "en", query_limit = 50))

  expect_true(inherits(results, "tbl_df"), "The result should be a tibble.")
  expect_gt(nrow(results), 0, "The number of rows should be greater than 0.")
  expect_true("Title" %in% names(results), "The result should contain a 'Title' column.")
  expect_true("Author" %in% names(results), "The result should contain an 'Author' column.")
})

test_that("search_finna_from_file handles empty file gracefully", {
  # Create a temporary empty file
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  writeLines(character(0), temp_file)

  expect_error(
    search_finna_from_file(temp_file),
    regexp = "No results found for the given file content.",
    info = "The function should return an error for an empty file."
  )
})

test_that("search_finna_from_file handles non-existent file", {
  expect_error(
    search_finna_from_file("non_existent_file.txt"),
    regexp = "Failed to read the file: cannot open file",
    info = "The function should return an error when the file does not exist."
  )
})


test_that("search_finna_from_file respects query limit and splits long text", {
  # Create a temporary file with long text
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  writeLines(paste(rep("A very long text query that exceeds the query limit.", 10), collapse = " "), temp_file)

  # Perform the search
  results <- suppressWarnings(search_finna_from_file(temp_file, query_limit = 50))

  expect_true(inherits(results, "tbl_df"), "The result should be a tibble.")
  expect_gt(nrow(results), 0, "The number of rows should be greater than 0.")
})

# test_that("search_finna_from_file handles search_finna returning no results", {
#   # Create a temporary file with test content
#   temp_file <- tempfile()
#   on.exit(unlink(temp_file))
#   writeLines("This is a test query with no results", temp_file)
#
#   expect_error(
#     suppressWarnings(search_finna_from_file(temp_file)),
#     regexp = "No results found for the given file content.",
#     info = "The function should return an error when no results are found."
#   )
# })


