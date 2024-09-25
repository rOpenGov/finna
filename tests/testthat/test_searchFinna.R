# tests/testthat/test-search_finna.R

test_that("search_finna performs a basic search correctly", {
  # Test 1: Basic Search
  search_results <- search_finna("sibelius", type = "Author")
  expect_true(is.data.frame(search_results))
  expect_gt(nrow(search_results), 0)
  expect_true("Title" %in% names(search_results))
  expect_true("Author" %in% names(search_results))
  expect_true("Year" %in% names(search_results))

  # Test 2: Check error handling with an invalid query
  search_results_error <- suppressWarnings(search_finna(""))
  expect_null(search_results_error) # Expect NULL because the query is invalid and should cause an error

  # Test 3: Search with additional filters
  search_results_filtered <- search_finna("sibelius", filters = c("search_daterange_mv:[1900 TO 1950]"))
  expect_true(is.data.frame(search_results_filtered))
  expect_gt(nrow(search_results_filtered), 0)

  # Test 4: Check different parameters, ensure that the number of rows returned is <= limit
  search_results_params <- search_finna("sibelius", type = "Title", limit = 100, lng = "en-gb")
  expect_true(is.data.frame(search_results_params))
  expect_gte(nrow(search_results_params), 100) # The number of results should be less than or equal to the limit
  expect_equal(attr(search_results_params, "language"), "en-gb")
})

