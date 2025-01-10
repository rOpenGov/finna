test_that("save_for_offline saves valid data to a file", {
  # Create a temporary data frame
  test_data <- tibble::tibble(
    id = 1:5,
    Title = paste("Title", 1:5),
    Author = paste("Author", 1:5)
  )

  # Specify a temporary file name
  temp_file <- "test_search_results"

  # Run the function
  expect_message(
    save_for_offline(test_data, temp_file),
    regexp = "Search results saved successfully to data/test_search_results.rds"
  )

  # Check if the file exists
  expect_true(file.exists(file.path("data", paste0(temp_file, ".rds"))))

  # Clean up
  unlink("data", recursive = TRUE)
})

test_that("save_for_offline adds .rds extension if missing", {
  test_data <- tibble::tibble(
    id = 1:3,
    Title = c("A", "B", "C"),
    Author = c("X", "Y", "Z")
  )

  temp_file <- "test_results"

  # Save the file without ".rds" in the name
  save_for_offline(test_data, temp_file)

  # Check if the file exists with the correct extension
  expect_true(file.exists(file.path("data", paste0(temp_file, ".rds"))))

  # Clean up
  unlink("data", recursive = TRUE)
})

test_that("save_for_offline throws error for empty or NULL data", {
  # Create an empty tibble
  empty_data <- tibble::tibble()

  # Expect an error for empty data
  expect_error(
    save_for_offline(empty_data, "empty_data_test"),
    regexp = "No data to save. Ensure that the search results are valid."
  )

  # Expect an error for NULL data
  expect_error(
    save_for_offline(NULL, "null_data_test"),
    regexp = "No data to save. Ensure that the search results are valid."
  )
})

test_that("load_offline_data loads valid data from a file", {
  # Create a temporary data frame
  test_data <- tibble::tibble(
    id = 1:3,
    Title = c("A", "B", "C"),
    Author = c("X", "Y", "Z")
  )

  # Save the test data
  temp_file <- "test_load_results"
  save_for_offline(test_data, temp_file)

  # Load the saved data
  loaded_data <- load_offline_data(temp_file)

  # Check if the data matches
  expect_equal(loaded_data, test_data)

  # Clean up
  unlink("data", recursive = TRUE)
})

test_that("load_offline_data adds .rds extension if missing", {
  # Create a temporary data frame
  test_data <- tibble::tibble(
    id = 1:4,
    Title = c("D", "E", "F", "G"),
    Author = c("P", "Q", "R", "S")
  )

  # Save the test data
  temp_file <- "test_extension"
  save_for_offline(test_data, temp_file)

  # Load the data without providing .rds
  loaded_data <- load_offline_data("test_extension")

  # Check if the data matches
  expect_equal(loaded_data, test_data)

  # Clean up
  unlink("data", recursive = TRUE)
})

test_that("load_offline_data throws error for non-existent file", {
  expect_error(
    load_offline_data("non_existent_file"),
    regexp = "File not found. Please ensure the file exists and try again."
  )
})
