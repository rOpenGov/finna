test_that("integrate_metadata successfully merges two datasets by the specified key", {

  # Example first dataset (Finna metadata)
  finna_data <- tibble::tibble(
    Title = c("Sibelius Symphony No. 5", "Finlandia", "Karelia Suite"),
    Author = c("Jean Sibelius", "Jean Sibelius", "Jean Sibelius"),
    Year = c(1915, 1899, 1893)
  )

  # Example second dataset (other dataset)
  other_data <- tibble::tibble(
    Title = c("Sibelius Symphony No. 5", "Finlandia", "Valse Triste"),
    Rating = c(5, 4, 3)
  )

  # Call the function to integrate the datasets
  integrated_data <- integrate_metadata(finna_data, other_data, key = "Title")

  # Check that the result is a tibble
  expect_s3_class(integrated_data, "tbl_df")

  # Check that the correct number of rows are present (full join should result in 4 rows)
  expect_equal(nrow(integrated_data), 4)

  # Check that the columns are correctly merged
  expect_named(integrated_data, c("Title", "Author", "Year", "Rating"))

  # Check that specific rows are correctly merged
  expect_equal(integrated_data$Rating[integrated_data$Title == "Sibelius Symphony No. 5"], 5)
  expect_true(is.na(integrated_data$Rating[integrated_data$Title == "Karelia Suite"]))  # No rating for Karelia Suite
  expect_true(is.na(integrated_data$Author[integrated_data$Title == "Valse Triste"]))  # No author for Valse Triste
})
