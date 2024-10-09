
test_that("integrate_metadata merges two datasets by the specified key", {
  # Example tibbles
  metadata1 <- tibble::tibble(
    Title = c("Sibelius", "Finlandia"),
    Author = c("Jean Sibelius", "Jean Sibelius")
  )

  metadata2 <- tibble::tibble(
    Title = c("Sibelius", "Karelia Suite"),
    Year = c("1865", "1893")
  )

  # Call the function
  integrated_data <- integrate_metadata(metadata1, metadata2, key = "Title")

  # Check that the result is a tibble
  expect_s3_class(integrated_data, "tbl_df")

  # Check that the merge was successful
  expect_equal(nrow(integrated_data), 3)  # Should have 3 rows after the join
  expect_true("Year" %in% names(integrated_data))  # Check if the "Year" column is present
})
