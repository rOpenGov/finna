test_that("refine_metadata returns a tibble with selected columns", {
  # Example input tibble including all the necessary columns
  input_data <- tibble::tibble(
    Title = c("Sibelius", NA),
    Author = c("Jean Sibelius", NA),
    Year = c("1865", NA),
    Formats = c("Book", NA),
    Library = c("National Library", NA),
    Language = c("Finnish", NA),  # Add the Language column
    Subjects = c("Music", NA),    # Add the Subjects column
    Series = c("Series 1", NA)    # Add the Series column
  )

  # Call the function
  refined_data <- refine_metadata(input_data)

  # Check that the result is a tibble
  expect_s3_class(refined_data, "tbl_df")

  # Check that the refined tibble has the correct columns
  expect_named(refined_data, c("Title", "Author", "Year", "Language","Formats", "Subjects", "Library", "Series"))

  # Check that missing values were replaced correctly
  expect_equal(refined_data$Title[2], "Unknown Title")
  expect_equal(refined_data$Author[2], "Unknown Author")
  expect_equal(refined_data$Year[2], "Unknown Year")
  expect_equal(refined_data$Formats[2], "Unknown Format")
  expect_equal(refined_data$Library[2], "Unknown Library")
  expect_equal(refined_data$Language[2], "Unknown Language")  # Check for Language
  expect_equal(refined_data$Subjects[2], "Unknown Subjects")  # Check for Subjects
  expect_equal(refined_data$Series[2], "Unknown Series")      # Check for Series
})
