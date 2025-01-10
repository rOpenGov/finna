test_that("analyze_trends_over_time handles missing 'Year' column gracefully", {
  data <- tibble::tibble(Title = c("Title1", "Title2"), Author = c("Author1", "Author2"))
  expect_error(
    analyze_trends_over_time(data),
    regexp = "The data must contain a 'Year' column for time analysis."
  )
})

test_that("analyze_trends_over_time handles empty data gracefully", {
  data <- tibble::tibble(Year = character())
  expect_error(
    analyze_trends_over_time(data),
    regexp = "The data contains no records to analyze."
  )
})

test_that("analyze_trends_over_time filters out invalid years", {
  data <- tibble::tibble(
    Year = c("1901", "not_a_year", NA, "2005", "", "1500"),
    Title = c("Title1", "Title2", "Title3", "Title4", "Title5", "Title6")
  )

  plot <- analyze_trends_over_time(data)
  expect_s3_class(plot, "gg")
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_true(all(plot_data$x %in% c(1500, 1900, 2000)))
})

test_that("analyze_trends_over_time correctly groups records by decade", {
  data <- tibble::tibble(
    Year = c("1901", "1902", "1910", "1915", "2000", "2005", "2020"),
    Title = c("Title1", "Title2", "Title3", "Title4", "Title5", "Title6", "Title7")
  )

  plot <- analyze_trends_over_time(data)
  expect_s3_class(plot, "gg")
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(plot_data$x, c(1900, 1910, 2000, 2020))
  expect_equal(plot_data$y, c(2, 2, 2, 1))
})

test_that("analyze_trends_over_time produces a line plot", {
  data <- tibble::tibble(
    Year = c("1800", "1805", "1810", "1815", "1900", "2000"),
    Title = c("Title1", "Title2", "Title3", "Title4", "Title5", "Title6")
  )

  plot <- analyze_trends_over_time(data)
  expect_s3_class(plot, "gg")
  layers <- ggplot2::ggplot_build(plot)$plot$layers
  expect_true(any(sapply(layers, function(layer) inherits(layer$geom, "GeomLine"))))
})

test_that("analyze_trends_over_time handles edge cases", {
  # All invalid years
  data <- tibble::tibble(
    Year = c("not_a_year", "", NA, "18000"),
    Title = c("Title1", "Title2", "Title3", "Title4")
  )
  expect_error(
    analyze_trends_over_time(data),
    regexp = "The data contains no valid years for analysis."
  )

  # All years in a single decade
  data <- tibble::tibble(
    Year = c("2000", "2001", "2002", "2003"),
    Title = c("Title1", "Title2", "Title3", "Title4")
  )
  plot <- analyze_trends_over_time(data)
  plot_data <- ggplot2::ggplot_build(plot)$data[[1]]
  expect_equal(plot_data$x, 2000)
  expect_equal(plot_data$y, 4)
})
