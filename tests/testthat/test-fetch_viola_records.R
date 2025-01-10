test_that("fetch_viola_records handles valid year ranges correctly", {
  # Mock search_finna to return predictable results
  mockery::stub(fetch_viola_records, "search_finna", function(query, filters, limit) {
    tibble::tibble(
      id = 1:5,
      Title = paste("Record", 1:5),
      main_date_str = c("2001", "2002", "2003", "2004", "2005")
    )
  })

  result <- fetch_viola_records(
    year_ranges = list(c(2000, 2005)),
    limit_per_query = 5
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 5)
  expect_true("id" %in% names(result))
  expect_true("Title" %in% names(result))
})

test_that("fetch_viola_records validates year ranges", {
  expect_error(fetch_viola_records(year_ranges = list(c(2005, 2000))), "Each year range must be a numeric vector")
})

test_that("fetch_viola_records respects total_limit", {
  mockery::stub(fetch_viola_records, "search_finna", function(query, filters, limit) {
    tibble::tibble(
      id = 1:10,
      Title = paste("Record", 1:10),
      main_date_str = c("2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")
    ) %>% dplyr::slice_head(n = limit)
  })

  result <- fetch_viola_records(
    year_ranges = list(c(2000, 2010)),
    total_limit = 5,
    limit_per_query = 10
  )

  expect_equal(nrow(result), 5)
})


test_that("fetch_viola_records fetches NA records when include_na is TRUE", {
  mockery::stub(fetch_viola_records, "search_finna", function(query, filters, limit) {
    if ("-main_date_str:*" %in% filters) {
      tibble::tibble(
        id = 6:10,
        Title = paste("Record", 6:10),
        main_date_str = NA
      )
    } else {
      tibble::tibble(
        id = 1:5,
        Title = paste("Record", 1:5),
        main_date_str = c("2001", "2002", "2003", "2004", "2005")
      )
    }
  })

  result <- fetch_viola_records(
    year_ranges = list(c(2000, 2005)),
    include_na = TRUE,
    limit_per_query = 5
  )

  expect_equal(nrow(result), 10)
  expect_true(any(is.na(result$main_date_str)))
})

test_that("fetch_viola_records handles empty results gracefully", {
  mockery::stub(fetch_viola_records, "search_finna", function(query, filters, limit) {
    tibble::tibble()
  })

  result <- fetch_viola_records(
    year_ranges = list(c(2000, 2005)),
    limit_per_query = 5
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})
