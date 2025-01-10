#' @title Analyze Refined Finna Metadata
#'
#' @description
#' Performs basic analysis on Finna metadata, summarizing the distribution of formats, years, and authors.
#'
#' @param metadata A tibble containing refined Finna metadata.
#' @return A list of tibbles with summaries of formats, years, and authors.
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refined_data <- refine_metadata(sibelius_data)
#' analyze_metadata(refined_data)
#'
analyze_metadata <- function(metadata) {
  format_distribution <- metadata %>%
    count(Formats, sort = TRUE)

  year_distribution <- metadata %>%
    filter(!is.na(Year)) %>%
    count(Year, sort = TRUE)

  author_distribution <- metadata %>%
    filter(!is.na(Author)) %>%
    count(Author, sort = TRUE)

  list(
    format_distribution = format_distribution,
    year_distribution = year_distribution,
    author_distribution = author_distribution
  )
}
# Declare global variables to avoid warnings during checks
utils::globalVariables(c("Formats", "Year", "Author"))
