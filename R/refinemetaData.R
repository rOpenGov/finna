#' Refine Finna Metadata
#'
#' Refines the Finna metadata tibble by keeping relevant fields and cleaning up missing values.
#'
#' @param data A tibble containing raw Finna metadata.
#' @return A tibble with selected, cleaned metadata fields.
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refine_metadata(sibelius_data)
#'
refine_metadata <- function(data) {
  refined <- data %>%
    mutate(
      Title = if_else(is.na(Title), "Unknown Title", Title),
      Author = if_else(is.na(Author), "Unknown Author", Author),
      Year = if_else(is.na(Year), "Unknown Year", Year),
      Language = if_else(is.na(Language), "Unknown Language", Language),
      Formats = if_else(is.na(Formats), "Unknown Format", Formats),
      Subjects = if_else(is.na(Subjects), "Unknown Subjects", Subjects),
      Library = if_else(is.na(Library), "Unknown Library", Library),
      Series = if_else(is.na(Series), "Unknown Series", Series)
    ) %>%
    select(Title, Author, Year, Language, Formats, Subjects, Library, Series)

  return(refined)
}
