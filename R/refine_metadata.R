#' @title Refine Finna Metadata
#'
#' @description
#' The `refine_metadata` function cleans and standardizes Finna metadata by:
#' - **Validating Required Fields:** Checks for the presence of key metadata fields and returns `NULL` if any are missing.
#' - **Handling Missing Values:** Replaces `NA` values in critical fields with descriptive placeholder text (e.g., "Unknown Title").
#' - **Selecting Relevant Fields:** Keeps only the following fields for streamlined analysis:
#'   - `Title`: The title of the resource.
#'   - `Author`: The creator or author of the resource.
#'   - `Year`: The publication or release year.
#'   - `Language`: The language of the resource.
#'   - `Formats`: The format(s) of the resource (e.g., Book, Audio).
#'   - `Subjects`: The subject keywords or classifications.
#'   - `Library`: The owning library or institution.
#'   - `Series`: The series or collection the resource belongs to.
#'
#' @param data A tibble containing raw Finna metadata.
#' @return A tibble with selected, cleaned metadata fields, or NULL if required fields are missing.
#' @import dplyr
#' @export
#' @examples
#' library(finna)
#' sibelius_data <- search_finna("sibelius")
#' refine_metadata(sibelius_data)
#'
refine_metadata <- function(data) {
  required_columns <- c("Title", "Author", "Year", "Language", "Formats", "Subjects", "Library", "Series")

  # Check if the required columns exist in the data
  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0) {
    warning(paste("The following required columns are missing:", paste(missing_columns, collapse = ", ")))
    return(NULL)  # Return NULL if any required columns are missing
  }

  # Proceed with refining the metadata if all required columns are present
  refined <- data %>%
    dplyr::mutate(
      Title = if_else(is.na(.data$Title), "Unknown Title", Title),
      Author = if_else(is.na(.data$Author), "Unknown Author", Author),
      Year = if_else(is.na(.data$Year), "Unknown Year", Year),
      Language = if_else(is.na(.data$Language), "Unknown Language", Language),
      Formats = if_else(is.na(.data$Formats), "Unknown Format", Formats),
      Subjects = if_else(is.na(.data$Subjects), "Unknown Subjects", Subjects),
      Library = if_else(is.na(.data$Library), "Unknown Library", Library),
      Series = if_else(is.na(.data$Series), "Unknown Series", Series)
    ) %>%
    select(Title, Author, Year, Language, Formats, Subjects, Library, Series)

  return(refined)
}
