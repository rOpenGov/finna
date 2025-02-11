#' Search for Fennica Authors in Kanto (Finto) with Improved Name Matching
#'
#' This function searches for Fennica authors in the Kanto (Finto) vocabulary,
#' using optimized API calls and better handling for organization names.
#'
#' @param authors_df A tibble with an `Author` column containing names.
#' @return A tibble with `id`, `Title`, `Author`, and `Kanto_ID` columns matching authors to their Kanto URIs.
#' @examples
#' \dontrun{
#'   authors <- tibble::tibble(
#'     id = c("helka.9912014063506253", "fikka.4030053", "jykdok.803124"),
#'     Title = c("Lauseita Jak. Juteinin kirjoista",
#'               "Försök till utredande af finska kieliopista",
#'               "Suomen maanopas"),
#'     Author = c("Jaakko Juteini", "Jaakko Juteini", "August Finelius")
#'   )
#'   search_fennica_authors(authors)
#' }
#' @importFrom dplyr mutate left_join distinct bind_rows filter select
#' @importFrom tibble tibble as_tibble
#' @export

search_fennica_authors <- function(authors_df) {

  # Extract unique author names
  unique_authors <- unique(authors_df$Author)

  # Create an empty tibble to store results
  author_lookup <- tibble::tibble(Author = unique_authors, Kanto_ID = NA_character_)

  # Function to clean and simplify names
  clean_name <- function(name) {
    name <- gsub("[,()]", "", name)  # Remove commas and parentheses
    name <- gsub("\\byhti\u00F6\\b|\\borganization\\b|\\bmedia\\b", "", name, ignore.case = TRUE)  # Remove unwanted words
    name <- trimws(name)  # Trim spaces
    return(name)
  }

  # Query each unique author separately
  for (i in seq_along(unique_authors)) {
    name <- unique_authors[i]
    clean_author <- clean_name(name)  # Clean name before searching

    # Try different name variations
    name_parts <- unlist(strsplit(clean_author, " "))

    # Full cleaned name first
    query1 <- paste0(clean_author, "*")
    result1 <- search_concepts(query = query1, vocab = "finaf", lang = "fi")

    # Try first two words if full name fails
    query2 <- if (length(name_parts) >= 2) paste0(name_parts[1], " ", name_parts[2], "*") else NA
    result2 <- if (!is.na(query2)) search_concepts(query = query2, vocab = "finaf", lang = "fi") else NULL

    # Try only the first word as a last attempt
    query3 <- if (length(name_parts) >= 1) paste0(name_parts[1], "*") else NA
    result3 <- if (!is.na(query3)) search_concepts(query = query3, vocab = "finaf", lang = "fi") else NULL

    # Combine non-empty results
    valid_results <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(result1, result2, result3))

    if (length(valid_results) > 0) {
      combined_results <- bind_rows(valid_results) %>%
        distinct(uri, .keep_all = TRUE)

      if ("uri" %in% colnames(combined_results)) {
        author_lookup$Kanto_ID[i] <- combined_results$uri[1]  # Return first match
      }
    }
  }

  # Merge lookup table with original data
  authors_df <- authors_df %>%
    left_join(author_lookup, by = "Author")

  return(as_tibble(authors_df))
}
