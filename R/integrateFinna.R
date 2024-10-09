#' Integrate Finna Metadata with Another Dataset
#'
#' Merges Finna metadata with another dataset using a common key (e.g., "Title").
#'
#' @param metadata1 A tibble containing refined Finna metadata.
#' @param metadata2 A tibble containing another dataset to merge with the Finna metadata.
#' @param key A string specifying the key to join by. Defaults to "Title".
#' @return A tibble containing the merged dataset.
#' @import dplyr
#' @export
#' @examples
#' library(dplyr)
#' finna_data <- search_finna("sibelius")
#'
#' # Example other dataset to merge with
#' other_data <- tibble::tibble(
#'   Title = c("Sibelius Symphony No. 5", "Finlandia", "Valse Triste"),
#'   Rating = c(5, 4, 3)
#' )
#'
#' # Integrate the two datasets by "Title"
#' integrated_data <- integrate_metadata(finna_data, other_data, key = "Title")
#' print(integrated_data)
integrate_metadata <- function(metadata1, metadata2, key = "Title") {
  # Perform a full join based on the specified key
  integrated <- full_join(metadata1, metadata2, by = key)
  return(integrated)
}
