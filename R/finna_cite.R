#' @title Create a Data Bibliography for Finna Records
#'
#' @description
#' Creates a bibliography from selected Finna dataset records.
#'
#' @param id Finna record ID.
#' @param lang Language for the citation. Options are English (en), Finnish (fi), and Swedish (sv).
#' @param format Default is "Biblatex", alternatives are "bibentry" or "Bibtex".
#'
#' @return A Biblatex, bibentry, or Bibtex object.
#'
#' @seealso [utils::bibentry()] [RefManageR::toBiblatex()]
#'
#' @references See citation("finna")
#'
#' @importFrom RefManageR toBiblatex
#' @importFrom utils toBibtex person
#' @importFrom lubridate ymd year
#' @examples
#' \dontrun{
#' finna_cite("lapinkirjasto.1533951", lang = "en", format = "Biblatex")
#' finna_cite("lapinkirjasto.1533951", lang = "fi", format = "bibentry")
#' finna_cite("lapinkirjasto.1533951", lang = "en", format = "Bibtex")
#' }
#' @export
finna_cite <- function(id,
                       lang = "fi",
                       format = "Biblatex") {

  format <- tolower(as.character(format))

  # Fetch metadata using the Finna API
  metadata <- get_finna_records(id)
  #print(metadata)

  if(is.null(metadata)) {
    stop("The ID does not match any records in the Finna database.")
  }

  if(!any(lang %in% c("en", "fi", "sv"))){
    stop("Supported languages are English (en), Finnish (fi), and Swedish (sv).")
  }

  if(!format %in% c("bibentry", "bibtex", "biblatex")){
    warning("The ", format, " is not recognized. Defaulting to Biblatex.")
    format <- "biblatex"
  }

  # Extract relevant details from the metadata
  title <- metadata$Title %||% "Unknown Title"
  author <- metadata$Author %||% "Unknown Author"
  publisher <- metadata$Publisher %||% "Unknown Publisher"
  publish_date <- metadata$Year %||% "Unknown Year"
  access_date <- as.character(Sys.Date())

  # Create BibEntry for citation
  ref <- RefManageR::BibEntry(
    bibtype = "Misc",
    title = title,
    author = utils::person(author),
    year = publish_date,
    url = paste0("https://www.finna.fi/Record/", id),
    organization = publisher,
    urldate = access_date,
    note = paste0("Accessed on ", access_date, ".")
  )

  # Convert to the requested format
  if(format == "bibtex") {
    ref <- utils::toBibtex(ref)
  } else if (format == "biblatex") {
    ref <- RefManageR::toBiblatex(ref)
  }

  return(ref)
}
