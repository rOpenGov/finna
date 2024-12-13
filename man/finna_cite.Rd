% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finna_cite.R
\name{finna_cite}
\alias{finna_cite}
\title{Create a Data Bibliography for Finna Records}
\usage{
finna_cite(id, lang = "fi", format = "Biblatex")
}
\arguments{
\item{id}{Finna record ID.}

\item{lang}{Language for the citation. Options are English (en), Finnish (fi), and Swedish (sv).}

\item{format}{Default is "Biblatex", alternatives are "bibentry" or "Bibtex".}
}
\value{
A Biblatex, bibentry, or Bibtex object.
}
\description{
Creates a bibliography from selected Finna dataset records.
}
\examples{
\dontrun{
finna_cite("lapinkirjasto.1533951", lang = "en", format = "Biblatex")
finna_cite("lapinkirjasto.1533951", lang = "fi", format = "bibentry")
finna_cite("lapinkirjasto.1533951", lang = "en", format = "Bibtex")
}
}
\references{
See citation("finna")
}
\seealso{
\code{\link[utils:bibentry]{utils::bibentry()}} \code{\link[RefManageR:toBiblatex]{RefManageR::toBiblatex()}}
}
