% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze_metadata.R
\name{analyze_metadata}
\alias{analyze_metadata}
\title{Analyze Refined Finna Metadata}
\usage{
analyze_metadata(metadata)
}
\arguments{
\item{metadata}{A tibble containing refined Finna metadata.}
}
\value{
A list of tibbles with summaries of formats, years, and authors.
}
\description{
Performs basic analysis on Finna metadata, summarizing the distribution of formats, years, and authors.
}
\examples{
library(finna)
sibelius_data <- search_finna("sibelius")
refined_data <- refine_metadata(sibelius_data)
analyze_metadata(refined_data)

}
