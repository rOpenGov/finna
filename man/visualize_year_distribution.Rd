% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualizeFinna.R
\name{visualize_year_distribution}
\alias{visualize_year_distribution}
\title{Visualize Year Distribution}
\usage{
visualize_year_distribution(year_data)
}
\arguments{
\item{year_data}{A tibble containing year distribution data (Year, count).}
}
\value{
A ggplot2 object representing the bar plot.
}
\description{
Creates a bar plot of the publication year distribution.
}
\examples{
library(finna)
sibelius_data <- search_finna("sibelius")
refined_data <- refine_metadata(sibelius_data)
analysis_results <- analyze_metadata(refined_data)
visualize_year_distribution(analysis_results$year_distribution)
}
