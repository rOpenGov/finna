% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_finna.R
\name{visualize_format_distribution_pie}
\alias{visualize_format_distribution_pie}
\title{Visualize Format Distribution as Pie Chart}
\usage{
visualize_format_distribution_pie(metadata)
}
\arguments{
\item{metadata}{A tibble containing refined Finna metadata, with a "Formats" column.}
}
\value{
A ggplot2 object showing the pie chart of format distribution.
}
\description{
Creates a pie chart showing the distribution of records by formats.
}
\examples{
library(finna)
sibelius_data <- search_finna("sibelius")
refined_data <- refine_metadata(sibelius_data)
visualize_format_distribution_pie(refined_data)
}
