% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_finna.R
\name{visualize_word_cloud}
\alias{visualize_word_cloud}
\title{Visualize Word Cloud of Titles or Subjects with Stop Words Removal}
\usage{
visualize_word_cloud(metadata, column = "Title")
}
\arguments{
\item{metadata}{A tibble containing refined Finna metadata, with "Title" or "Subjects" column.}

\item{column}{The column to visualize as a word cloud (e.g., "Title" or "Subjects").}
}
\value{
A word cloud plot of the most frequent words.
}
\description{
Creates a word cloud showing the frequency of words in titles or subjects,
removing common Finnish, Swedish, and English stop words.
}
\examples{
library(finna)
music_data <- search_finna("music")
refined_data <- refine_metadata(music_data)
visualize_word_cloud(refined_data, "Title")
}
