% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{filter_initial_classes}
\alias{filter_initial_classes}
\title{Filter Crop Classification Based on Available Codes}
\usage{
filter_initial_classes(initial_classes, processed_files, codierung_all)
}
\arguments{
\item{initial_classes}{List of character vectors where each vector contains crop names
belonging to a specific classification category}

\item{processed_files}{List of processed spatial files, where each file contains:
\itemize{
\item sf_object: Simple features object containing the spatial data
\item selected_column: Character string indicating the column containing crop codes
}}

\item{codierung_all}{Data frame mapping between numeric crop codes (NC) and clear text
names (english_names)}
}
\value{
List of filtered classification categories containing only crop names that
exist in the processed files. Empty categories are removed.
}
\description{
Takes a list of initial crop classifications and filters them based on the codes
actually present in the processed spatial files. This ensures that only relevant
crop classes are maintained in the classification system.
}
\details{
The function performs several steps:
\enumerate{
\item Extracts all unique crop codes from the processed spatial files
\item For each classification category:
\itemize{
\item Converts crop names to their corresponding numeric codes
\item Keeps only codes that exist in the processed files
\item Converts filtered codes back to crop names
}
\item Removes any empty classification categories
}
}
\examples{
\dontrun{
# Example initial classes
initial_classes <- list(
  cereals = c("winter wheat", "winter barley", "rye"),
  oilseeds = c("rapeseed", "sunflower")
)

# Filter based on available data
filtered_classes <- filter_initial_classes(
  initial_classes = initial_classes,
  processed_files = my_processed_files,
  codierung_all = crop_codes_df
)
}

}
\keyword{internal}
