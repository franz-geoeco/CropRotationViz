% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_app.R
\name{run_processing_app}
\alias{run_processing_app}
\title{Run the Crop Rotation Visualization Application}
\usage{
run_processing_app(
  output_dir = NA,
  common_column = NA,
  start_year = NA,
  intersection_type = NA,
  preview = TRUE,
  vector_file = T
)
}
\arguments{
\item{output_dir}{Character string specifying the default output directory path. If NA (default),
the user will be prompted to select an output directory through the application interface.}

\item{common_column}{Character string specifying a default column name to be used for crop
identification across all input files. If NA (default), column selection will be prompted
for each file.}

\item{start_year}{Numeric value indicating the initial year for the sequence analysis.
If NA (default), years can be selected individually for each file. Otherwise, years
will be auto-incremented from this starting value.}

\item{intersection_type}{= "complete",}

\item{preview}{logical. If True (default) you get a snapshot as png from your processed data as sankey chart in PNG format.}

\item{vector_file}{logical. If True (default) you will get a vector file as an additional output with all intersected and aggregated field data.}
}
\value{
A Shiny application object that can be run with \code{runApp()} or deployed
to a Shiny server.
}
\description{
Launches the Shiny application for analyzing and visualizing agricultural crop rotation sequences.
This application provides an interactive interface for loading spatial data, processing crop rotations,
and generating visualizations and analysis outputs.
}
\details{
The application provides a comprehensive interface for crop rotation analysis:
\itemize{
\item{Data Input}{
\itemize{
\item Support for multiple spatial file formats (.shp, .geojson, .fgb, .gpkg, .sqlite)
\item Capability to load up to 10 years of spatial data
\item Flexible column mapping for crop identification
}
}
\item{Processing Options}{
\itemize{
\item Choice between national coding (NC) or name-based crop identification
\item Optional crop class aggregation
\item Multiple intersection processing methods
}
}
\item{Output Generation}{
\itemize{
\item Multiple export format options
\item Comprehensive metadata documentation
\item Sankey diagram visualizations
\item Statistical summaries
}
}
}
}
\section{Data Requirements}{

The application expects:
\itemize{
\item Spatial data files containing crop information
\item Consistent coordinate reference systems across files
\item Valid crop codes or names in specified columns
}
}

\section{Output Files}{

The application generates several output files:
\itemize{
\item Spatial data file (in chosen format) containing intersection results
\item RData file with processed data and statistics
\item Processing information text file with metadata
\item Sankey diagram visualization of crop sequences
}
}

\examples{
\dontrun{
# Basic usage with default settings
run_sequencer_app()

# Specify output directory
run_processing_app(output_dir = "path/to/output")

# Specify output directory and common column name
run_processing_app(
  output_dir = "path/to/output",
  common_column = "crop_code"
)

# Full configuration with start year
run_processing_app(
  output_dir = "path/to/output",
  common_column = "crop_code",
  start_year = 2020,
  preview = T,
  vector_file = T
)
}
}
