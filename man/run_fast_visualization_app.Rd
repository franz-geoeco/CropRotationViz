% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_app.R
\name{run_fast_visualization_app}
\alias{run_fast_visualization_app}
\title{Launch Interactive District/River Catchment Visualization App}
\usage{
run_fast_visualization_app(input_dir = NA)
}
\arguments{
\item{input_dir}{Optional directory path for data loading. If NA (default),
the app will present a file upload interface.}
}
\value{
Launches a Shiny application instance
}
\description{
Initializes and launches the interactive mapping application for
comparing district or river catchment visualizations. This function handles data loading,
resource path configuration, and app deployment.
}
\details{
The function performs the following setup steps:
\itemize{
\item Loads package data from CropRotationViz
\item Creates an isolated environment for app data
\item Configures resource paths for static assets
\item Launches the Shiny application in the default browser
}
}
\note{
The application will automatically open in the system's default web browser
when launched.
}
\section{Dependencies}{

Requires the following package components:
\itemize{
\item CropRotationViz package with Input_App_data
\item fast_ui function for UI generation
\item fast_viz_server function for server logic
}
}

\examples{
\dontrun{
# Launch app with default file upload interface
run_fast_visualization_app()

# Launch app with specific data directory
run_fast_visualization_app(input_dir = "path/to/data")
}

}
