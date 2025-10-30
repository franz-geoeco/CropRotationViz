# R/run_app.R
#' @title Run the Crop Rotation Visualization Application
#'
#' @description
#' Launches the Shiny application for analyzing and visualizing agricultural crop rotation sequences.
#' This application provides an interactive interface for loading spatial data, processing crop rotations,
#' and generating visualizations and analysis outputs.
#'
#' @param output_dir Character string specifying the default output directory path. If NA (default),
#'   the user will be prompted to select an output directory through the application interface.
#' @param common_column Character string specifying a default column name to be used for crop
#'   identification across all input files. If NA (default), column selection will be prompted
#'   for each file.
#' @param start_year Numeric value indicating the initial year for the sequence analysis.
#'   If NA (default), years can be selected individually for each file. Otherwise, years
#'   will be auto-incremented from this starting value.
#' @param intersection_type = "complete",
#' @param preview logical. If True (default) you get a snapshot as png from your processed data as sankey chart in PNG format.
#' @param vector_file logical. If True (default) you will get a vector file as an additional output with all intersected and aggregated field data.
#' 
#' @details
#' The application provides a comprehensive interface for crop rotation analysis:
#'   \itemize{
#'     \item{Data Input}{
#'       \itemize{
#'         \item Support for multiple spatial file formats (.shp, .geojson, .fgb, .gpkg, .sqlite)
#'         \item Capability to load up to 10 years of spatial data
#'         \item Flexible column mapping for crop identification
#'       }
#'     }
#'     \item{Processing Options}{
#'       \itemize{
#'         \item Choice between national coding (NC) or name-based crop identification
#'         \item Optional crop class aggregation
#'         \item Multiple intersection processing methods
#'       }
#'     }
#'     \item{Output Generation}{
#'       \itemize{
#'         \item Multiple export format options
#'         \item Comprehensive metadata documentation
#'         \item Sankey diagram visualizations
#'         \item Statistical summaries
#'       }
#'     }
#'   }
#'
#' @section Data Requirements:
#' The application expects:
#'   \itemize{
#'     \item Spatial data files containing crop information
#'     \item Consistent coordinate reference systems across files
#'     \item Valid crop codes or names in specified columns
#'   }
#'
#' @section Output Files:
#' The application generates several output files:
#'   \itemize{
#'     \item Spatial data file (in chosen format) containing intersection results
#'     \item RData file with processed data and statistics
#'     \item Processing information text file with metadata
#'     \item Sankey diagram visualization of crop sequences
#'   }
#'
#' @return A Shiny application object that can be run with \code{runApp()} or deployed
#'   to a Shiny server.
#'
#' @export
#' @import shiny
#' @importFrom shiny fluidPage
#'
#' @examples
#' \dontrun{
#' # Basic usage with default settings
#' run_sequencer_app()
#'
#' # Specify output directory
#' run_processing_app(output_dir = "path/to/output")
#'
#' # Specify output directory and common column name
#' run_processing_app(
#'   output_dir = "path/to/output",
#'   common_column = "crop_code"
#' )
#'
#' # Full configuration with start year
#' run_processing_app(
#'   output_dir = "path/to/output",
#'   common_column = "crop_code",
#'   start_year = 2020,
#'   preview = T,
#'   vector_file = T
#' )
#' }
run_processing_app <- function(output_dir = NA, common_column = NA, start_year = NA, intersection_type = NA, preview = TRUE, vector_file = T, ncores = 4) {
  # Load the package data
  utils::data("Input_App_data", package = "CropRotationViz", envir = environment())
  
  # Make data available to the app
  app_data <- new.env(parent = emptyenv())
  app_data$Input_App_data <- Input_App_data
  
  # Add resource path for www directory
  addResourcePath(
    prefix = "www",
    directoryPath = system.file(
      "www", package = "CropRotationViz"
    )
  )
  
  # Launch the application in browser
  options(shiny.launch.browser = TRUE)
  
  # Launch the application
  shiny::shinyApp(
    ui = processing_ui(app_data, output_dir, start_year, vector_file), 
    server = function(input, output, session) {
      processing_server(input, output, session, app_data, output_dir, common_column, preview, vector_file, ncores)
    }
  )
}



#' Run the Crop Rotation Visualization Application
#'
#' @description
#' Launches the Shiny application for visualizing crop rotation plans and their impact.
#' The application provides an interactive interface for exploring rotation patterns,
#' analyzing sustainability metrics, and understanding environmental effects of different
#' cropping sequences.
#'
#' @param input_dir Optional filepath to custom input data directory. If NA (default), 
#' you have to choose it manually in the app.
#'
#' @return A Shiny application object
#' 
#' @details
#' The function initializes the visualization environment by:
#' 1. Loading built-in or custom input data
#' 2. Setting up the resource paths for static assets
#' 3. Launching the Shiny server with configured UI and server logic
#'
#' @export
#' @import shiny
#' @importFrom shiny fluidPage
#' @examples
#' \dontrun{
#' run_visualization_app()
#' run_visualization_app("path/to/custom/data")
#' }
run_visualization_app <- function(input_dir = NA) {
  # Load the package data
  utils::data("Input_App_data", package = "CropRotationViz", envir = environment())
  
  # Make data available to the app
  app_data <- new.env(parent = emptyenv())
  app_data$Input_App_data <- Input_App_data
  
  # Add resource path for www directory
  addResourcePath(
    prefix = "www",
    directoryPath = system.file(
      "www", package = "CropRotationViz"
    )
  )
  
  # Launch the application in browser
  options(shiny.launch.browser = TRUE)
  
  # Launch the application
  shiny::shinyApp(
    ui = ui(app_data), 
    server = function(input, output, session) {
      viz_server(input, output, session, app_data, input_dir)
    }
  )
}

#' Launch Interactive District/River Catchment Visualization App
#' 
#' @description Initializes and launches the interactive mapping application for 
#' comparing district or river catchment visualizations. This function handles data loading,
#' resource path configuration, and app deployment.
#'
#' @details The function performs the following setup steps:
#'   \itemize{
#'     \item Loads package data from CropRotationViz
#'     \item Creates an isolated environment for app data
#'     \item Configures resource paths for static assets
#'     \item Launches the Shiny application in the default browser
#'   }
#' 
#' @param input_dir Optional directory path for data loading. If NA (default),
#'   the app will present a file upload interface.
#'
#' @section Dependencies:
#' Requires the following package components:
#'   \itemize{
#'     \item CropRotationViz package with Input_App_data
#'     \item fast_ui function for UI generation
#'     \item fast_viz_server function for server logic
#'   }
#'
#' @return Launches a Shiny application instance
#' 
#' @import shiny
#' @importFrom utils data
#'
#' @examples
#' \dontrun{
#' # Launch app with default file upload interface
#' run_fast_visualization_app()
#' 
#' # Launch app with specific data directory
#' run_fast_visualization_app(input_dir = "path/to/data")
#' }
#' 
#' @note The application will automatically open in the system's default web browser
#'   when launched.
#'
#' @export
run_fast_visualization_app <- function(input_dir = NA) {
  
  # Load the package data
  utils::data("Input_App_data", package = "CropRotationViz", envir = environment())
  
  # Make data available to the app
  app_data <- new.env(parent = emptyenv())
  app_data$Input_App_data <- Input_App_data
  
  # Add resource path for www directory
  addResourcePath(
    prefix = "www",
    directoryPath = system.file(
      "www", package = "CropRotationViz"
    )
  )
  
  # Launch the application in browser
  options(shiny.launch.browser = TRUE)
  
  # Launch the application
  shiny::shinyApp(
    ui = fast_ui(app_data), 
    server = function(input, output, session) {
      fast_viz_server(input, output, session, app_data, input_dir)
    }
  )
}


#' Launch the Field Level Visualization Application
#' 
#' @description
#' This function initializes and launches a Shiny application for visualizing crop rotation
#' data at the field level. It loads required data from the CropRotationViz package,
#' sets up the necessary environment and resources, and configures the application
#' settings before launching.
#' 
#' @details
#' The function performs the following steps:
#' 1. Loads the Input_App_data dataset from the CropRotationViz package
#' 2. Creates a new environment to store application data
#' 3. Sets up resource paths for static files
#' 4. Configures Shiny options including browser launch and maximum request size
#' 5. Launches the Shiny application with field level UI and server components
#'
#' @return A Shiny application object that can be run with runApp()
#'
#' @examples
#' \dontrun{
#' run_field_level_app()
#' }
#'
#' @export
run_field_level_app <- function() {
  utils::data("Input_App_data", package = "CropRotationViz", envir = environment())
  
  app_data <- new.env(parent = emptyenv())
  app_data$Input_App_data <- Input_App_data
  
  addResourcePath("www", system.file("www", package = "CropRotationViz"))
  
  options(shiny.launch.browser = TRUE, shiny.maxRequestSize = 10000*1024^2)
  
  shinyApp(
    ui = field_level_ui(app_data),
    server = function(input, output, session) {
      field_level_server(input, output, session, app_data)
    }
  )
}