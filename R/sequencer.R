#===============================================================================
# CROP SEQUENCE BUILDER UI COMPONENT
#===============================================================================
#' 
#' @title Crop Sequence Builder Interface
#' @description Creates an interactive Shiny UI for analyzing and managing agricultural crop rotation sequences
#' across multiple years. The interface facilitates spatial data analysis, crop pattern visualization,
#' and sequence generation with support for various data formats and processing options.
#'
#' @param app_data List containing application configuration and reference data:
#'   \itemize{
#'     \item{Input_App_data}{List containing:
#'       \itemize{
#'         \item{codierung_all}{Data frame with crop coding references}
#'         \item{crop_codes}{Vector of valid crop identification codes}
#'         \item{display_names}{Mapping of codes to display names}
#'       }
#'     }
#'   }
#' @param output_dir Character string specifying the default output directory path. Default: NA
#' @param start_year Numeric value indicating the initial year for sequence analysis. Default: NA
#' @param vector_file logical. If True (default) you will get a vector file as an additional output with all intersected and aggregated field data.
#' 
#' @details The interface provides a comprehensive set of tools for crop rotation analysis:
#'   \itemize{
#'     \item{Data Input}{
#'       \itemize{
#'         \item Support for multiple spatial file formats (.shp, .geojson, .fgb, .gpkg, .sqlite)
#'         \item Upload capability for up to 10 years of spatial data
#'         \item Dynamic column selection for crop identification
#'       }
#'     }
#'     \item{Processing Options}{
#'       \itemize{
#'         \item Choice between Code  Coding or Name-based crop identification
#'         \item Optional crop class aggregation for Code-based analysis
#'         \item Selection between fast and complete field intersection methods
#'       }
#'     }
#'     \item{Visualization}{
#'       \itemize{
#'         \item Interactive map preview of loaded spatial data
#'         \item Summary display of loaded files and their properties
#'       }
#'     }
#'     \item{Output}{
#'       \itemize{
#'         \item Multiple export format options (Shapefile, GeoPackage, FlatGeobuf)
#'         \item Customizable output directory selection
#'       }
#'     }
#'   }
#'
#' @return A Shiny UI definition (fluidPage) containing the complete sequence builder interface
#' 
#' @import shiny
#' @import shinythemes
#' @import shinyFiles
#' @import sf
#' @import leaflet
#' @import ggalluvial
#' @importFrom shiny fluidPage
#' @importFrom leaflet leafletOutput renderLeaflet addTiles setView addPolygons
#' @importFrom shinyBS bsPopover bsButton
#' 
#' @examples
#' \dontrun{
#' # Initialize with default settings
#' ui <- processing_ui(app_data)
#' 
#' # Initialize with specific output directory and start year
#' ui <- processing_ui(
#'   app_data,
#'   output_dir = "path/to/output",
#'   start_year = 2020
#' )
#' }
#' 
#' @keywords internal
processing_ui <- function(app_data, output_dir = NA, start_year = NA, vector_file = TRUE) {
  
  # Add Bootstrap dependencies
  shiny::addResourcePath(
    "shinyBS", 
    system.file("www", package = "shinyBS")
  )
  
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "shinyBS/shinyBS.css"),
      tags$script(src = "shinyBS/shinyBS.js")
    ),
    theme = shinytheme("cyborg"),
    #-------------------------------------------------------------------------------
    # Header Section
    #-------------------------------------------------------------------------------
    br(),
    fluidRow(
      column(style = "margin-top: -0px; margin-bottom: 0px;", width = 5, h2("Crop Sequence Builder")), 
      column(4),
      column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
    ),
    tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
    
    #-------------------------------------------------------------------------------
    # Step 1: File Selection and Configuration
    #-------------------------------------------------------------------------------
    fluidRow(
      column(4,
             # Counter display
             tags$span(
               textOutput("file_counter"), 
               style = "font-size: 20px; color: #ffffff; font-weight: bold"
             ), br()),
      column(2,
             radioButtons("id_or_name", label = "Select Crop Column Type", choices = c("Code", "Name"), inline = TRUE)
      ),
      column(1,
             shinyBS::bsButton(
               "id-info",
               label = "",
               icon = icon("info"),
               style = "default",
               size = "extra-small"
             ),
             shinyBS::bsPopover(
               "id-info",
               "Crop Column",
               "Here you can select the column which describes your crop. Use Code if you want to use the national coding or/and you want to use the aggregation, which summarizes some smaller classes to bigger groups.",
               placement = "left",
               trigger = "toggle",
               options = list(
                 container = 'body',
                 html = TRUE
               )
             )
      ),
      column(2,             
             radioButtons("aggregation", 
                          label = "Aggregation of crop classes", 
                          choices = c("Yes", "No"), 
                          inline = TRUE)
             
      ),
      column(2,
             radioButtons("radio_process", "Type of field intersection",
                          c("Complete" = "complete",
                            "Fast" = "fast"), inline = TRUE)
      ),
      column(1,
             shinyBS::bsButton(
               "radio-info",
               label = "",
               icon = icon("info"),
               style = "default",
               size = "extra-small"
             ),
             shinyBS::bsPopover(
               "radio-info",
               "Type of intersection",
               "Here you can select the type of the intersection framework. The fast intersect just intersects your fields over the years and leaves out fields not present in a year. The comprehensive method is much slower but contains each area you put in with comprehensive union functions.",
               placement = "left",
               trigger = "toggle",
               options = list(
                 container = 'body',
                 html = TRUE
               )
             )
      )
    ),
    tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
    
    #-------------------------------------------------------------------------------
    # Step 2: Dynamic File Selectors
    #-------------------------------------------------------------------------------
    lapply(1:10, function(i) {
      tagList(
        # File selector
        conditionalPanel(
          condition = sprintf(
            "output.show_selector_%d && !output.file%d_selected", 
            i, i
          ),
          shinyFilesButton(
            sprintf("Btn_GetFile%d", i),
            sprintf("Choose Annual Field Polygon File: year %d", i),
            title = sprintf("Please select polygon file year: %d (.shp, .geojson, .fgb, .gpkg or .sqlite):", i),
            multiple = FALSE,
            buttonType = "default",
            class = "btn-primary",
            style = "background-color: rgb(116, 150, 30); border-color: rgb(116, 150, 30); color: white;"
          )
        ),
        
        fluidRow(
          column(4,
                 div(textOutput(sprintf("txt_file%d", i)), style = "color: white; font-size: 15px;")
          ),
          column(3,
                 conditionalPanel(
                   condition = sprintf("output.file%d_loaded", i),
                   selectInput(
                     sprintf("column_selector%d", i),
                     sprintf("Column describing the Code or Name #%d:", i),
                     choices = NULL
                   )
                 )
          ),
          if(is.na(start_year)){
            column(2,
                   conditionalPanel(
                     condition = sprintf("output.file%d_loaded", i),
                     selectInput(
                       sprintf("year_selector%d", i),
                       sprintf("Year for file #%d:", i),
                       choices = 2000:2030,
                       selected = 2010
                     )
                   )
            )
          } else {
            column(2,
                   conditionalPanel(
                     condition = sprintf("output.file%d_loaded", i),
                     selectInput(
                       sprintf("year_selector%d", i),
                       sprintf("Year for file #%d:", i),
                       choices = 2000:2030,
                       selected = start_year+i-1
                     )
                   )
            )
          },
          column(1,
                 conditionalPanel(
                   condition = sprintf("output.file%d_selected", i),
                   actionButton(
                     sprintf("btn_remove%d", i),
                     "Remove Layer",
                     class = "btn-danger",
                     style = "margin-top: 25px;"
                   )
                 )
          )
        )
      )
    }),
    
    #-------------------------------------------------------------------------------
    # Step 3: File Summary and Map Preview
    #-------------------------------------------------------------------------------
    fluidRow(
      column(5,
             verbatimTextOutput("loaded_files_summary")
      ),
      column(4,
             leafletOutput("map")
      ),
      column(2,
             conditionalPanel(
               condition = "output.show_process_button",
               shinyFilesButton(
                 "Choose_AOI_File",
                 "Choose a AOI File (optional)",
                 title = "Please select polygon file which will be used for splitting in areas of interest (.shp, .geojson, .fgb, .gpkg or .sqlite):",
                 multiple = FALSE,
                 buttonType = "default",
                 class = "btn-primary",
                 style = "background-color: rgb(116, 150, 30); border-color: rgb(116, 150, 30); color: white;"
               )
             ),
             # Add column selector UI below the file button
             conditionalPanel(
               condition = "output.show_aoi_selector",
               uiOutput("aoi_column_selector")
             )
      ),
      column(1,
             conditionalPanel(
               condition = "output.show_process_button",
               shinyBS::bsButton(
                 "AOI_info",  
                 label = "",
                 icon = icon("info"),
                 style = "default",
                 size = "extra-small"
               )
             ),
             shinyBS::bsPopover(
               "AOI_info",  
               "Choose a AOI File (optional)",
               "Here you can add a polygon file defining areas of interest in your region. These are used to divide the region into areas of interest that can be analysed individually. This sectioning will be used besides the administrative boundaries from GADM and the river basins in Germany.",
               placement = "left",
               trigger = "hover",
               options = list(
                 container = 'body',
                 html = TRUE,
                 delay = list(show = 0, hide = 0)  
               )
             )
      )
      
    ),
    
    tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
    
    #-------------------------------------------------------------------------------
    # Step 4: Processing Options
    #-------------------------------------------------------------------------------
    fluidRow(
      column(4,
             conditionalPanel(
               condition = "output.show_process_button && !input.btn_continue",
               actionButton("btn_continue", "Continue to Processing", 
                            class = "btn-primary btn-lg",
                            style = "margin-top: 20px; background-color: rgb(116, 150, 30); border-color: rgb(116, 150, 30); color: white;")
             )
      ),
      column(2,
             conditionalPanel(
               condition = "output.show_process_button && !input.btn_continue && input.id_or_name == 'Code'",
               radioButtons("language",
                            "Crop Name Language",
                            c("English", "German"), inline = TRUE)
             )
      )
    ),
    # Processing Options (shown after continue)
    conditionalPanel(
      condition = "input.btn_continue",
      fluidRow(
        if(is.na(output_dir)) {
          column(width = 4,
                 conditionalPanel(
                   condition = "output.show_process_button",
                   shinyDirButton(
                     "dir", 
                     "Choose Output Directory", 
                     "Choose a output directory", 
                     style = "background-color: rgb(116, 150, 30); border-color: rgb(116, 150, 30); color: white;"
                   ),
                   verbatimTextOutput("selected_dir")
                 )
          )
        },
        if(vector_file) {
          column(width = 4,
                 conditionalPanel(
                   condition = "output.show_process_button", 
                   radioButtons(
                     "filetype",
                     "Type of output Format",
                     c("Shapefile", "GeoPackage", "FlatGeobuf"),
                     inline = TRUE
                   )
                 )
          )
        },
        if(vector_file){
          column(1, style = "padding: 0; margin-left: 10px;",
                 conditionalPanel(
                   condition = "output.show_process_button", 
                   shinyBS::bsButton(
                     "format",  
                     label = "",
                     icon = icon("info"),
                     style = "default",
                     size = "extra-small"
                   )
                 ),
                 conditionalPanel(
                   condition = "output.show_process_button", 
                   shinyBS::bsPopover(
                     "format",  
                     "Polygon Output Format",
                     "Here you can select which output format you want for the intersected polygon file.",
                     placement = "right",
                     trigger = "hover",
                     options = list(
                       container = 'body',
                       html = TRUE,
                       delay = list(show = 0, hide = 0)  
                     )
                   )
                 )
          )
        },
        column(2,
               conditionalPanel(
                 condition = "output.show_process_button",
                 radioButtons("fastImages",
                              "Do you want a fast visualization version?",
                              c("Yes", "No"), inline = TRUE)
               )
        ),
        column(1, style = "padding: 0; margin-left: 10px;",
               conditionalPanel(
                 condition = "output.show_process_button", 
                 shinyBS::bsButton(
                   "fast_sel",  
                   label = "",
                   icon = icon("info"),
                   style = "default",
                   size = "extra-small"
                 )
               ),
               conditionalPanel(
                 condition = "output.show_process_button", 
                 shinyBS::bsPopover(
                   "fast_sel",  
                   "Fast Version?",
                   "Here you can select whether you want to create pre-rendered charts, which allow you to use a fast visualisation mode.",
                   placement = "right",
                   trigger = "hover",
                   options = list(
                     container = 'body',
                     html = TRUE,
                     delay = list(show = 0, hide = 0)  
                   )
                 )
               )
        ),
      ),
      
      # Final Process Button
      fluidRow(
        column(4,
               conditionalPanel(
                 condition = "output.show_process_button",
                 actionButton("btn_process", "Process Files", 
                              class = "btn-primary btn-lg",
                              style = "margin-top: 20px; background-color: rgb(116, 150, 30); border-color: rgb(116, 150, 30);")
               )
        )
      )
    ), br(), 
    tags$head(
      tags$style(HTML("
            .crop-box {
                border: 1px solid #ccc;
                padding: 3px;
                margin: 2px;
                min-height: 45px;
                width: 150px;
                float: left;
            }
            .add-class-btn {
                margin: 5px 0;
                clear: both;
                font-size: 11px;
                padding: 2px 8px;
            }
            .rank-list-container {
                min-height: 45px;
                border: 1px dashed #ccc;
                padding: 1px;
                margin-bottom: 2px;
                font-size: 11px;
            }
            .name-input input {
                height: 20px;
                font-size: 11px;
                padding: 1px 1px;
                color: white !important;
                font-weight: bold;
            }
            .remove-btn {
                padding: 0px 1px;
                font-size: 10px;
                margin-top: 3px;
                float: right;
            }
            .aggregation-container {
                display: grid;
                grid-template-columns: repeat(auto-fit, minmax(140px, 1fr));
                grid-auto-rows: min-content;
                grid-gap: 10px;
                width: 100%;
                justify-content: start;
                max-height: 600px; /* Set a maximum height */
                overflow-y: auto; /* Enable vertical scrolling */
                padding: 10px;
                /* Smooth scrolling for better UX */
                scroll-behavior: smooth;
                /* Style the scrollbar for better visibility */
                scrollbar-width: thin;
                scrollbar-color: rgba(116, 150, 30, 0.6) transparent;
            }
            
            /* Webkit scrollbar styling */
            .aggregation-container::-webkit-scrollbar {
                width: 8px;
            }
            
            .aggregation-container::-webkit-scrollbar-track {
                background: transparent;
            }
            
            .aggregation-container::-webkit-scrollbar-thumb {
                background-color: rgba(116, 150, 30, 0.6);
                border-radius: 4px;
            }
            .rank-list-item {
                padding: 1px 3px !important;
                margin: 1px 0 !important;
                font-size: 10px !important;
                background-color: #f8f9fa;
                border: 1px solid #dee2e6;
                border-radius: 2px;
            }
            .collapse-header {
                cursor: pointer;
                user-select: none;
                padding: 7px;
                margin-bottom: 5px;
            }
            .collapse-header:hover {
                opacity: 0.8;
            }
            .section-content {
                padding: 10px;
            }
        "))
    ),
    # Step 5: Crop Code Aggregation Editor (appears after continue)
    conditionalPanel(
      condition = "input.btn_continue && input.aggregation == 'Yes'",
      fluidRow(
        column(12,
               wellPanel(
                 # Editor Header
                 div(id = "editor-header",
                     class = "collapse-header",
                     onclick = "$('#editor-content').collapse('toggle')",
                     div(style = "display: flex; align-items: center;",
                         h4(style = "margin: 0; display: flex; align-items: center;",
                            span(id = "editor-icon", "▶", style = "margin-right: 10px;"),
                            "Crop Code Aggregation Editor"
                         ),
                         column(1, style = "padding: 0; margin-left: 10px;",
                                shinyBS::bsButton(
                                  "aggregation_editor_info",  
                                  label = "",
                                  icon = icon("info"),
                                  style = "default",
                                  size = "extra-small"
                                ),
                                shinyBS::bsPopover(
                                  "aggregation_editor_info", 
                                  "Crop Aggregation Info",
                                  "Here you can inspect and change the default crop aggregation. You can create new classes, rename existing and drag and drop crops into the aggregation classes on the reight. Each single crop on the left list will be still processed but not aggregated.",
                                  placement = "right",
                                  trigger = "hover",
                                  options = list(
                                    container = 'body',
                                    html = TRUE,
                                    delay = list(show = 0, hide = 0)  
                                  )
                                )
                         )
                     )
                 ),
                 # Editor Content
                 div(id = "editor-content", class = "collapse",
                     div(class = "section-content",
                         fluidRow(
                           column(3,
                                  wellPanel(
                                    h4("Available Crops / User Specific Single Crop Classes"),
                                    uiOutput("available_crops_ui")
                                  )
                           ),
                           column(9,
                                  wellPanel(
                                    h4("Aggregation Classes"),
                                    actionButton("add_class", "Add New Aggregation Class", 
                                                 class = "add-class-btn"),
                                    div(class = "aggregation-container",
                                        uiOutput("aggregation_classes")
                                    )
                                  )
                           )
                         )
                     )
                 )
               )
        )
      ),
      
      # Visualization Panel
      fluidRow(
        column(12,
               wellPanel(
                 # Plotly Header
                 div(id = "plotly-header",
                     class = "collapse-header",
                     onclick = "$('#plotly-content').collapse('toggle')",
                     div(style = "display: flex; align-items: center;",
                         h4(style = "margin: 0; display: flex; align-items: center;",
                            span(id = "plotly-icon", "▶", style = "margin-right: 10px;"),
                            "Aggregation Visualization"
                         ),
                         column(1, style = "padding: 0; margin-left: 10px;",
                                shinyBS::bsButton(
                                  "aggregation_info",
                                  label = "",
                                  icon = icon("info"),
                                  style = "default",
                                  size = "extra-small"
                                ),
                                shinyBS::bsPopover(
                                  "aggregation_info",
                                  "Crop Aggregation Info",
                                  "Here you can inspect the current aggragation of crops into broader classes, which will be used if you click on prosessing.",
                                  placement = "right",
                                  trigger = "hover",
                                  options = list(
                                    container = 'body',
                                    html = TRUE
                                  )
                                )
                         )
                     )
                 ),
                 # Plotly Content
                 div(id = "plotly-content", class = "collapse",
                     div(class = "section-content",
                         plotlyOutput("sankeyPlot", height = "600px")
                     )
                 )
               )
        )
      ),
      
      # Add JavaScript for collapse icons
      tags$script("
    $('#editor-content').on('shown.bs.collapse', function () {
      $('#editor-icon').html('▼');
    });
    $('#editor-content').on('hidden.bs.collapse', function () {
      $('#editor-icon').html('▶');
    });
    $('#plotly-content').on('shown.bs.collapse', function () {
      $('#plotly-icon').html('▼');
    });
    $('#plotly-content').on('hidden.bs.collapse', function () {
      $('#plotly-icon').html('▶');
    });
  ")
    ),
    
    br(), br(), br(), br(),
    
    # Footer
    tags$footer(
      style = "position: fixed; 
           bottom: 0; 
           width: 100%; 
           background-color: #f5f5f5; 
           padding: 10px; 
           text-align: center;
           border-top: 1px solid #e7e7e7;
           left: 0;
           z-index: 1000;",
      
      div(
        style = "display: inline-block;",
        p(
          tags$span("Author: "),
          tags$a(
            href = "https://github.com/franzschulze/CropRotationViz", 
            "Franz Schulze",
            style = "color: #5d9bd9; text-decoration: underline;"
          ),
          style = "margin: 0; 
               display: inline-block; 
               margin-right: 20px;"
        ),
        p(
          tags$span("Institution: "),
          tags$a(
            href = "https://geooeko.geo.uni-halle.de/",
            "Department of Geoecology - Institute of Geosciences and Geography - University of Halle-Wittenberg",
            style = "color: #5d9bd9; text-decoration: underline;"
          ),
          style = "margin: 0; 
               display: inline-block;"
        )
      )
    )
  )
} 

#' @title Crop Sequence Builder Server Logic
#' @description Implements the server-side logic for analyzing and managing agricultural crop rotation sequences.
#' Provides comprehensive data processing, spatial analysis, and visualization capabilities for multi-year
#' crop rotation patterns.
#' 
#' @param input Shiny input object containing UI input values
#' @param output Shiny output object for rendering UI elements
#' @param session Shiny session object managing the current user session
#' @param app_data List containing application configuration and reference data:
#'   \itemize{
#'     \item{Input_App_data}{List containing:
#'       \itemize{
#'         \item{codierung_all}{Data frame mapping crop codes to names and classifications}
#'         \item{crop_codes}{List defining crop code hierarchies and aggregations}
#'         \item{display_names}{Named vector mapping internal codes to user-friendly names}
#'         \item{EZG}{Additional reference data for crop classification}
#'       }
#'     }
#'   }
#' @param output_dir Character string specifying default output directory path. Default: NA
#' @param common_column Character string specifying a default column for crop identification across files. Default: NA
#' @param preview logical. If True (default) you get a snapshot as png from your processed data as sankey chart in PNG format.
#' @param vector_file logical. If True (default) you will get a vector file as an additional output with all intersected and aggregated field data.
#' 
#' @details The server component implements several key functionalities:
#'   \itemize{
#'     \item{Data Management}{
#'       \itemize{
#'         \item Dynamic file loading and validation for multiple spatial formats
#'         \item Automatic column detection and mapping
#'         \item Year assignment and sequence tracking
#'         \item Interactive map visualization with bounding box display
#'       }
#'     }
#'     \item{Spatial Processing}{
#'       \itemize{
#'         \item Support for multiple spatial file formats (.shp, .geojson, .fgb, .gpkg, .sqlite)
#'         \item Two intersection methods: complete (preserving all areas) and fast (optimized for performance)
#'         \item Optional crop class aggregation based on national coding system
#'         \item Area calculations and spatial transformations
#'       }
#'     }
#'     \item{Progress Tracking}{
#'       \itemize{
#'         \item Real-time validation feedback
#'         \item File loading status updates
#'         \item Processing progress indicators
#'         \item Error handling and notifications
#'       }
#'     }
#'     \item{Output Generation}{
#'       \itemize{
#'         \item Multiple export format options (Shapefile, GeoPackage, FlatGeobuf)
#'         \item Comprehensive metadata documentation
#'         \item Sankey diagram visualization of crop sequences
#'         \item RData file with processed results
#'       }
#'     }
#'   }
#'
#' @section Processing Workflow:
#'   The server follows a structured workflow:
#'   \enumerate{
#'     \item File validation and loading
#'     \item Column mapping and year assignment
#'     \item Spatial intersection processing
#'     \item Optional crop class aggregation
#'     \item Area calculations and statistics
#'     \item Output file generation
#'     \item Visualization creation
#'   }
#'
#' @section Performance Considerations:
#'   \itemize{
#'     \item Automatic detection and warning for large datasets (>100,000 features)
#'     \item Memory-efficient processing for spatial operations
#'     \item Progress tracking for long-running operations
#'     \item Optimized intersection methods for different use cases
#'   }
#'
#' @return Reactive expression containing a list of processed files with their metadata:
#'   \itemize{
#'     \item{sf_object}{Simple features object containing spatial data}
#'     \item{selected_column}{Name of column containing crop information}
#'     \item{selected_year}{Year assigned to the dataset}
#'     \item{filepath}{Original file path}
#'   }
#' 
#' @import dplyr
#' @import shiny
#' @import sf
#' @import leaflet
#' @import tools
#' @import ggalluvial
#' @importFrom shinyFiles shinyFileChoose parseFilePaths parseDirPath
#' @importFrom ggalluvial geom_stratum geom_flow
#' @importFrom sortable bucket_list add_rank_list
#' 
#' @examples
#' \dontrun{
#' # Basic server initialization
#' server <- function(input, output, session) {
#'   processing_server(input, output, session, app_data)
#' }
#' 
#' # Server with custom output directory and column mapping
#' server <- function(input, output, session) {
#'   processing_server(
#'     input, 
#'     output, 
#'     session, 
#'     app_data,
#'     output_dir = "path/to/output",
#'     common_column = "crop_code"
#'   )
#' }
#' }
#' 
#' @keywords internal
processing_server <- function(input, output, session, app_data, output_dir = NA, common_column = NA, preview = TRUE, vector_file = TRUE) {
  library(ggalluvial)
  
  # Start timing
  start_time <- Sys.time()
  initial_mem <- gc(reset = TRUE)
  
  # global options
  options(warn = -1)
  sf_use_s2(F)
  
  # Show welcome alert when the app starts
  observe({
    shinyalert(
      title = "Welcome!",
      text = paste("This application allows you to combine multiple annual cropping polygon layers into a comprehensive crop sequence.",
                   "Select in the following your annual files and process it. When the processing ends you can close the app and start the visualization application."),
      type = "info",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Let's Begin!",
      confirmButtonCol = "#5d9bd9",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  # Access the data from app_data
  codierung_all <- app_data$Input_App_data$codierung_all
  crop_codes    <- app_data$Input_App_data$crop_codes
  countriesSP <- app_data$Input_App_data$countriesSP
  display_names <- app_data$Input_App_data$display_names
  EZG           <- app_data$Input_App_data$EZG
  
  # Helper function to get names from codes - defined once at the top
  get_names <- function(codes, codierung_all) {
    if(input$language == "English"){
      names <- codierung_all$english_names[match(codes, codierung_all$NC)]
    }else{
      names <- codierung_all$german_names[match(codes, codierung_all$NC)]
    }
    return(names[!is.na(names)])
  }
  
  # Set up volumes for file selection
  if(is.na(output_dir)){
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  } else {
    volumes <- c(Home = output_dir, "R Installation" = R.home(), getVolumes()())
  }
  
  # Initialize directory selection
  shinyDirChoose(input, "dir", roots = volumes, session = session)
  selected_dir <- reactiveVal()
  
  class_names <- reactiveVal(list())
  
  
  # Display selected directory
  output$selected_dir <- renderPrint({
    if (length(input$dir) > 1) {
      parseDirPath(volumes, input$dir)
    } else {
      cat("No directory selected (input dir)")
    }
  })
  
  # Update selected directory when input changes
  observeEvent(input$dir, {
    if (!is.null(input$dir)) {
      path <- parseDirPath(volumes, input$dir)
      selected_dir(path)
    }
  })
  
  get_selected_dir <- reactive({
    selected_dir()
  })
  
  
  
  #----------------------------------------------------------------------------------------------------------
  # Create reactive values for all possible files (1-10)
  file_reactives <- lapply(1:10, function(i) {
    list(
      selected_file = reactiveVal(),
      sf_data = reactiveVal(),
      is_valid = reactiveVal(FALSE),
      is_complete = reactiveVal(FALSE),
      selected_year = reactiveVal()
    )
  })
  
  # Add reactive value for tracking the first layer's bounding box
  first_layer_bbox <- reactiveVal()
  processed_files <- reactiveVal(list())
  current_file_count <- reactiveVal(1)
  
  output$show_process_button <- reactive({
    return(current_file_count() >= 3)
  })
  outputOptions(output, "show_process_button", suspendWhenHidden = FALSE)
  
  # Function to reset a file's reactive values
  resetFileReactives <- function(i) {
    file_reactives[[i]]$selected_file(NULL)
    file_reactives[[i]]$sf_data(NULL)
    file_reactives[[i]]$is_valid(FALSE)
    file_reactives[[i]]$is_complete(FALSE)
    file_reactives[[i]]$selected_year(NULL)
    
    # Reset the corresponding UI elements
    updateSelectInput(session, sprintf("column_selector%d", i), choices = NULL)
    updateSelectInput(session, sprintf("year_selector%d", i), selected = 2010)
    
    # Update processed files list
    current_files <- processed_files()
    current_files[[i]] <- NULL
    processed_files(current_files)
    
    # Update file counter
    new_count <- max(1, min(i, length(current_files[!sapply(current_files, is.null)]) + 1))
    current_file_count(new_count)
    
    # Reset the bounding box if first file is removed
    if (i == 1) {
      first_layer_bbox(NULL)
    }
  }
  
  # Add observers for remove buttons
  lapply(1:10, function(i) {
    observeEvent(input[[sprintf("btn_remove%d", i)]], {
      resetFileReactives(i)
      showNotification(sprintf("File %d removed successfully", i), type = "message")
    })
  })
  
  # Function to process shapefile selection
  processShapefileSelection <- function(input_id, file_reactive, column_selector_id, common_column = NA) {
    shinyFileChoose(input, input_id,
                    roots = volumes,
                    session = session,
                    restrictions = system.file(package = "base"),
                    filetypes = c("shp", "fgb", "gpkg", "sqlite", "geojson"))
    
    if(!is.null(input[[input_id]])) {
      file_selected <- parseFilePaths(volumes, input[[input_id]])
      
      if(nrow(file_selected) > 0) {
        file_ext <- tolower(tools::file_ext(file_selected$datapath))
        
        if(file_ext == "shp" | file_ext == "fgb"| file_ext == "gpkg"| file_ext == "sqlite"| file_ext == "geojson") {
          file_reactive$selected_file(file_selected$datapath)
          
          tryCatch({
            sf_obj <- st_read(file_selected$datapath, quiet = TRUE)
            file_reactive$sf_data(sf_obj)
            file_reactive$is_valid(TRUE)
            
            if(is.na(common_column)){
              updateSelectInput(session, column_selector_id,
                                choices = setdiff(names(sf_obj), 
                                                  attr(sf_obj, "sf_column")))
            }else{
              updateSelectInput(session, column_selector_id,
                                choices = setdiff(names(sf_obj), 
                                                  attr(sf_obj, "sf_column")),
                                selected = common_column)
            }
            
            # Update bounding box if this is the first file
            if (input_id == "Btn_GetFile1") {
              bbox <- st_bbox(sf_obj)
              first_layer_bbox(bbox)
            }
            
          }, error = function(e) {
            file_reactive$is_valid(FALSE)
            file_reactive$sf_data(NULL)
            showNotification(paste("Error reading file:", e$message), 
                             type = "error")
          })
        } else {
          file_reactive$selected_file(NULL)
          file_reactive$sf_data(NULL)
          file_reactive$is_valid(FALSE)
        }
      }
    }
  }
  
  # Create observers for all possible files
  lapply(1:10, function(i) {
    # Observer for file selection
    observe({
      processShapefileSelection(
        sprintf("Btn_GetFile%d", i),
        file_reactives[[i]],
        sprintf("column_selector%d", i),
        common_column
      )
    })
    
    # Text output for file path
    output[[sprintf("txt_file%d", i)]] <- renderText({
      if(file_reactives[[i]]$is_valid()) {
        paste(sprintf("File #%d:", i), 
              file_reactives[[i]]$selected_file())
      }
    })
    
    # Validation message
    output[[sprintf("validation_message%d", i)]] <- renderText({
      if(!is.null(input[[sprintf("Btn_GetFile%d", i)]]) && 
         !file_reactives[[i]]$is_valid()) {
        "Please select a valid polygon-file (.shp, .geojson, .fgb, .gpkg or .sqlite)"
      }
    })
    
    # Observer for column and year selection
    observeEvent(input[[sprintf("column_selector%d", i)]], {
      req(file_reactives[[i]]$sf_data(), 
          input[[sprintf("column_selector%d", i)]],
          input[[sprintf("year_selector%d", i)]])
      
      if(input[[sprintf("column_selector%d", i)]] != "") {
        file_reactives[[i]]$is_complete(TRUE)
        file_reactives[[i]]$selected_year(input[[sprintf("year_selector%d", i)]])
        
        current_files <- processed_files()
        current_files[[i]] <- list(
          sf_object = file_reactives[[i]]$sf_data(),
          selected_column = input[[sprintf("column_selector%d", i)]],
          selected_year = input[[sprintf("year_selector%d", i)]],
          filepath = file_reactives[[i]]$selected_file()
        )
        processed_files(current_files)
        
        if(i < 10) {
          current_file_count(i + 1)
        }
      }
    })
    
    # Additional observer for year changes
    observeEvent(input[[sprintf("year_selector%d", i)]], {
      req(file_reactives[[i]]$is_complete())
      
      current_files <- processed_files()
      if(!is.null(current_files[[i]])) {
        current_files[[i]]$selected_year <- input[[sprintf("year_selector%d", i)]]
        processed_files(current_files)
      }
    })
    
    # Control visibility conditions
    output[[sprintf("file%d_loaded", i)]] <- reactive({
      return(!is.null(file_reactives[[i]]$sf_data()))
    })
    
    output[[sprintf("file%d_selected", i)]] <- reactive({
      return(file_reactives[[i]]$is_complete())
    })
    
    output[[sprintf("show_selector_%d", i)]] <- reactive({
      if(i == 1) return(TRUE)
      return(file_reactives[[i-1]]$is_complete())
    })
    
    outputOptions(output, sprintf("file%d_loaded", i), 
                  suspendWhenHidden = FALSE)
    outputOptions(output, sprintf("file%d_selected", i), 
                  suspendWhenHidden = FALSE)
    outputOptions(output, sprintf("show_selector_%d", i), 
                  suspendWhenHidden = FALSE)
  })
  
  # Add map output
  output$map <- renderLeaflet({
    bbox <- first_layer_bbox()
    
    if (is.null(bbox)) {
      # Return a default map if no file is loaded
      leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 0, zoom = 2)
    } else {
      # Create a simple features polygon from the bounding box
      bbox_polygon <- st_as_sfc(st_bbox(bbox))
      
      # Transform to WGS84 if needed (Leaflet requires WGS84)
      if (st_crs(bbox_polygon) != 4326) {
        bbox_polygon <- st_transform(bbox_polygon, 4326)
      }
      
      # Create map with the bounding box
      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data = bbox_polygon,
          fillColor = "rgb(0,86,157)",
          fillOpacity = 0.3,
          weight = 2,
          color = "rgb(0,86,157)",
          dashArray = "5,5",
          group = "field bbox"
        )
    }
  })
  
  output$file_counter <- renderText({
    count <- current_file_count()
    if(count < 10) {
      sprintf("Choosing file %d of maximum 10", count)
    } else {
      "Maximum number of files (10) reached"
    }
  })
  
  output$loaded_files_summary <- renderPrint({
    files <- processed_files()
    if(length(files) == 0) {
      cat("No files loaded yet")
    } else {
      cat("Currently loaded files:\n\n")
      for(i in seq_along(files)) {
        if(!is.null(files[[i]])) {
          cat(sprintf("Year %d:\n", i))
          cat(sprintf("  Path: %s\n", files[[i]]$filepath))
          cat(sprintf("  Selected Column: %s\n", files[[i]]$selected_column))
          cat(sprintf("  Selected Year: %s\n", files[[i]]$selected_year))
          cat(sprintf("  Number of Features: %d\n", nrow(files[[i]]$sf_object)))
          cat("\n")
        }
      }
    }
  })
  
  
  #----------------------------------------------------------------------------------------------------------
  # Add reactive values for AOI data and column selection
  output$show_aoi_selector <- reactive({
    return(aoi_loaded())
  })
  outputOptions(output, "show_aoi_selector", suspendWhenHidden = FALSE)
  aoi_data <- reactiveVal(NULL)
  aoi_loaded <- reactiveVal(FALSE)
  aoi_column <- reactiveVal(NULL)
  
  # Add observer for AOI file selection
  observeEvent(input$Choose_AOI_File, {
    shinyFileChoose(
      input,
      "Choose_AOI_File",
      roots = volumes,
      session = session,
      restrictions = system.file(package = "base"),
      filetypes = c("shp", "fgb", "gpkg", "sqlite", "geojson")
    )
    
    if (!is.null(input$Choose_AOI_File)) {
      file_selected <- parseFilePaths(volumes, input$Choose_AOI_File)
      
      if (nrow(file_selected) > 0) {
        file_ext <- tolower(tools::file_ext(file_selected$datapath))
        
        if (file_ext %in% c("shp", "fgb", "gpkg", "sqlite", "geojson")) {
          tryCatch({
            # Read the AOI file
            aoi_sf <- st_read(file_selected$datapath, quiet = TRUE)
            
            # Basic validation
            if (!is(aoi_sf, "sf")) {
              showNotification("Invalid AOI file format. Please ensure it contains polygon geometries.",
                               type = "error")
              return(NULL)
            }
            
            if (!all(st_geometry_type(aoi_sf) %in% c("POLYGON", "MULTIPOLYGON"))) {
              showNotification("AOI file must contain polygon geometries.",
                               type = "error")
              return(NULL)
            }
            
            # Store the initial AOI data
            aoi_data(aoi_sf)
            aoi_loaded(TRUE)
            
            # Create column selector UI
            output$aoi_column_selector <- renderUI({
              req(aoi_loaded())
              selectInput(
                "aoi_name_column",
                "Select column for AOI names:",
                choices = setdiff(names(aoi_sf), attr(aoi_sf, "sf_column")),
                selected = NULL,
                width = "100%"
              )
            })
            
            # Show success notification
            showNotification(
              sprintf("AOI file loaded successfully. Please select the name column."),
              type = "message"
            )
            
          }, error = function(e) {
            showNotification(
              sprintf("Error reading AOI file: %s", e$message),
              type = "error"
            )
            aoi_data(NULL)
            aoi_loaded(FALSE)
          })
        } else {
          showNotification(
            "Invalid file type. Please select a .shp, .geojson, .fgb, .gpkg, or .sqlite file.",
            type = "error"
          )
        }
      }
    }
  })
  
  # Observer for column selection
  observeEvent(input$aoi_name_column, {
    req(first_layer_bbox(), aoi_loaded(), input$aoi_name_column, aoi_data())
    
    aoi_sf <- aoi_data()
    
    selected_column <- input$aoi_name_column
    
    # Validate that selected column exists and has unique values
    if (selected_column %in% names(aoi_sf)) {
      # Check for NA or empty values in selected column
      if (any(is.na(aoi_sf[[selected_column]]) | aoi_sf[[selected_column]] == "")) {
        showNotification(
          "Warning: Selected column contains missing or empty values. These may cause issues.",
          type = "warning"
        )
      }
      
      # Check for duplicate values
      if (any(duplicated(aoi_sf[[selected_column]]))) {
        showNotification(
          "Warning: Selected column contains duplicate values. This may cause issues with identification.",
          type = "warning"
        )
      }
      
      # Keep only the selected column and geometry
      aoi_sf_cleaned <- aoi_sf %>%
        select(all_of(selected_column), geometry)
      
      # Rename the selected column to a standardized name
      names(aoi_sf_cleaned)[names(aoi_sf_cleaned) == selected_column] <- "AOI_name"
      
      # Update stored AOI data
      aoi_data(aoi_sf_cleaned)
      aoi_column(selected_column)
      
      # Update the map to show the AOI boundaries with labels
      observe({
        if (aoi_loaded()) {
          aoi <- aoi_data()
          
          bbox_polygon <- st_as_sfc(st_bbox(first_layer_bbox()))
          bbox_polygon <- st_transform(bbox_polygon, 4326)
          
          # Transform to WGS84 if needed
          if (st_crs(aoi) != 4326) {
            aoi <- st_transform(aoi, 4326)
          }
          
          aoi <- sf::st_crop(aoi, bbox_polygon)
          
          # Update the leaflet map
          leafletProxy("map") %>%
            addPolygons(
              data = aoi,
              fillColor = "red",
              fillOpacity = 0.2,
              weight = 2,
              color = "red",
              dashArray = "5,5",
              group = "aoi",
              label = ~AOI_name,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "12px",
                direction = "auto"
              )
            )%>%
            addLayersControl(overlayGroups = c("field bbox","aoi") , 
                             options = layersControlOptions(collapsed = FALSE))
        }
      })
      
      showNotification(
        sprintf("AOI column '%s' selected and data cleaned", selected_column),
        type = "message"
      )
    }
  })
  
  # Add reactive value to store AOI data for use in other parts of the app
  aoi_reactive <- reactive({
    req(aoi_data(), aoi_column())
    return(aoi_data())
  })
  
  #----------------------------------------------------------------------------------------------------------
  
  
  # Initialize reactive values
  class_state <- reactiveVal(NULL)
  available_crops <- reactiveVal(character(0))
  class_counter <- reactiveVal(0)
  
  # Initialize all_unique_crops reactive value to store all crops from files
  all_unique_crops <- reactiveVal(NULL)
  
  # Create observer to handle initialization of class_state
  observe({
    req(input$btn_continue)
    req(length(processed_files()) > 1)
    
    if(input$language == "English"){
      color_palette <- app_data$Input_App_data$crop_color_mapping$en
    }else{
      color_palette <- app_data$Input_App_data$crop_color_mapping$de
    }
    
    # Initialize class_state if NULL
    if (is.null(class_state())) {
      # Get all unique crops first
      unique_crops <- get_all_crops(processed_files(), codierung_all, input$language)
      all_unique_crops(unique_crops)
      
      if (input$id_or_name == "Code") {
        # Define base classes
        if(input$language == "English"){
          base_classes <- list(
            "potatoes" = get_names(c(601, 602, 604, 605, 606), codierung_all),
            "kitchen herbs" = get_names(c(650:687), codierung_all),
            "cucurbits" = get_names(c(626:631), codierung_all),
            "winter oil-plant" = get_names(c(311, 315, 390), codierung_all),
            "fodder beet" = get_names(c(413, 414), codierung_all),
            "gardens/plots" = get_names(c(914, 920), codierung_all),
            "vegetables (cruciferous)" = get_names(c(613, 615:618, 611), codierung_all),
            "grassland" = get_names(c(424, 451:460, 443, 444, 492, 493, 702, 844, 855, 912, 928, 991, 992, 960), codierung_all),
            "protein plants" = get_names(c(210:292, 330, 635), codierung_all),
            "clover/lutzerne" = get_names(c(421:423, 425:427, 431:433, 921, 922), codierung_all),
            "solanaceae" = get_names(c(622:624), codierung_all),
            "storage areas" = get_names(c(990, 994, 996), codierung_all),
            "permanent/tree" = get_names(c(564, 834, 840:843, 982, 983), codierung_all),
            "energy plants"  = get_names(c(802:806, 852:854, 866, 871, 801), codierung_all),
            "summer oil-plant" = get_names(c(312, 316, 393, 341), codierung_all),
            "umbelliferae"  = get_names(c(634, 641, 643, 648), codierung_all),
            "fallow" = get_names(c(62, 560, 573, 576, 581:586, 583, 591:595, 849, 884, 910, 885, 593, 849, 884, 886, 887, 961), codierung_all),
            "other vegtables" = get_names(c(610, 647, 644, 639, 638, 636, 637, 851), codierung_all),
            "other fodder crops" = get_names(c(429, 430), codierung_all),
            "summer mixed cereals"  = get_names(c(144, 145), codierung_all),
            "winter mixed cereals" = get_names(c(125, 126), codierung_all),
            "rare cereals"  = get_names(c(181:183, 186, 187, 999), codierung_all),
            "maize"  = get_names(c(171, 172, 177, 411, 919), codierung_all),
            "forest" = get_names(c(556, 568, 952, 955, 956, 995), codierung_all),
            "ornamental plants" = get_names(c(720:776, 778:799, 510:520), codierung_all),
            "flowering area"  = get_names(c(574, 575, 590, 777, 888, 915, 918), codierung_all),
            "mixed crops" = get_names(c(150, 434, 882, 917), codierung_all),
            "millet" = get_names(c(181, 183, 184), codierung_all),
            "mustard" = get_names(c(612, 614, 619), codierung_all),
            "fruits" = get_names(c(480, 481, 821:832, 836:839), codierung_all),
            "landscape elements" = get_names(1:100, codierung_all),
            "hops" = get_names(c(858, 857), codierung_all),
            "accompanying flora" = get_names(c(640, 642), codierung_all),
            "speciality crops" = get_names(c(705, 710), codierung_all)
          )
        }else{
          base_classes <- list(
            "Kartoffeln" = get_names(c(601, 602, 604, 605, 606), codierung_all),
            "Küchenkräuter" = get_names(c(650:687), codierung_all),
            "Kürbisgewächse" = get_names(c(626:631), codierung_all),
            "Winter-Ölpflanzen" = get_names(c(311, 315, 390), codierung_all),
            "Futterrüben" = get_names(c(413, 414), codierung_all),
            "Gärten/Parzellen" = get_names(c(914, 920), codierung_all),
            "Gemüse (Kreuzblütler)" = get_names(c(613, 615:618, 611), codierung_all),
            "Grünland" = get_names(c(424, 451:460, 443, 444, 492, 493, 702, 844, 855, 912, 928, 991, 992, 960), codierung_all),
            "Eiweißpflanzen" = get_names(c(210:292, 330, 635), codierung_all),
            "Klee/Luzerne" = get_names(c(421:423, 425:427, 431:433, 921, 922), codierung_all),
            "Nachtschattengewächse" = get_names(c(622:624), codierung_all),
            "Lagerflächen" = get_names(c(990, 994, 996), codierung_all),
            "Dauerkulturen/Bäume" = get_names(c(564, 834, 840:843, 982, 983), codierung_all),
            "Energiepflanzen" = get_names(c(802:806, 852:854, 866, 871, 801), codierung_all),
            "Sommer-Ölpflanzen" = get_names(c(312, 316, 393, 341), codierung_all),
            "Doldenblütler" = get_names(c(634, 641, 643, 648), codierung_all),
            "Brache" = get_names(c(62, 560, 573, 576, 581:586, 583, 591:595, 849, 884, 910, 885, 593, 849, 884, 886, 887, 961), codierung_all),
            "Sonstiges Gemüse" = get_names(c(610, 647, 644, 639, 638, 636, 637, 851), codierung_all),
            "Sonstige Futterpflanzen" = get_names(c(429, 430), codierung_all),
            "Sommermenggetreide" = get_names(c(144, 145), codierung_all),
            "Wintermenggetreide" = get_names(c(125, 126), codierung_all),
            "Seltene Getreide" = get_names(c(181:183, 186, 187, 999), codierung_all),
            "Mais" = get_names(c(171, 172, 177, 411, 919), codierung_all),
            "Wald" = get_names(c(556, 568, 952, 955, 956, 995), codierung_all),
            "Zierpflanzen" = get_names(c(720:776, 778:799, 510:520), codierung_all),
            "Blühfläche" = get_names(c(574, 575, 590, 777, 888, 915, 918), codierung_all),
            "Mischkulturen" = get_names(c(150, 434, 882, 917), codierung_all),
            "Hirse" = get_names(c(181, 183, 184), codierung_all),
            "Senf" = get_names(c(612, 614, 619), codierung_all),
            "Obst" = get_names(c(480, 481, 821:832, 836:839), codierung_all),
            "Landschaftselemente" = get_names(1:100, codierung_all),
            "Hopfen" = get_names(c(858, 857), codierung_all),
            "Begleitflora" = get_names(c(640, 642), codierung_all),
            "Sonderkulturen" = get_names(c(705, 710), codierung_all)
          )
        }
        
        # Filter out empty classes and those with no matching crops
        filtered_classes <- lapply(base_classes, function(class_names) {
          intersect(class_names, unique_crops)
        })
        filtered_classes <- filtered_classes[sapply(filtered_classes, length) > 0]
        
        # Create initial classes list
        initial_classes <- lapply(seq_along(filtered_classes), function(i) {
          list(
            id = paste0("class", i),
            name = names(filtered_classes)[i],
            crops = as.character(filtered_classes[[i]]),
            color = color_palette[i]
          )
        })
        
        # Sort by number of crops
        sorted_indices <- order(sapply(initial_classes, function(x) length(x$crops)), decreasing = F)
        class_state(initial_classes[sorted_indices])
        class_counter(length(filtered_classes))
        
        # Initially set all crops not in any class as available
        assigned_crops <- unique(unlist(lapply(initial_classes, function(x) x$crops)))
        unassigned_crops <- setdiff(unique_crops, assigned_crops)
        available_crops(unassigned_crops)
        
        # Initialize class names storage
        initial_names <- sapply(base_classes, function(x) names(x))
        class_names(setNames(as.list(initial_names), paste0("class", seq_along(initial_names))))
      }else{
        # For Name-based aggregation, start with empty classes
        class_state(list())
        class_counter(0)
        
        # Make all crops available initially
        available_crops(unique_crops)
        
        # Initialize empty class names storage
        class_names(list())
      }
    }
  })
  
  
  # observer for class name changes
  observe({
    current_state <- class_state()
    if (!is.null(current_state)) {
      current_names <- class_names()
      
      # Update stored names when text inputs change
      for (class in current_state) {
        name_input <- input[[paste0(class$id, "_name")]]
        if (!is.null(name_input)) {
          current_names[[class$id]] <- name_input
        }
      }
      class_names(current_names)
    }
  })
  
  # Update available_crops observer
  observe({
    req(class_state())
    req(all_unique_crops())
    
    # Get currently assigned crops from all classes
    assigned_crops <- character(0)
    for(class in class_state()) {
      class_crops <- input[[class$id]]
      if(!is.null(class_crops)) {
        assigned_crops <- c(assigned_crops, class_crops)
      }
    }
    
    # Set available crops as all unique crops minus assigned crops
    available_crops(setdiff(all_unique_crops(), assigned_crops))
  })
  
  
  # Add this block before where color_palette is used
  color_palette <- reactive({
    if(input$language == "English"){
      app_data$Input_App_data$crop_color_mapping$en
    } else {
      app_data$Input_App_data$crop_color_mapping$de
    }
  })
  
  # Add new aggregation class
  observeEvent(input$add_class, {
    req(class_state())
    current_state <- class_state()
    color_palette <- color_palette()
    counter <- class_counter() + 1
    class_counter(counter)
    
    new_id <- paste0("class", counter)
    new_class <- list(
      id = new_id,
      name = paste("New Class", length(current_state) + 1),
      crops = character(0),  # Start with empty crops vector
      color = color_palette[counter %% length(color_palette) + 1]
    )
    
    class_state(c(current_state, list(new_class)))
  })
  
  # Generate UI for available crops
  output$available_crops_ui <- renderUI({
    bucket_list(
      header = " ",
      group_name = "crop_groups",
      orientation = "vertical",
      add_rank_list(
        text = "Drag codes from here",
        labels = available_crops(),
        input_id = "available_crops"
      )
    )
  })
  
  # Generate UI for aggregation classes
  output$aggregation_classes <- renderUI({
    req(class_state())
    current_state <- class_state()
    current_names <- class_names()
    
    lapply(current_state, function(class) {
      # Get the current name from stored names or default
      current_name <- if (is.null(current_names[[class$id]])) class$name else current_names[[class$id]]
      
      div(
        class = "crop-box",
        style = sprintf("border-color: %s;border-width: 3px;", class$color),
        div(
          class = "name-input",
          div(
            textInput(paste0(class$id, "_name"), 
                      NULL,
                      value = current_name,
                      placeholder = "Class Name"),
            tags$style(sprintf(
              "#%s {background-color: %s; border-color: %s;border-width: 3px;}",
              paste0(class$id, "_name"), 
              adjustcolor(class$color, alpha.f = 0.2),
              class$color
            ))
          )
        ),
        bucket_list(
          header = NULL,
          group_name = "crop_groups",
          orientation = "vertical",
          add_rank_list(
            text = "Drop codes",
            labels = if(!is.null(input[[class$id]])) input[[class$id]] else class$crops,
            input_id = class$id
          )
        ),
        actionButton(paste0(class$id, "_remove"), 
                     "×",
                     class = "btn-danger remove-btn")
      )
    })
  })
  
  # Handle class removal
  observe({
    current_state <- class_state()
    lapply(current_state, function(class) {
      observeEvent(input[[paste0(class$id, "_remove")]], {
        crops_to_return <- input[[class$id]]
        if(!is.null(crops_to_return)) {
          available_crops(c(available_crops(), crops_to_return))
        }
        updated_state <- current_state[sapply(current_state, function(x) x$id != class$id)]
        class_state(updated_state)
      })
    })
  })
  
  # Prepare Sankey data
  prepare_sankey_data <- reactive({
    current_state <- class_state()
    
    color_palette <- color_palette()
    
    mapping <- data.frame(crop = character(), group = character(), color = character(), 
                          stringsAsFactors = FALSE)
    
    # Get mappings from current classes
    for(class in current_state) {
      class_crops <- input[[class$id]]
      if(!is.null(class_crops) && length(class_crops) > 0) {
        mapping <- rbind(mapping,
                         data.frame(
                           crop = class_crops,
                           group = if(is.null(input[[paste0(class$id, "_name")]])) 
                             class$name else input[[paste0(class$id, "_name")]],
                           color = class$color
                         ))
      }
    }
    
    # Add unmapped crops as individual classes with their own names
    unmapped_crops <- available_crops()
    if(length(unmapped_crops) > 0) {
      mapping <- rbind(mapping,
                       data.frame(
                         crop = unmapped_crops,
                         group = unmapped_crops,  # Use crop name as the group name
                         color = sapply(seq_along(unmapped_crops), function(i) {
                           # Generate distinct colors for each unmapped crop
                           color_palette[length(current_state) + i %% length(color_palette) + 1]
                         })
                       ))
    }
    
    # Create nodes and links
    unique_crops <- unique(mapping$crop)
    unique_groups <- unique(mapping$group)
    node_labels <- c(unique_crops, unique_groups)
    
    # Create node colors with distinct colors for individual crops
    node_colors <- c(
      rep("#808080", length(unique_crops)),  # Grey for individual codes
      sapply(unique_groups, function(g) {
        if(g %in% unmapped_crops) {
          # For unmapped crops (individual classes), use their assigned color
          mapping$color[mapping$group == g][1]
        } else {
          # For aggregated classes, use the class color
          mapping$color[mapping$group == g][1]
        }
      })
    )
    
    # Create links with values
    links <- mapping %>%
      mutate(
        source = match(crop, node_labels) - 1,
        target = match(group, node_labels) - 1,
        value = 1  # Constant value for equal visibility
      )
    
    list(
      node = list(
        label = node_labels,
        color = node_colors,
        pad = 15,
        thickness = 20,
        line = list(color = "black", width = 0.5)
      ),
      link = list(
        source = links$source,
        target = links$target,
        value = links$value,
        color = sapply(links$color, function(c) adjustcolor(c, alpha.f = 0.3))
      )
    )
  })
  
  # Generate Plotly Sankey diagram
  output$sankeyPlot <- renderPlotly({
    sankey_data <- prepare_sankey_data()
    
    # Calculate number of unique items (crops + groups)
    n_items <- length(sankey_data$node$label)
    
    # Calculate height based on number of items
    # Assuming we want roughly 100px per item, with some minimum height
    plot_height <- max(800, n_items * 13)  
    
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = sankey_data$node,
      link = sankey_data$link
    ) %>%
      layout(
        title = "Crop Code Aggregation Flow",
        font = list(size = 10),
        xaxis = list(showgrid = FALSE, zeroline = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE),
        hovermode = "x",
        width = NULL,
        height = plot_height
      )
  })
  
  prepare_aggregation_data <- reactive({
    current_states <- class_state()
    current_names <- class_names()
    
    # Initialize empty list to store results
    result <- list()
    
    # Process each class in current_states
    for(class in current_states) {
      # Get the class name from stored names or default
      class_name <- if (is.null(current_names[[class$id]])) class$name else current_names[[class$id]]
      
      # Get the crops for this class from input or default
      class_crops <- if (is.null(input[[class$id]])) class$crops else input[[class$id]]
      
      if(length(class_crops) > 0) {
        if(input$id_or_name == "Code") {
          # Convert crop names to codes
          codes <- unlist(sapply(class_crops, function(x) {
            if(input$language == "English"){
              matches <- codierung_all$NC[codierung_all$english_names == x]
            }else{
              matches <- codierung_all$NC[codierung_all$german_names == x]
            }
            if(length(matches) > 0) matches else NA
          }))
          
          codes <- codes[!is.na(codes)]
          
          if(length(codes) > 0) {
            result[[class_name]] <- codes
          }
        } else {
          # For Name mode, use the crop names directly
          result[[class_name]] <- class_crops
        }
      }
    }
    
    # Handle unmapped crops
    unmapped_crops <- available_crops()
    if(length(unmapped_crops) > 0) {
      if(input$id_or_name == "Code") {
        # For Code mode, convert unmapped crop names to individual codes
        for(crop in unmapped_crops) {
          if(input$language == "English"){
            code <- codierung_all$NC[match(crop, codierung_all$english_names)]
          }else{
            code <- codierung_all$NC[match(crop, codierung_all$german_names)]
          }
          if(!is.na(code)) {
            # Check if this code is already in any existing class
            code_exists <- FALSE
            for(existing_codes in result) {
              if(code %in% existing_codes) {
                code_exists <- TRUE
                break
              }
            }
            
            # Only add if code doesn't exist in any class
            if(!code_exists) {
              result[[crop]] <- code
            }
          }
        }
      } else {
        # For Name mode, add unmapped crops as individual classes
        for(crop in unmapped_crops) {
          # Check if this crop name is already in any existing class
          crop_exists <- FALSE
          for(existing_crops in result) {
            if(crop %in% existing_crops) {
              crop_exists <- TRUE
              break
            }
          }
          
          # Only add if crop doesn't exist in any class
          if(!crop_exists) {
            result[[crop]] <- crop
          }
        }
      }
    }
    
    # Return the converter list
    list(converter = result)
  })
  
  #----------------------------------------------------------------------------------------------------------
  # processor
  processor <- function(current_dir){
    # Start timing and memory tracking
    start_time <- Sys.time()
    initial_mem <- gc(reset = TRUE)
    
    color_palette <- color_palette()
    sankey_data <- prepare_sankey_data()
    all_files <- processed_files()
    years <- sapply(all_files, function(x) x$selected_year)
    aoi_data <- aoi_data()
    
    # Check for large files
    for(i in seq_along(all_files)) {
      if(!is.null(all_files[[i]]) && nrow(all_files[[i]]$sf_object) > 100000) {
        showNotification(
          "One or more files contain over 100,000 features. Processing may take some time...",
          type = "warning",
          duration = NULL
        )
        break
      }
    }
    # processing start message
    showNotification("Processing started...", type = "message")
    
    withProgress(message = 'Processing......', value = 0, {
      tryCatch({
        # Initialize memory checkpoints list
        mem_checkpoints <- list()
        
        incProgress(0.05, detail = "Adding names to all files")
        mem_checkpoints$after_names <- gc(reset = TRUE)
        
        # pre-process the layers
        all_files <- add_names(all_files, codierung_all, input$id_or_name, input$language)
        
        # start intersection
        incProgress(0.05, detail = "Intersecting")
        if(input$radio_process == "complete"){
          CropRotViz_intersection <- intersect_fields(all_files)
        }else{
          CropRotViz_intersection <- intersect_fields_simple(all_files)
        }
        
        mem_checkpoints$after_intersection <- gc(reset = TRUE)
        
        #--------------------------------------------------------------
        # make aggregation if needed
        if(input$aggregation == "Yes" && input$id_or_name == "Code"){
          
          aggregation_codes <- prepare_aggregation_data()
          aggregation_codes <- aggregation_codes$converter
          
          incProgress(0.05, detail = "aggregating")
          
          CropRotViz_intersection <- aggregator(CropRotViz_intersection, years, aggregation_codes, type = "NC")
          mem_checkpoints$after_aggregation <- gc(reset = TRUE)
          
        }else if(input$aggregation == "Yes" && input$id_or_name == "Name"){
          aggregation_codes <- prepare_aggregation_data()
          aggregation_codes <- aggregation_codes$converter
          
          incProgress(0.05, detail = "aggregating")
          CropRotViz_intersection <- aggregator(CropRotViz_intersection, years, aggregation_codes, type = "Name")
          mem_checkpoints$after_aggregation <- gc(reset = TRUE)
        }else{
          aggregation_codes <- NA
        }
        
        #--------------------------------------------------------------
        # intersect with attributes
        incProgress(0.05, detail = "intersecting with areas")
        list_intersect_with_borders <- intersect_with_borders(CropRotViz_intersection, 3, countriesSP,
                                                              EZG, aoi_data)
        
        CropRotViz_intersection <- list_intersect_with_borders$intersected
        
        Districts <- list_intersect_with_borders$borders_inter
        
        if("EZG_inter" %in% names(list_intersect_with_borders)){
          EZGs <- list_intersect_with_borders$EZG_inter
        }else{
          EZGs <- NULL
        }
        
        if("AOI_inter" %in% names(list_intersect_with_borders)){
          AOIs <- list_intersect_with_borders$AOI_inter
        }else{
          AOIs <- NULL
        }
        
        incProgress(0.05, detail = "writing vector file")
        if(vector_file){
          if(input$filetype == "Shapefile"){
            sf::st_write(sf::st_make_valid(CropRotViz_intersection), paste0(current_dir, "/CropRotViz_intersection.shp"), quiet = TRUE, driver = "ESRI Shapefile", append = F, delete_layer = TRUE)
            
          }else if(input$filetype == "GeoPackage"){
            sf::st_write(sf::st_make_valid(CropRotViz_intersection), paste0(current_dir, "/CropRotViz_intersection.gpkg"), quiet = TRUE, append = F, delete_layer = TRUE)
            
          }else{
            sf::st_write(sf::st_make_valid(CropRotViz_intersection), paste0(current_dir, "/CropRotViz_intersection.fgb"), quiet = TRUE, driver = "FlatGeobuf", append = F, delete_layer = T)
          }
        }
        mem_checkpoints$after_file_write <- gc(reset = TRUE)
        
        #--------------------------------------------------------------
        if(input$aggregation == "Yes"){
          agg_cols <- grep("^Aggregated_", names(CropRotViz_intersection), value = TRUE)
        }else{
          agg_cols <- grep("^Name_", names(CropRotViz_intersection), value = TRUE)
        }
        
        # diversity map
        incProgress(0.05, detail = "preparing diversity map")
        
        # diversity mapping
        diversity_data <- handle_diversity_mapping(
          list_intersect_with_borders = list_intersect_with_borders,
          CropRotViz_intersection = CropRotViz_intersection,
          agg_cols = agg_cols,
          Districts = Districts,
          EZGs = EZGs,
          AOIs = AOIs
        )
        
        #--------------------------------------------------------------
        incProgress(0.05, detail = "preparing the rest of the outputs")
        # Modified district processing section with error handling
        district_CropRotViz_intersection <- list()
        
        # district processing section with error handling
        for (i in 1:nrow(Districts)) {
          tryCatch({
            district <- sf::st_drop_geometry(Districts)[i,1]
            
            # Get subset for current district
            current <- subset(CropRotViz_intersection, District == district)
            
            # Check if subset has any rows
            if (nrow(current) > 0) {
              # Process the district data
              current <- current %>%
                plyr::count(vars = agg_cols, wt_var = "area") %>%
                mutate(id = row_number(),
                       freq = freq/1e6)
              
              # Only proceed if we have data after counting
              if (nrow(current) > 0) {
                # Add rotation column
                current$rotation <- do.call(paste, st_drop_geometry(current[agg_cols]))
                district_CropRotViz_intersection[[district]] <- current
              } else {
                warning(sprintf("No data after aggregation for district: %s", district))
                # Add empty placeholder if needed
                district_CropRotViz_intersection[[district]] <- data.frame()
              }
            } else {
              warning(sprintf("No data found for district: %s", district))
              # Add empty placeholder if needed
              district_CropRotViz_intersection[[district]] <- data.frame()
            }
          }, error = function(e) {
            warning(sprintf("Error processing district %s: %s", district, e$message))
            # Add empty placeholder if needed
            district_CropRotViz_intersection[[district]] <- data.frame()
          })
        }
        
        # After the loop, check if we have any valid data
        if (all(sapply(district_CropRotViz_intersection, nrow) == 0)) {
          stop("No valid data found for any district")
        }
        
        # Remove any empty districts from the list 
        district_CropRotViz_intersection <- district_CropRotViz_intersection[
          sapply(district_CropRotViz_intersection, nrow) > 0
        ]
        mem_checkpoints$after_district_processing <- gc(reset = TRUE)
        
        if("EZG" %in% names(CropRotViz_intersection)){
          EZG_CropRotViz_intersection <- list()
          
          for (i in 1:length(unique(st_drop_geometry(EZGs)$EZG))) {
            EZG_sel <- unique(st_drop_geometry(EZGs)$EZG)[i]
            
            current <- subset(CropRotViz_intersection, EZG == EZG_sel)
            current <- current%>%
              plyr::count(vars = agg_cols, wt_var = "area")%>%
              mutate(id = row_number(),
                     freq = freq/1e6)
            
            # ad rotation column
            current$rotation <- do.call(paste, st_drop_geometry(current[agg_cols]))  
            
            EZG_CropRotViz_intersection[[EZG_sel]] <- current
          }
        }else{
          EZGs  <- NA
          EZG_CropRotViz_intersection <- NA
        }
        mem_checkpoints$after_EZG_processing <- gc(reset = TRUE)
        
        if("AOI" %in% names(CropRotViz_intersection)){
          AOI_CropRotViz_intersection <- list()
          
          for (i in 1:length(unique(st_drop_geometry(AOIs)$AOI))) {
            AOI_sel <- unique(st_drop_geometry(AOIs)$AOI)[i]
            
            current <- subset(CropRotViz_intersection, AOI == AOI_sel)
            current <- current%>%
              plyr::count(vars = agg_cols, wt_var = "area")%>%
              mutate(id = row_number(),
                     freq = freq/1e6)
            
            # ad rotation column
            current$rotation <- do.call(paste, st_drop_geometry(current[agg_cols]))  
            
            AOI_CropRotViz_intersection[[AOI_sel]] <- current
          }
        }else{
          AOIs  <- NA
          AOI_CropRotViz_intersection <- NA
        }
        
        #----------------------------------------------------------------------------------------------------------------------------------
        # calculate complete cropping area
        cropping_area <- sum(do.call(rbind, district_CropRotViz_intersection)$freq)
        
        # create all available choices
        if(input$aggregation == "Yes"){
          Crop_choices <- unique(unlist(unique(c(st_drop_geometry(CropRotViz_intersection)[grep("^Aggregated_", names(CropRotViz_intersection), value = TRUE)]))))
        }else{
          Crop_choices <- unique(unlist(unique(c(st_drop_geometry(CropRotViz_intersection)[grep("^Name_", names(CropRotViz_intersection), value = TRUE)]))))
        }
        
        # create df for area distribution visualisation
        agg_cols <- c(
          agg_cols,
          "District", 
          "area",
          if ("EZG" %in% names(CropRotViz_intersection)) "EZG",
          if ("EZG" %in% names(CropRotViz_intersection) && 
              "AOI" %in% names(CropRotViz_intersection)) "AOI"
        )
        
        distribution_df <- st_drop_geometry(CropRotViz_intersection)[,agg_cols]
        
        language <- input$language
        
        # save environment
        save(EZG_CropRotViz_intersection, district_CropRotViz_intersection, AOI_CropRotViz_intersection,
             cropping_area, years, Crop_choices, Districts, EZGs,
             distribution_df, diversity_data, sankey_data, aggregation_codes, language, 
             file = paste0(current_dir, "/CropRotViz_intersection.RData"))
        
        #-----------------------------------------------------------------------------------------------------
        incProgress(0.05, detail = "preparing images for fast preview version")
        
        # create snapshot if preview == T
        if(preview){
          data <- do.call(rbind, district_CropRotViz_intersection)
          data$id <- 1:nrow(data)
          create_crop_rotation_sankey(data, 
                                      output_path = paste0(current_dir, "/CropRotViz_intersection.png"), 
                                      min_area = 0, 
                                      color = color_palette)
        }
        
        if(input$fastImages == "Yes"){
          # create dir
          dir.create(paste0(current_dir, "/images"))
          
          # plot the districts 
          for(name in names(district_CropRotViz_intersection)){
            snipped <- district_CropRotViz_intersection[[name]]
            sanitized_name <- gsub("/", "_", name)
            if(sum(snipped$freq) > 1){
              create_crop_rotation_sankey(snipped,
                                          output_path = paste0(current_dir, "/images/", sanitized_name, ".png"),
                                          min_area = 0, 
                                          exclude_crops = c(),
                                          color = color_palette)
            }
          }
          
          # plot the EZGs
          for(name in names(EZG_CropRotViz_intersection)){
            snipped <- EZG_CropRotViz_intersection[[name]]
            sanitized_name <- gsub("/", "_", name)
            if(sum(snipped$freq) > 1){
              create_crop_rotation_sankey(snipped,
                                          output_path = paste0(current_dir, "/images/", sanitized_name, ".png"), 
                                          min_area = 0,
                                          exclude_crops = c(),
                                          color = color_palette)
            }
          }
          
          # plot the AOIs
          for(name in names(AOI_CropRotViz_intersection)){
            snipped <- AOI_CropRotViz_intersection[[name]]
            sanitized_name <- gsub("/", "_", name)
            if(sum(snipped$freq) > 1){
              create_crop_rotation_sankey(snipped,
                                          output_path = paste0(current_dir, "/images/", sanitized_name, ".png"), 
                                          min_area = 0,
                                          exclude_crops = c(),
                                          color = color_palette)
            }
          }
        }
        
        #--------------------------------------------------------------------------------------------
        mem_checkpoints$after_save <- gc(reset = TRUE)
        
        # write meta file
        file_conn <- file(paste0(current_dir, "/CropRotViz_processing_info.txt"), "w")
        writeLines("Processing Information Summary for the field intersection\n", file_conn)
        writeLines(paste("Processing Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), file_conn)
        writeLines("----------------------------------------\n", file_conn)
        
        for(i in seq_along(all_files)) {
          if(!is.null(all_files[[i]])) {
            # Write file information
            writeLines(sprintf("Year %d:", i), file_conn)
            writeLines(sprintf("  Path: %s", all_files[[i]]$filepath), file_conn)
            writeLines(sprintf("  Selected Column: %s", all_files[[i]]$selected_column), file_conn)
            writeLines(sprintf("  Selected Year: %s", all_files[[i]]$selected_year), file_conn)
            writeLines(sprintf("  Number of Features: %d", nrow(all_files[[i]]$sf_object)), file_conn)
            writeLines("", file_conn)  # Empty line between entries
          }
        }
        
        # Calculate final memory and time
        final_mem <- gc(reset = TRUE)
        end_time <- Sys.time()
        
        # Write performance metrics
        writeLines("\nPerformance Metrics:", file_conn)
        writeLines("----------------------------------------", file_conn)
        writeLines(paste("Total Processing Time:", format(difftime(end_time, start_time, units = "auto"))), file_conn)
        writeLines("\nMemory Usage Checkpoints (in MB):", file_conn)
        writeLines(paste("  Initial:", round(sum(initial_mem[,2]) / 1024, 2)), file_conn)
        writeLines(paste("  After Names:", round(sum(mem_checkpoints$after_names[,2]) / 1024, 2)), file_conn)
        writeLines(paste("  After Intersection:", round(sum(mem_checkpoints$after_intersection[,2]) / 1024, 2)), file_conn)
        if(!is.null(mem_checkpoints$after_aggregation)) {
          writeLines(paste("  After Aggregation:", round(sum(mem_checkpoints$after_aggregation[,2]) / 1024, 2)), file_conn)
        }
        writeLines(paste("  After File Write:", round(sum(mem_checkpoints$after_file_write[,2]) / 1024, 2)), file_conn)
        writeLines(paste("  After District Processing:", round(sum(mem_checkpoints$after_district_processing[,2]) / 1024, 2)), file_conn)
        writeLines(paste("  After EZG Processing:", round(sum(mem_checkpoints$after_EZG_processing[,2]) / 1024, 2)), file_conn)
        writeLines(paste("  After Save:", round(sum(mem_checkpoints$after_save[,2]) / 1024, 2)), file_conn)
        writeLines(paste("  Final:", round(sum(final_mem[,2]) / 1024, 2)), file_conn)
        writeLines(paste("\nPeak Memory Usage:", round(max(sapply(c(list(initial_mem), mem_checkpoints, list(final_mem)), 
                                                                  function(x) sum(x[,2]))) / 1024, 2), "MB"), file_conn)
        writeLines("\n----------------------------------------", file_conn)
        writeLines(paste("Number of Features after intersection:", nrow(CropRotViz_intersection)), file_conn)
        writeLines("\nOptions", file_conn)
        writeLines(paste("Aggregation:", input$aggregation), file_conn)
        writeLines(paste("Intersection:", input$radio_process), file_conn)
        writeLines("\n----------------------------------------", file_conn)
        writeLines("Author: Franz Schulze", file_conn)
        writeLines("If you use CropRotationViz in your research, please cite it:", file_conn)
        writeLines("@software{CropRotationViz2024,", file_conn)
        writeLines("  author = {Schulze, Franz},", file_conn)
        writeLines("  title = {CropRotationViz: Interactive Tool for Crop Rotation Sequence Analysis},", file_conn)
        writeLines("  year = {2024},", file_conn)
        writeLines("  publisher = {GitHub},", file_conn)
        writeLines("  journal = {GitHub repository},", file_conn)
        writeLines("  url = {https://github.com/franz-geoeco/CropRotationViz}", file_conn)
        writeLines("}", file_conn)
        
        close(file_conn)
        
        print(paste(end_time, start_time))
        
        
        # Show completion notification with performance metrics
        showNotification(
          paste("Processing completed successfully!",
                "\nTotal time:", format(difftime(end_time, start_time, units = "auto")),
                "\nPeak memory usage:", round(max(sapply(c(list(initial_mem), mem_checkpoints, list(final_mem)), 
                                                         function(x) sum(x[,2]))) / 1024, 2), "MB"),
          type = "message",
          duration = 10
        )
        message("Processing completed successfully!")
        
      }, error = function(e) {
        end_time <- Sys.time()
        showNotification(paste("Error during processing:", e$message,
                               "\nTime elapsed:", format(difftime(end_time, start_time, units = "auto"))),
                         type = "error")
      })
    })
  }
  
  #----------------------------------------------------------------------------------------------------------
  
  # Process button observer with output_dir
  observeEvent(input$btn_process, {
    if (!is.na(output_dir)) {
      processor(output_dir)
    } else {
      req(selected_dir())  # Ensure directory is selected
      processor(get_selected_dir())
    }
  })
  
  # Return the processed files reactive expression
  return(reactive({ processed_files() }))
}