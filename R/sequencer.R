#' Crop Sequence Builder UI Component
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
#'         \item Choice between NC (National Coding) or Name-based crop identification
#'         \item Optional crop class aggregation for NC-based analysis
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
#' ui <- sequencer_ui(app_data)
#' 
#' # Initialize with specific output directory and start year
#' ui <- sequencer_ui(
#'   app_data,
#'   output_dir = "path/to/output",
#'   start_year = 2020
#' )
#' }
#' 
#' @keywords internal
sequencer_ui <- function(app_data, output_dir = NA, start_year = NA, vector_file = TRUE) {
  fluidPage(
    theme = shinytheme("cyborg"),
    br(),
    fluidRow(
      column(style = "margin-top: -0px; margin-bottom: 0px;", width = 5, h2("Crop Sequence Builder")), 
      column(4),
      column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
    ),
    tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
    
    fluidRow(column(4,
                    # Counter display
                    tags$span(
                      textOutput("file_counter"), 
                      style = "font-size: 20px; color: #ffffff; font-weight: bold"
                    ),br()),
             column(2,
                    radioButtons("id_or_name", label = "Select Crop Column", choices = c("NC", "Name"), inline = T)
             ),
             column(1,
                    bsPopover(
                      id = "id-info",
                      title = "Crop Column",
                      content = HTML(paste0(
                        "Here you can select the coloumn, which describes your crop. Use NC if you have want to use a the national coding or/and you want to use the aggreagation, which summarizes some smaller classes to bigger groups."
                      )),
                      placement = "left",
                      trigger = "hover",
                      options = list(container = "body")
                    ),
                    bsButton("id-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")
             ),
             conditionalPanel(
               condition = "input.id_or_name == 'NC'",
               column(2,             
                      radioButtons("aggregation", 
                                   label = "Aggregation of crop classes", 
                                   choices = c("Yes", "No"), 
                                   inline = TRUE)
               )
               
             ),
             column(2,
                    radioButtons("radio_process", "Type of field intersection",
                                 c("Complete" = "complete",
                                   "Fast" = "fast"), inline = T)
             ),
             column(1,
                    bsPopover(
                      id = "radio-info",
                      title = "Type of intersection",
                      content = HTML(paste0(
                        "Here you can select the type of the intersection framework. he fast intersect just over the years your fields and leaves out fields not present in a year. The coprhensive method is much slower but contains each area you put in with comprehesive union functions."
                      )),
                      placement = "left",
                      trigger = "hover",
                      options = list(container = "body")
                    ),
                    bsButton("radio-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")
             )
    ),
    
    tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
    
    # Container for dynamic file selectors and their components
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
            sprintf("Choose File year: %d", i),
            title = sprintf("Please select polygon file year: %d (.shp, .geojson, .fgb, .gpkg or .sqlite):", i),
            multiple = FALSE,
            buttonType = "default",
            class = "btn-primary", # Add Bootstrap button class for color
            style = "background-color: rgb(116, 150, 30); border-color: rgb(116, 150, 30); color: white;" # Or use direct CSS styling
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
                     sprintf("Column describing the NC or Name #%d:", i),
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
          }else{
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
    fluidRow(
      column(4,
             # Display current list of loaded files
             verbatimTextOutput("loaded_files_summary")
      ),
      column(2),
      column(4,
             leafletOutput("map")
      )
    ),
    tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
    fluidRow(
      if(is.na(output_dir)) {
        column(width = 4,
               conditionalPanel(
                 condition = "output.show_process_button",
                 shinyDirButton(
                   "dir", 
                   "Choose Output Directory", 
                   "Choose an output directory", 
                   style = "background-color: rgb(116, 150, 30); border-color: rgb(116, 150, 30); color: white;"
                 ),
                 verbatimTextOutput("selected_dir")
               )
        )
      },
      if(vector_file){
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
      column(3,
             radioButtons("fastImages",
                          "Do wou want a fast visualization version?",
                          c("Yes", "No"), inline = T)
      )
    ),
    fluidRow(
      column(4,
             conditionalPanel(
               condition = "output.show_process_button",
               actionButton("btn_process", "Process Files", 
                            class = "btn-primary btn-lg",
                            style = "margin-top: 20px;")
             )
      )
    ), br(), br(),br(),br(),
    
    tags$footer(
      style = "position: fixed; 
           bottom: 0; 
           width: 100%; 
           background-color: #f5f5f5; 
           padding: 10px; 
           text-align: center;
           border-top: 1px solid #e7e7e7;
           left: 0;
           z-index: 1000;",  # Added z-index to ensure visibility
      
      div(
        style = "display: inline-block;",
        p(
          tags$span("Author: "),  # Using tags$span for text
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
          tags$span("Institution: "),  # Using tags$span for text
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
#' @import shiny
#' @import sf
#' @import leaflet
#' @import tools
#' @import ggalluvial
#' @importFrom shinyFiles shinyFileChoose parseFilePaths parseDirPath
#' @importFrom ggalluvial geom_stratum geom_flow
#' 
#' @examples
#' \dontrun{
#' # Basic server initialization
#' server <- function(input, output, session) {
#'   sequencer_server(input, output, session, app_data)
#' }
#' 
#' # Server with custom output directory and column mapping
#' server <- function(input, output, session) {
#'   sequencer_server(
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
sequencer_server <- function(input, output, session, app_data, output_dir = NA, common_column = NA, preview = TRUE, vector_file = TRUE) {
  # Start timing
  start_time <- Sys.time()
  initial_mem <- gc(reset = TRUE)
  
  # global options
  options(warn = -1)
  sf_use_s2(F)
  
  # Show welcome alert when the app starts
  observe({
    # Only show once when session starts
    shinyalert(
      title = "Welcome!",
      text = paste("This application allows you to combine multiple annual cropping polygon layers into a comprehensive crop sequence.",
                   "Select in the following your annual files and process it. Whean the processing ends you can close the app and start the visualization application."),
      type = "info",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "Let's Begin!",
      confirmButtonCol = "#3085d6",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  # Access the data
  codierung_all <- app_data$Input_App_data$codierung_all
  crop_codes    <- app_data$Input_App_data$crop_codes
  display_names <- app_data$Input_App_data$display_names
  EZG           <- app_data$Input_App_data$EZG
  volumes       <- getVolumes()
  
  #########
  # Set the root directory where user can select from
  if(is.na(output_dir)){
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  }else{
    volumes <- c(Home = output_dir, "R Installation" = R.home(), getVolumes()())
  }
  
  # Initialize directory selection
  shinyDirChoose(
    input,
    "dir",
    roots = volumes,
    session = session
  )
  
  # Create reactive value to store the selected directory path
  selected_dir <- reactiveVal()
  
  # Display selected directory
  output$selected_dir <- renderPrint({
    if (length(input$dir) > 1) {
      parseDirPath(volumes, input$dir)
    }else{
      cat( "No directory selected (input dir)")
    }
  })
  
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
  #----------------------------------------------------------------------------------------------------------
  
  # Function to process shapefile selection
  processShapefileSelection <- function(input_id, file_reactive, column_selector_id, common_column = NA) {
    shinyFileChoose(input, input_id,
                    roots = volumes,
                    session = session,
                    restrictions = system.file(package = "base"),
                    filetypes = c("shp", "fgb", "gpkg", "sqlite", "geojson"))
    
    if(!is.null(input[[input_id]])) {
      file_selected <- shinyFiles::parseFilePaths(volumes, input[[input_id]])
      
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
          dashArray = "5,5"
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
  
  # processor
  processor <- function(current_dir){
    # Start timing and memory tracking
    start_time <- Sys.time()
    initial_mem <- gc(reset = TRUE)
    
    all_files <- processed_files()
    years <- sapply(all_files, function(x) x$selected_year)
    
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
        all_files <- add_names(all_files, codierung_all, input$id_or_name)

        # start intersection
        incProgress(0.05, detail = "Intersecting")
        if(input$radio_process == "complete"){
          CropRotViz_intersection <- intersect_fields(all_files)
        }else{
          CropRotViz_intersection <- intersect_fields_simple(all_files)
        }
        mem_checkpoints$after_intersection <- gc(reset = TRUE)
        
        # make aggregation if needed
        if(input$aggregation == "Yes" & input$id_or_name == "NC"){
          incProgress(0.05, detail = "aggregating")
          CropRotViz_intersection <- aggregator(CropRotViz_intersection, years, crop_codes, display_names)
          mem_checkpoints$after_aggregation <- gc(reset = TRUE)
        }
        
        # intersect with attributes
        incProgress(0.05, detail = "intersecting with areas")
        list_intersect_with_borders <- intersect_with_borders(CropRotViz_intersection, 3, EZG)
        
        CropRotViz_intersection <- list_intersect_with_borders[[1]]
        
        Districts <- list_intersect_with_borders[[2]]
        
        if(length(list_intersect_with_borders) == 3){
          EZGs <- list_intersect_with_borders[[3]]
        }
        
        # calculate area in m²
        CropRotViz_intersection$area <- as.numeric(st_area(CropRotViz_intersection))
        
        incProgress(0.05, detail = "writing vector file")
        if(vector_file){
          if(input$filetype == "Shapefile"){
            st_write(CropRotViz_intersection, paste0(current_dir, "/CropRotViz_intersection.shp"), quiet = TRUE, driver = "ESRI Shapefile", append = F, delete_layer = TRUE)
            
          }else if(input$filetype == "GeoPackage"){
            st_write(CropRotViz_intersection, paste0(current_dir, "/CropRotViz_intersection.gpkg"), quiet = TRUE, append = F, delete_layer = TRUE)
            
          }else{
            st_write(CropRotViz_intersection, paste0(current_dir, "/CropRotViz_intersection.fgb"), quiet = TRUE, driver = "FlatGeobuf", append = F, delete_layer = T)
          }
        }
        mem_checkpoints$after_file_write <- gc(reset = TRUE)
        
        if(input$id_or_name == "NC" & input$aggregation == "Yes"){
          agg_cols <- grep("^Aggregated_", names(CropRotViz_intersection), value = TRUE)
        }else{
          agg_cols <- grep("^Crop_", names(CropRotViz_intersection), value = TRUE)
        }
        
        # diversity map
        incProgress(0.05, detail = "preparing diversity map")
        
        # Function to check if there's enough data for diversity mapping
        has_sufficient_data <- function(data_list, min_rows = 9, min_years = 3) {
          return(dim(data_list)[1] > min_rows && length(years) > min_years)
        }
        
        # Handle diversity mapping based on available borders
        diversity_data <- if (length(list_intersect_with_borders) == 3) {
          if (has_sufficient_data(list_intersect_with_borders[[3]])) {
            diversity_mapping(CropRotViz_intersection, agg_cols, Districts, EZGs)
          } else {
            NA
          }
        } else {
          if (has_sufficient_data(list_intersect_with_borders[[2]])) {
            diversity_mapping(CropRotViz_intersection, agg_cols, Districts)
          } else {
            NA
          }
        }
        
        # preparing district_CropRotViz_intersection list
        district_CropRotViz_intersection <- list()
        
        incProgress(0.05, detail = "preparing the rest of the outputs")
        for (i in 1:length(unique(st_drop_geometry(Districts)[,1]))) {
          
          district <- unique(st_drop_geometry(Districts)[,1])[i]
          current <- subset(CropRotViz_intersection, District == district)
          
          current <- current%>%
            plyr::count(vars = agg_cols, wt_var = "area")%>%
            mutate(id = row_number(),
                   freq = freq/1e6)
          
          # ad rotation column
          current$rotation <- do.call(paste, st_drop_geometry(current[agg_cols]))
          
          district_CropRotViz_intersection[[district]] <- current
        }
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
        
        # calculate complete cropping area
        cropping_area <- sum(do.call(rbind, district_CropRotViz_intersection)$freq)
        
        # create all available choices
        if(input$id_or_name == "NC" & input$aggregation == "Yes"){
          Crop_choices <- unique(unlist(unique(c(st_drop_geometry(CropRotViz_intersection)[grep("^Aggregated_", names(CropRotViz_intersection), value = TRUE)]))))
        }else{
          Crop_choices <- unique(unlist(unique(c(st_drop_geometry(CropRotViz_intersection)[grep("^Crop_", names(CropRotViz_intersection), value = TRUE)]))))
        }
        
        
        # create df for area distribution visualisation
        if(length(list_intersect_with_borders) == 3){
          agg_cols <- c(agg_cols, "EZG", "District", "area")
        }else{
          agg_cols <- c(agg_cols, "District", "area")
        }
        distribution_df <- st_drop_geometry(CropRotViz_intersection)[,agg_cols]
        
        # save environment
        save(EZG_CropRotViz_intersection, district_CropRotViz_intersection, cropping_area, years, Crop_choices, Districts, EZGs, distribution_df, diversity_data, file = paste0(current_dir, "/CropRotViz_intersection.RData"))
        
        #---------------------------------------------------
        incProgress(0.05, detail = "preparing images for fast preview version")
        
        # create snapshot if preview == T
        if(preview){
          data <- do.call(rbind, district_CropRotViz_intersection)
          data$id <- 1:nrow(data)
          create_crop_rotation_sankey(data, 
                                      output_path = paste0(current_dir, "/CropRotViz_intersection.png"), 
                                      min_area = 0, 
                                      color = app_data$Input_App_data$crop_color_mapping)
        }
        
        if(input$fastImages == "Yes"){
          # create dir
          dir.create(paste0(current_dir, "/images"))

          # plot the districts 
          for(name in names(district_CropRotViz_intersection)){
            snipped <- district_CropRotViz_intersection[[name]]
            sanitized_name <- gsub("/", "_", name)
            plot <- create_crop_rotation_sankey(snipped,
                                                output_path = paste0(current_dir, "/images/", sanitized_name, ".png"),
                                                min_area = 0, 
                                                exclude_crops = c(),
                                                color = app_data$Input_App_data$crop_color_mapping)
          }
          
          # plot the EZGs
          for(name in names(EZG_CropRotViz_intersection)){
            snipped <- EZG_CropRotViz_intersection[[name]]
            sanitized_name <- gsub("/", "_", name)
            plot <- create_crop_rotation_sankey(snipped,
                                                output_path = paste0(current_dir, "/images/", sanitized_name, ".png"), 
                                                min_area = 0,
                                                exclude_crops = c(),
                                                color = app_data$Input_App_data$crop_color_mapping)
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
        writeLines("  url = {https://github.com/franzschulze/CropRotationViz}", file_conn)
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
  
  # Process button observer
  observeEvent(input$btn_process, {
    req(selected_dir())  # Ensure directory is selected
    processor(get_selected_dir())
  })
  
  # Process button observer
  observeEvent(input$btn_process, {
    req(output_dir)  # Ensure directory is selected
    processor(output_dir)
  })
  return(reactive({ processed_files() }))
}

