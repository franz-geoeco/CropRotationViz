#' Interactive District/River Catchment Map UI
#' 
#' @description Creates an interactive mapping interface that allows users to compare
#' districts or river catchments through a side-by-side visualization layout. The interface
#' includes a central map with selectable regions and panels for displaying related images.
#'
#' @details The UI consists of three main components:
#'   \itemize{
#'     \item Control Panel:
#'       \itemize{
#'         \item Map type selector (Districts/Catchment)
#'     }
#'     \item Central Interactive Map:
#'       \itemize{
#'         \item Leaflet-based map for region selection
#'         \item Interactive selection capabilities
#'     }
#'     \item Comparison Panels:
#'       \itemize{
#'         \item Left panel for first selection visualization
#'         \item Right panel for second selection visualization
#'         \item Conditional display based on user selections
#'     }
#'   }
#' 
#' @section Theme:
#' Uses the "cyborg" theme with custom styling for:
#'   \itemize{
#'     \item Title formatting
#'     \item Conditional panel headers (red/orange)
#'     \item Layout spacing and alignment
#'   }
#'
#' @param input_dir Optional input directory path. Default is NA.
#' 
#' @return A Shiny UI object containing a fluid page layout with map and comparison panels
#' 
#' @import shiny
#' @import shinythemes
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column
#'   radioButtons conditionalPanel div textOutput imageOutput
#' @importFrom shinythemes shinytheme
#' @importFrom leaflet leafletOutput
#'
#' @examples
#' \dontrun{
#' # Run the app with this UI
#' shinyApp(ui = fast_viz_ui, server = server)
#' }
#' 
#' @export
fast_viz_ui <- function(input_dir = NA){
  
  # Extract years and create span string outside the fluidRow
  year_span <- paste0(
    min(as.numeric(gsub(".*?(\\d+).*", "\\1", 
                        names(district_CropRotViz_intersection[[1]])[grepl("\\d", names(district_CropRotViz_intersection[[1]]))]))),
    "-",
    max(as.numeric(gsub(".*?(\\d+).*", "\\1", 
                        names(district_CropRotViz_intersection[[1]])[grepl("\\d", names(district_CropRotViz_intersection[[1]]))])))
  )
  
  fluidPage(
    theme = shinytheme("cyborg"),
    
    tabsetPanel(id = "tabs",
                tabPanel("Plot Sequence",
                         
                         titlePanel("Interactive District/River Catchment Crop Sequence Map"),
                         fluidRow(
                           column(3,
                                  radioButtons("mapType", 
                                               "Select Map Type:", inline = TRUE,
                                               choices = c("Districts" = "districts",
                                                           "Catchment" = "ezg"))
                           ),
                           column(5,
                                  h4("Click on one or two areas of interest click again to hide them again. All charts are saved in your folder in `images`"))
                         ),
                         fluidRow(
                           column(5,
                                  conditionalPanel(
                                    condition = "output.hasFirstSelection",
                                    div(textOutput("plotFirstTitle"), 
                                        style="color: red; font-size: 24px; font-weight: bold; margin: 10px 0;")
                                  )
                           ),
                           column(2),
                           column(5,
                                  conditionalPanel(
                                    condition = "output.hasSecondSelection",
                                    div(textOutput("plotSecondTitle"), 
                                        style="color: orange; font-size: 24px; font-weight: bold; margin: 10px 0;")
                                  )
                           )
                         ),
                         fluidRow(
                           # Left image panel
                           column(5,
                                  conditionalPanel(
                                    condition = "output.hasFirstSelection",
                                    imageOutput("firstImage", inline = T)
                                  )
                           ),
                           # Center map panel
                           column(2,
                                  leafletOutput("map", height = "600px")
                           ),
                           # Right image panel
                           column(5,
                                  conditionalPanel(
                                    condition = "output.hasSecondSelection",
                                    imageOutput("secondImage", inline = T)
                                  )
                           )
                         ),
                         # Horizontal line
                         tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                         fluidRow(
                           column(1),
                           column(7,
                                  h4("Crop Class Aggregation Visualization"))
                         ),
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    plotlyOutput("sankey_plotly")
                                  )
                           )
                         )
                ),
                if (exists("diversity_data") && is.list(diversity_data) && length(diversity_data) > 0) {
                  tabPanel("Plot Diversity",
                           titlePanel("Interactive District/River Catchment Crop Diversity Map"),
                           fluidRow(column(3,
                                           radioButtons("AreaType", 
                                                        "Select Map Type:", inline = TRUE,
                                                        choices = c("Districts" = "districts",
                                                                    "Catchment" = "ezg"))),
                                    column(5,
                                           h4(paste0("The Structural Diversity is defined by the number of transitions and unique crops over the period ", 
                                                     year_span, 
                                                     ". The map shows the area weighted mean value per area."))
                                    )),
                           fluidRow(column(1),
                                    column(10,
                                           leafletOutput("diversity", height = "650px")
                                    )),
                           
                  )
                },
                
                # Custom footer
                tags$footer(
                  style = "position: fixed; 
                           bottom: 0; 
                           width: 100%; 
                           background-color: #f5f5f5; 
                           padding: 10px; 
                           text-align: center;
                           border-top: 1px solid #e7e7e7;
                           left: 0;
                           z-index: 2000;",  # Added z-index to ensure visibility
                  
                  div(
                    style = "display: inline-block;",
                    p(
                      tags$span("Author: "),  # Using tags$span for text
                      tags$a(
                        href = "https://github.com/franz-geoeco/CropRotationViz", 
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
  )
}

#' Main Application UI with Data Loader
#' 
#' @description Creates the main application UI with a dynamic loader interface that
#' switches to the visualization UI once data is loaded. Provides a file upload
#' interface for loading RData files containing crop rotation data.
#'
#' @param app_data List containing application data including:
#'   \itemize{
#'     \item Input_App_data - List with loaded environment and other configuration
#'   }
#'
#' @details The UI transitions through two stages:
#'   \itemize{
#'     \item Initial loader interface:
#'       \itemize{
#'         \item RData file upload
#'         \item Variable validation
#'         \item Progress indication
#'       }
#'     \item Main visualization interface:
#'       \itemize{
#'         \item Full visualization capabilities
#'         \item Interactive components
#'         \item Data analysis tools
#'       }
#'   }
#' 
#' @return A Shiny UI object with dynamic loading capabilities
#' 
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column
#'   plotOutput uiOutput renderUI verbatimTextOutput fileInput
#'   actionButton wellPanel
#'   
#' @examples
#' \dontrun{
#' shinyApp(ui = ui, server = server)
#' }
fast_ui <- function(app_data) {
  # Load package data
  utils::data("Input_App_data", envir = environment())
  
  fluidPage(
    theme = shinytheme("cyborg"),
    
    # Conditional UI - show either loader or main app
    uiOutput("fast_dynamic_ui")
  )
}

#' Interactive District/River Catchment Map Server
#' 
#' @description Server logic for an interactive mapping application that enables users to 
#' load environmental data and compare visualizations between different districts or river catchments.
#' The server handles data loading, map interactions, and dynamic image rendering.
#'
#' @details The server functionality includes:
#'   \itemize{
#'     \item Data Loading Interface:
#'       \itemize{
#'         \item RData file upload capability
#'         \item Validation of required variables
#'         \item Dynamic UI transitions based on data state
#'     }
#'     \item Map Interaction Handling:
#'       \itemize{
#'         \item District/Catchment selection tracking
#'         \item Interactive highlighting of selected regions
#'         \item Toggle selection functionality
#'     }
#'     \item Visualization Management:
#'       \itemize{
#'         \item Dynamic loading of region-specific images
#'         \item Conditional panel display logic
#'         \item Title updates based on selections
#'     }
#'   }
#' 
#' @section Required Variables:
#' The following variables must be present in the loaded RData file:
#'   \itemize{
#'     \item district_CropRotViz_intersection
#'     \item cropping_area
#'     \item Crop_choices
#'     \item Districts
#'   }
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param app_data Application data list
#' @param input_dir Optional directory path for data loading. Default is NA.
#' 
#' @return A Shiny server function that manages the interactive mapping interface
#' 
#' @import shiny
#' @import leaflet
#' @importFrom sf st_transform st_coordinates st_centroid
#' @importFrom shiny reactiveVal observeEvent renderUI showNotification
#'
#'
#' @examples
#' \dontrun{
#' # Run the app with this server
#' shinyApp(ui = fast_viz_ui, server = fast_viz_server)
#' }
#' 
#' @export
fast_viz_server <- function(input, output, session, app_data, input_dir) {
  # global options
  options(warn = -1)
  sf_use_s2(F)
  options(shiny.maxRequestSize=1024^3 )
  
  # Reactive value to track if data is loaded
  data_loaded <- reactiveVal(FALSE)
  
  loaded_env <- app_data$Input_App_data$loaded_env
  
  # Original viz_server logic
  # ALERTS
  observeEvent(input$tabs, {
    if(input$tabs == "Plot Sequence"){
      text_1 <- "Plot Crop Sequences"
      text_2 <- "This is a fast version of the sequence plotter. Here you can select one or two areas on the map and compare the crop sequences."
    } else if(input$tabs == "Plot Diversity"){
      text_1 <- "Plot Crop Diversity Map"
      text_2 <- "Here you can plot and inspect the structural crop diversity per catchment or district. The diversity coloring is defined by the number of transition and the number of unique crops per area."
    }
    shinyalert(text_1, text_2, type = "info")
  })
  
  #--------------------------------------------------------------------------------------------
  # Render dynamic UI based on whether data is loaded
  output$fast_dynamic_ui <- renderUI({
    if (!data_loaded() & is.na(input_dir)) {
      # Show loader interface
      fluidPage(
        br(),
        fluidRow(
          column(width = 4, 
                 offset = 4,
                 wellPanel(
                   style = "background-color: #164f8c; color: white;",
                   h3("Load Environment Data", align = "center"),
                   fileInput("file", "Choose .RData File",
                             accept = c(".RData"),
                             multiple = FALSE),
                   verbatimTextOutput("loaded_vars"),
                   actionButton("proceed", "Proceed to App", 
                                style = "color: #fff; background-color: #28a745; border-color: #28a745; width: 100%;",
                                class = "btn-lg")
                 )
          )
        )
      )
    } else if (!is.na(input_dir)){
      tryCatch({
        # Load the RData file into the container environment
        load(list.files(input_dir, pattern = ".*intersection\\.RData$", full.names = TRUE)[1], envir = loaded_env)
        
        # Copy required objects to the global environment
        list2env(as.list(loaded_env), .GlobalEnv)
        
        # Show success message
        showNotification("Data loaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading file:", e$message), type = "error")
      })
      
      # Check if required variables are loaded
      required_vars <- c("district_CropRotViz_intersection", "cropping_area", "Crop_choices", "Districts")
      
      missing_vars <- required_vars[!required_vars %in% ls(loaded_env)]
      
      if (length(missing_vars) > 0) {
        showNotification(
          paste("Missing required variables:", 
                paste(missing_vars, collapse = ", ")), 
          type = "error"
        )
      } else {
        data_loaded(TRUE)
      }
      # Show main app UI
      fast_viz_ui(input_dir)
    } else {
      fast_viz_ui()
    }
  })
  
  #--------------------------------------------------------------------------------------------
  # Handle file upload
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      # Load the RData file into the container environment
      load(input$file$datapath, envir = loaded_env)
      
      # Copy required objects to the global environment
      list2env(as.list(loaded_env), .GlobalEnv)
      
      # Show success message
      showNotification("Data loaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Display loaded variables
  output$loaded_vars <- renderPrint({
    if (!is.null(input$file)) {
      cat("Loaded variables:\n")
      ls(loaded_env)
    }
  })
  
  # Handle proceed button
  observeEvent(input$proceed, {
    req(input$file)
    
    # Check if required variables are loaded
    required_vars <- c("district_CropRotViz_intersection", "cropping_area", "Crop_choices", "Districts")
    
    missing_vars <- required_vars[!required_vars %in% ls(loaded_env)]
    
    if (length(missing_vars) > 0) {
      showNotification(
        paste("Missing required variables:", 
              paste(missing_vars, collapse = ", ")), 
        type = "error"
      )
    } else {
      data_loaded(TRUE)
    }
  })
  
  #--------------------------------------------------------------------------------------------
  # If data is loaded, run the main app server logic
  observe({
    req(data_loaded())
    
    # Reactive values for storing selected features
    firstSelection <- reactiveVal(NULL)
    secondSelection <- reactiveVal(NULL)
    
    # Control visibility of images
    output$hasFirstSelection <- reactive({
      !is.null(firstSelection())
    })
    output$hasSecondSelection <- reactive({
      !is.null(secondSelection())
    })
    output$hasSelection <- reactive({
      !is.null(firstSelection()) || !is.null(secondSelection())
    })
    
    outputOptions(output, "hasFirstSelection", suspendWhenHidden = FALSE)
    outputOptions(output, "hasSecondSelection", suspendWhenHidden = FALSE)
    outputOptions(output, "hasSelection", suspendWhenHidden = FALSE)
    
    # Load the appropriate spatial data based on selection
    spatial_data <- reactive({
      if(input$mapType == "districts") {
        Districts
      } else {
        EZGs[,1]
      }
    })
    
    # Create the map
    output$map <- renderLeaflet({
      data <- spatial_data()
      data <- st_transform(data, crs = "EPSG:4326")
      names(data) <- c("name", "geometry")
      
      # Get existing image files
      image_files <- gsub("\\.png$", "", list.files(paste0(input_dir, "/images"), pattern = "\\.png$"))
      
      # Case-insensitive matching
      data <- data[sapply(data$name, function(x) {
        # Normalize both name and image files
        normalized_x <- tolower(gsub("[/_ ]", "", x))
        normalized_image_files <- tolower(gsub("[/_ ]", "", image_files))
        
        # Check for exact matches after normalization
        any(normalized_x == normalized_image_files)
      }), ]
      
      data$id <- 1:nrow(data)
      
      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data = data,
          fillColor = ~ifelse(name == firstSelection(), "red",
                              ifelse(name == secondSelection(), "orange", "lightblue")),
          fillOpacity = 0.5,
          color = "black",
          weight = 2,
          layerId = ~id,
          label = ~name,
          labelOptions = labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "15px",
            direction = "auto"
          ),
          highlight = highlightOptions(
            weight = 3,
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        ) %>%
        setView(lng = mean(st_coordinates(st_centroid(data))[,1]),
                lat = mean(st_coordinates(st_centroid(data))[,2]),
                zoom = 7)
    })
    
    #--------------------------------------------------------------------------------------------
    # Handle clicks on the map
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      if (!is.null(click$id)) {
        data <- spatial_data()
        names(data) <- c("name", "geometry")
        
        image_files <- gsub("\\.png$", "", list.files(paste0(input_dir, "/images"), pattern = "\\.png$"))
        
        # Case-insensitive matching
        data <- data[sapply(data$name, function(x) {
          # Normalize both name and image files
          normalized_x <- tolower(gsub("[/_ ]", "", x))
          normalized_image_files <- tolower(gsub("[/_ ]", "", image_files))
          
          # Check for exact matches after normalization
          any(normalized_x == normalized_image_files)
        }), ]
        selected_name <- data$name[as.numeric(click$id)]
        
        # If clicking the same area that's already selected, deselect it
        if (!is.null(firstSelection()) && selected_name == firstSelection()) {
          firstSelection(NULL)
        } else if (!is.null(secondSelection()) && selected_name == secondSelection()) {
          secondSelection(NULL)
        } else {
          # If neither panel is selected, populate the first
          if (is.null(firstSelection())) {
            firstSelection(selected_name)
          }
          # If first panel is selected but second isn't, populate second
          else if (is.null(secondSelection())) {
            secondSelection(selected_name)
          }
          # If both panels are populated, replace the second
          else {
            secondSelection(selected_name)
          }
        }
      }
    })
    
    #--------------------------------------------------------------------------------------------
    # Render first image
    output$firstImage <- renderImage({
      req(firstSelection())
      print(firstSelection())
      sanitized_name <- gsub("/", "_", firstSelection())
      filename <- normalizePath(paste0(input_dir, '/images/', sanitized_name, '.png'))
      print(filename)
      list(
        src = filename,
        width = "100%",    # This makes it take full width of container
        contentType = "image/png",
        alt = paste("Data visualization for", firstSelection())
      )
    }, deleteFile = FALSE)
    
    # Render second image
    output$secondImage <- renderImage({
      req(secondSelection())
      sanitized_name <- gsub("/", "_", secondSelection())
      filename <- normalizePath(paste0(input_dir, '/images/', sanitized_name, '.png'))
      list(
        src = filename,
        width = "100%",    # This makes it take full width of container
        contentType = "image/png",
        alt = paste("Data visualization for", secondSelection())
      )
    }, deleteFile = FALSE)
    
    
    # Update plot title
    output$plotFirstTitle <- renderText({
      req(firstSelection())
      paste("Data visualization for", firstSelection())
    })
    
    output$plotSecondTitle <- renderText({
      req(secondSelection())
      paste("Data visualization for", secondSelection())
    })
    
    
    
    #--------------------------------------------------------------------------------------------
    observe({
      req(data_loaded())
      
      if (any(grepl("Aggregated_", names(district_CropRotViz_intersection[[1]])))) {
        
        output$sankey_plotly <- renderPlotly({
          Sys.sleep(1.5)
          
          # Calculate number of unique items (crops + groups)
          n_items <- length(sankey_data$node$label)
          
          # Calculate height based on number of items
          # Assuming we want roughly 100px per item, with some minimum height
          plot_height <- max(800, n_items * 15)  
          
          plotly <- plot_ly(
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
          
          # Display the Sankey chart
          plotly %>%
            config(
              toImageButtonOptions = list(
                format = "png",
                filename = "reorganization",
                width = 1000,
                height = 1450
              )
            )
        })
      }else{
        output$sankey_plotly <- renderPlotly({
          plot_ly(
            x = c(0.5),  # Add a dummy point to create a valid trace
            y = c(0.5),
            type = "scatter",
            mode = "markers",
            marker = list(opacity = 0)  # Make the marker invisible
          ) %>%
            layout(
              xaxis = list(
                showline = FALSE,
                showgrid = FALSE,
                showticklabels = FALSE,
                zeroline = FALSE,
                range = c(0, 1)
              ),
              yaxis = list(
                showline = FALSE,
                showgrid = FALSE,
                showticklabels = FALSE,
                zeroline = FALSE,
                range = c(0, 1)
              ),
              annotations = list(
                x = 0.5,
                y = 0.5,
                text = "No Crop Class Aggregated Made",
                xref = "paper",
                yref = "paper",
                showarrow = FALSE,
                font = list(
                  size = 20,
                  color = "White"
                )
              ),
              plot_bgcolor = "rgba(0,0,0,0)",
              paper_bgcolor = "rgba(0,0,0,0)"
            ) %>%
            config(
              displayModeBar = FALSE  # Hide Plotly mode bar
            )
        })
      }
    })
    
    
    
    # plot diversity maps
    output$diversity <- renderLeaflet(
      if(input$AreaType == "districts"){
        diversity_mapper(data = diversity_data[[1]], type = "District")
      }else{
        diversity_mapper(data = diversity_data[[2]], type = "EZG")
      }
    )
  })
}