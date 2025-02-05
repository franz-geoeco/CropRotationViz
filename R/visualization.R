#' Crop Rotation Visualization UI
#' 
#' @description Creates an interactive visualization interface for crop rotation patterns,
#' featuring multiple visualization types including Sankey diagrams, tables, and plots.
#' The UI is split into two main tabs: general sequence plotting and crop-specific analysis.
#'
#' @details The UI includes several components:
#'   \itemize{
#'     \item Plot Sequence Tab:
#'       \itemize{
#'         \item Area range selector (0-4000 km²)
#'         \item Crop selection interface with multi-select capability
#'         \item Percentage area visualization
#'         \item Sankey diagram of crop rotations
#'         \item Interactive data table with color-coded rotations
#'         \item Crop class merging visualization
#'       }
#'     \item Plot Crop Specific Sequence Tab:
#'       \itemize{
#'         \item Specific crop selector
#'         \item Area range configuration
#'         \item Crop-specific Sankey diagram
#'         \item Detailed rotation patterns table
#'       }
#'   }
#' 
#' @section Theme:
#' Uses the "cyborg" theme with custom CSS modifications for:
#'   \itemize{
#'     \item DataTables styling
#'     \item Tab navigation appearance
#'     \item Popup content formatting
#'   }
#'
#' @note This UI requires the following global variables to be present:
#'   \itemize{
#'     \item choices - Vector of crop choices
#'     \item app_data - List containing Input_App_data
#'   }
#' 
#' @return A Shiny UI object containing the complete visualization interface
#' 
#' @import shiny shinythemes shinyBS shinyWidgets shinycssloaders DT plotly ggplot2 dplyr sf
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column
#'   plotOutput uiOutput renderUI verbatimTextOutput fileInput
#'   actionButton wellPanel
#' @importFrom shinythemes shinytheme
#' @importFrom shinyBS bsButton bsPopover
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @importFrom shinycssloaders withSpinner
#' @importFrom htmltools tags HTML
#' @importFrom tidyr pivot_longer
#' @importFrom plyr count
#' @importFrom purrr reduce
#' @importFrom stringr str_remove
#' @importFrom forcats fct_reorder
#' @importFrom shinyalert shinyalert
#' @import ggalluvial
#' @examples
#' \dontrun{
#' # Run the app with this UI
#' shinyApp(ui = viz_ui, server = server)
#' }
#' 
#' @export
viz_ui <- function(input_dir = NA){
  fluidPage(
    Crop_choices <- "",
    District_choices <- "",
    EZG_choices <- "",
    theme = shinytheme("cyborg"),
    
    tags$style(HTML("
      .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
        color: white !important;
        border-color: white !important; /* Border color of the show entries selector */
      }
      .dataTables_wrapper .dataTables_filter input {
        color: white !important;
      }
      .tabs-above > .nav > li[class=active] > a {
             background-color: #164f8c;
             color: #FFF;
      }
      .leaflet-popup-content-wrapper {
      background-color: #ffffff; 
      color: #4571c4; /* Text color for the popup */
      }
    ")),
    
    # Define the tabs
    tabsetPanel(id = "tabs",
                tabPanel("Plot Sequence",
                         br(),
                         fluidRow(
                           column(style = "margin-top: -30px; margin-bottom: 0px;", width = 4, h2("Crop Sequences")), 
                           column(5),
                           column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
                           ),
                           br(),
                         fluidRow(
                           column(2,
                                         numericRangeInput("Area_range_sec", "Sequence Area Range (km²):",
                                                           min = 0, max = 4000,
                                                           value = c(0, 1000))
                           ),
                           column(1, bsButton("range-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                           column(2, 
                                  pickerInput(
                                    inputId = "Crops_sec", 
                                    label = "Show/hide crops:", 
                                    choices = Crop_choices, 
                                    selected = Crop_choices, 
                                    options = pickerOptions(
                                      actionsBox = TRUE, 
                                      size = 15,
                                      selectedTextFormat = "count > 3"
                                    ), multiple = TRUE)
                           ),
                           column(1, bsButton("crops-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                           column(4, plotOutput("perc_plot", height = 80))
                         ),
                         bsPopover(
                           id = "range-info",
                           title = "Rotation Size",
                           content = HTML(paste0(
                             "Here you can select the size of a full rotation to be displayed. Therfore a selection from 100-200 means that a full 7 year rotation has to be between 100 and 200 km²."
                           )),
                           placement = "right",
                           trigger = "hover",
                           options = list(container = "body")
                         ),
                         bsPopover(
                           id = "crops-info",
                           title = "Crops Selection",
                           content = HTML(paste0(
                             "Here you can include or exclude crops from charting if you want to highlight specific crops or disable to dominant wants."
                           )),
                           placement = "right",
                           trigger = "hover",
                           options = list(container = "body")
                         ),
                         fluidRow(
                           column(1),
                           column(8,
                                  h4("Rotation in whole Region"))
                         ),
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    plotOutput("sankey_plot")
                                  )
                           ),
                           column(1,
                                  downloadButton("Save_sankey_plot", label = "Save Plot as PNG"
                                                 )
                                  )
                         ),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                         
                         # Horizontal line
                         tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                         fluidRow(
                           column(1),
                           column(3,
                                  h4("Unique Rotations and Size")),
                           column(1, bsButton("table-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small"))
                         ),br(),
                         bsPopover(
                           id = "table-info",
                           title = "Table with the Largest Crop Rotation Patterns",
                           content = HTML(paste0(
                             "Here you can see the largest crop rotation pattern in MWP in a 7 year rotation (or selected range). You can search, sort or expand the table as you like."
                           )),
                           placement = "left",
                           trigger = "hover",
                           options = list(container = "body")
                         ),
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    DTOutput('table', width = "100%", height = "auto", fill = TRUE)
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
                tabPanel("Plot Crop Specific Sequence",
                         br(),
                         fluidRow(
                           column(width = 4, h2("Crop Sequences")), 
                           column(5),
                           column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
                         ),
                         br(),
                         fluidRow(column(2,
                                         numericRangeInput("Area_range_spec", "Considered Rotation Area Range (km²):",
                                                           min = 0, max = 4000,
                                                           value = c(0, 1000))
                         ),
                         column(1, bsButton("range_spec-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                         column(2, 
                                pickerInput(
                                  inputId = "Crop_spec", 
                                  label = "Specific crop:", 
                                  choices = Crop_choices, 
                                  selected = "protein plants", 
                                  options = pickerOptions(
                                    actionsBox = TRUE, 
                                    size = 15,
                                    selectedTextFormat = "count > 3"
                                  ), multiple = F)
                         ),
                         column(1, bsButton("crops_spec-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small")),
                         column(4, plotOutput("perc_spec_plot", height = 80))
                         ),
                         bsPopover(
                           id = "range_spec-info",
                           title = "Rotation Size",
                           content = HTML(paste0(
                             "Here you can select the size of a full sequence to be displayed. Therfore a selection from 100-200 means that a full 7 year sequence has to be between 100 and 200 km²."
                           )),
                           placement = "right",
                           trigger = "hover",
                           options = list(container = "body")
                         ),
                         bsPopover(
                           id = "crops_spec-info",
                           title = "Crop Selection",
                           content = HTML(paste0(
                             "Here you can choose a crop for charting. The crop has to be in one of the seven years (in the sequence)"
                           )),
                           placement = "right",
                           trigger = "hover",
                           options = list(container = "body")
                         ),
                         fluidRow(
                           column(1),
                           column(8,
                                  h4("Crop Sequence in Entire Region"))
                         ),
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    plotOutput("sankey_specific")
                                  )
                           ),
                           column(1,
                                  downloadButton("Save_sankey_specific_plot", label = "Save Plot as PNG"
                                  )
                           )
                         ),
                         br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                         # Horizontal line
                         tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                         fluidRow(
                           column(1),
                           column(2,
                                  h4("Unique Rotations and Size")),
                           column(1, bsButton("table_spec-info", label = "", icon = icon("info", lib = "font-awesome"), style = "default", size = "extra-small"))
                           
                         ),
                         br(),
                         bsPopover(
                           id = "table_spec-info",
                           title = "Table with the Largest Crop Rotation Patterns",
                           content = HTML(paste0(
                             "Here you can see the largest crop rotation pattern in MWP in a 7 year rotation (or selected range). You can search, sort or expand the table as you like."
                           )),
                           placement = "left",
                           trigger = "hover",
                           options = list(container = "body")
                         ),
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    DTOutput('table_spec', width = "100%", height = "auto", fill = TRUE)
                                  )
                           )
                         ),
                         tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                         fluidRow(
                           column(1),
                           column(2,
                                  h4("Proportion Before and After your selected Crop"))
                         ),
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    plotlyOutput("sankey_plotly_specific")
                                  )
                                  ),
                           )
                         
                ),
                tabPanel("Plot Sequence per Area",
                         br(),
                         fluidRow(
                           column(width = 4, h2("Crop Sequences Per Area")), 
                           column(5),
                           column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
                         ),
                         fluidRow(
                           if(sum(is.na(EZGs))==0){
                             column(2,
                                    radioButtons(inputId = "district_or_ezg",
                                                 label = "Select",
                                                 selected = "District",
                                                 choices = c("District", "River Basin"))
                                    )
                           },
                           conditionalPanel(
                             condition = "input.district_or_ezg != 'River Basin'",
                             column(2,
                                    pickerInput(
                                      inputId = "District_sel", 
                                      label = "District you wanna display", 
                                      choices = District_choices, 
                                      selected = District_choices[1],
                                      options = pickerOptions(
                                        actionsBox = TRUE, 
                                        size = 15,
                                        selectedTextFormat = "count > 3"
                                      ), multiple = F)
                           )
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             column(2, 
                                    pickerInput(
                                      inputId = "EZG_sel", 
                                      label = "RIver basin you wanna display", 
                                      choices = EZG_choices, 
                                      selected = EZG_choices[1], 
                                      options = pickerOptions(
                                        actionsBox = TRUE, 
                                        size = 15,
                                        selectedTextFormat = "count > 3"
                                      ), multiple = F)
                             )
                           ),
                           column(2,
                                  numericRangeInput("Area_range_areas", "Sequence Area Range (km²):",
                                                    min = 0, max = 4000,
                                                    value = c(0, 1000))
                           ),
                           column(2, 
                                  pickerInput(
                                    inputId = "Crops_kreis", 
                                    label = "Show/hide crops:", 
                                    choices = Crop_choices, 
                                    selected = Crop_choices, 
                                    options = pickerOptions(
                                      actionsBox = TRUE, 
                                      size = 15,
                                      selectedTextFormat = "count > 3"
                                    ), multiple = TRUE)
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg != 'River Basin'",
                             column(4, plotOutput("perc_district_plot", height = 80))
                             ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             column(4, plotOutput("perc_basin_plot", height = 80))
                           )
                         ),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.district_or_ezg != 'River Basin'",
                              column(2,
                                      shinycssloaders::withSpinner(
                                        leafletOutput("leaflet_district")
                                      )
                              )
                             ),
                           conditionalPanel(
                             condition = "input.district_or_ezg != 'River Basin'",
                              column(10,
                                      shinycssloaders::withSpinner(
                                        plotOutput("sankey_district")
                                      )
                              )
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             column(2,
                                    shinycssloaders::withSpinner(
                                      leafletOutput("leaflet_basin")
                                    )
                             )
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             column(10,
                                    shinycssloaders::withSpinner(
                                      plotOutput("sankey_basin")
                                    )
                             )
                           )
                         ),
                         fluidRow(
                           br(),br(),
                           conditionalPanel(
                             condition = "input.district_or_ezg != 'River Basin'",
                             column(1,
                                    downloadButton("Save_sankey_district_plot", label = "Save Plot as PNG"
                                    )
                             )
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             column(1,
                                    downloadButton("Save_sankey_basin_plot", label = "Save Plot as PNG"
                                    )
                             )
                           )
                         ),
                         br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                         # Horizontal line
                         tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                         fluidRow(
                           column(3,
                                  sliderInput(
                                   "year_select",
                                   "Select Year",
                                   min = 2000,
                                   max = 2024,
                                   value = 2008,
                                   step = 1,
                                   sep = "",
                                   ticks = T
                                 ),
                                 conditionalPanel(
                                   condition = "input.district_or_ezg != 'River Basin'",
                                   shinycssloaders::withSpinner(
                                     plotlyOutput("district_donut_plot")  # Adjust height as needed
                                   )
                                 ),
                                 conditionalPanel(
                                   condition = "input.district_or_ezg == 'River Basin'",
                                   shinycssloaders::withSpinner(
                                     plotlyOutput("basin_donut_plot")  # Adjust height as needed
                                   )
                                 )
                                 ),
                           br(), br(), br(), br(), br(),
                           conditionalPanel(
                             condition = "input.district_or_ezg != 'River Basin'",
                             column(9,
                                    shinycssloaders::withSpinner(
                                      plotOutput("district_ridges")
                                    )
                             ), br()
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             column(9,
                                    shinycssloaders::withSpinner(
                                      plotOutput("basin_ridges")
                                    )
                             ), br()
                           )
                         )
                ),
                div(style = "padding-bottom: 100px;"), # Add padding for footer
                
                
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
ui <- function(app_data) {
  # Load package data
  utils::data("Input_App_data", envir = environment())
  
  fluidPage(
    theme = shinytheme("cyborg"),
    
    # Conditional UI - show either loader or main app
    uiOutput("dynamic_ui")
  )
}

#' Crop Rotation Visualization Server Logic
#' 
#' @description Implements comprehensive server-side logic for the crop rotation visualization
#' application. Handles data loading, transformations, and creates reactive visualizations
#' including Sankey diagrams, tables, and summary plots.
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param app_data List containing application data including crop mappings and settings
#'
#' @section Data Loading:
#'   Handles the loading and validation of RData files containing:
#'   \itemize{
#'     \item CropRotViz_intersection - Main rotation data
#'     \item cropping_area - Area calculations
#'     \item Crop_choices - Available crop choices
#'     \item District_choices - Available district choices
#'     \item EZG_choices - Available river basin choices
#'   }
#'
#' @section Visualization Components:
#'   Creates and manages multiple visualization types:
#'   \itemize{
#'     \item Percentage Plots:
#'       \itemize{
#'         \item Area distribution visualization
#'         \item Dynamic updates based on selection
#'       }
#'     \item Sankey Diagrams:
#'       \itemize{
#'         \item Crop rotation flow visualization
#'         \item Crop-specific pattern analysis
#'       }
#'     \item Data Tables:
#'       \itemize{
#'         \item Color-coded rotation summaries
#'         \item Interactive filtering and sorting
#'       }
#'     \item Plotly Integration:
#'       \itemize{
#'         \item Interactive Sankey diagrams
#'         \item Custom styling and layout
#'       }
#'   }
#'
#' @section Reactive Flow:
#'   \itemize{
#'     \item Data Loading Stage:
#'       \itemize{
#'         \item File validation
#'         \item Environment setup
#'         \item Variable checking
#'       }
#'     \item Data Processing Stage:
#'       \itemize{
#'         \item Rotation data transformation
#'         \item Area calculations
#'         \item Color mapping
#'       }
#'     \item Visualization Stage:
#'       \itemize{
#'         \item Plot generation
#'         \item Table formatting
#'         \item Interactive updates
#'       }
#'   }
#'
#' @return A Shiny server function that manages the complete visualization workflow
#' 
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column
#'   uiOutput renderUI verbatimTextOutput fileInput
#'   actionButton wellPanel
#' @importFrom ggridges geom_density_ridges theme_ridges
#' @import data.table 
#' @import ggalluvial
#' @examples
#' \dontrun{
#' # Run the complete Shiny application
#' shinyApp(ui = ui, server = viz_server)
#' }
#' 
#' @export
viz_server <- function(input, output, session, app_data, input_dir) {
  options(warn = -1) 
  
  all_wrap_number_count_small <- app_data$Input_App_data$all_wrap_number_count_small
  crop_color_mapping <- app_data$Input_App_data$crop_color_mapping
  loaded_env <- app_data$Input_App_data$loaded_env
  
  options(shiny.maxRequestSize=1024^3 )
  # Reactive value to track if data is loaded
  data_loaded <- reactiveVal(FALSE)
  
  # Render dynamic UI based on whether data is loaded
  output$dynamic_ui <- renderUI({
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
      viz_ui(input_dir)
    } else {
      viz_ui()
    }
  })
  
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
  
  # If data is loaded, run the main app server logic
  observe({
    req(data_loaded())
    
    CropRotViz_intersection <- data.table::rbindlist(district_CropRotViz_intersection)
    
    # Then add the id column
    CropRotViz_intersection[, id := seq_len(.N)]
    
    # Initialize choices after loading data
    # Get crop choices - more efficiently
    Crop_choices <- tryCatch({
      # First check for Aggregated_ columns
      agg_cols <- grep("^Aggregated_", names(CropRotViz_intersection), value = TRUE)
      if(length(agg_cols) > 0) {
        geom_dropped <- sf::st_drop_geometry(CropRotViz_intersection[, ..agg_cols])
        unique(unlist(geom_dropped, use.names = FALSE))
      } else {
        # Fall back to Name_ columns
        crop_cols <- grep("^Name_", names(CropRotViz_intersection), value = TRUE)
        if(length(crop_cols) > 0) {
          geom_dropped <- sf::st_drop_geometry(CropRotViz_intersection[, ..crop_cols])
          unique(unlist(geom_dropped, use.names = FALSE))
        } else {
          warning("No Aggregated_ or Name_ columns found")
          NULL
        }
      }
    }, error = function(e) {
      warning("Error extracting crop choices: ", e$message)
      NULL
    })
    District_choices <- names(district_CropRotViz_intersection)
    EZG_choices      <- names(EZG_CropRotViz_intersection)
    
    #--------------------------------------------------------------------------------------------
    
    # Update the pickerInput Crops_sec
    updatePickerInput(
      session,
      inputId = "Crops_sec",
      choices = setNames(as.character(Crop_choices), Crop_choices),
      selected = Crop_choices
    )
    
    # Update the pickerInput Crop_spec
    updatePickerInput(
      session,
      inputId = "Crop_spec",
      choices = setNames(as.character(Crop_choices), Crop_choices),
      selected = Crop_choices[7]
    )
    
    # Update the pickerInput Crops_kreis
    updatePickerInput(
      session,
      inputId = "Crops_kreis",
      choices = setNames(as.character(Crop_choices), Crop_choices),
      selected = Crop_choices
    )
    
    # Update the pickerInput District_sel
    updatePickerInput(
      session,
      inputId = "District_sel",
      choices = setNames(as.character(District_choices), District_choices),
      selected = District_choices[1]
    )
    
    # Update the pickerInput EZG_sel
    updatePickerInput(
      session,
      inputId = "EZG_sel",
      choices = setNames(as.character(EZG_choices), EZG_choices),
      selected = EZG_choices[1]
    )
    
    # Update the sliderInput year_select
    updateSliderInput(
      session,
      inputId = "year_select",
      min = min(years),
      max = max(years)
    )
    #--------------------------------------------------------------------------------------------
    
    
    # ALERTS
    observeEvent(input$tabs, {
      if(input$tabs == "Plot Sequence"){
        text_1 <- "Welcome"
        text_2 <- "This is an application for analyzing crop rotation pattern in the Entire Region\nThis first page is particular for the crop rotation analyses."
      } else if(input$tabs == "Plot Crop Specific Sequence"){
        text_1 <- "Plot Crop Specific Sequence"
        text_2 <- "Here you can create crop specific rotation pattern plots."
      }else if(input$tabs== "Plot Sequence per Area"){
        text_1 <- "Plot Sequence per Area"
        text_2 <- "Here you can create district specific rotation pattern plots."
      }
      shinyalert(text_1, text_2, type = "info", confirmButtonCol = "#5d9bd9")
    })
    
    #--------------------------------------------------------------------------------------------
    
    # rotation_data
    rotation_data <- reactive({
      req(input$Area_range_sec, input$Crops_sec)
      transform_rotation_data(
        All_rot_big = CropRotViz_intersection,
        distribution_df  = distribution_df,
        input_area_range = input$Area_range_sec,
        choices = Crop_choices,
        selected_crops = input$Crops_sec,
        type = "basic"
      )
    })
    
    
    crop_colors <- reactive({
      req(rotation_data())
      
      # Get unique keys
      unique_keys <- unique(rotation_data()[[1]]$key)
      
      # Set seed for reproducibility
      set.seed(123)
      
      # Color generation function
      generate_random_color <- function() {
        rgb(runif(1), runif(1), runif(1))
      }
      
      # Initialize color mapping
      if (is.null(crop_color_mapping)) {
        # No colors provided, generate for all unique keys
        color_map <- setNames(
          sapply(1:length(unique_keys), function(x) generate_random_color()), 
          unique_keys
        )
      } else {
        # Start with existing color mapping
        color_map <- crop_color_mapping
        
        # Identify missing keys
        missing_keys <- setdiff(unique_keys, names(color_map))
        
        # Generate colors for missing keys
        if (length(missing_keys) > 0) {
          additional_colors <- setNames(
            sapply(1:length(missing_keys), function(x) generate_random_color()), 
            missing_keys
          )
          color_map <- c(color_map, additional_colors)
        }
        
        # Ensure color map matches unique keys exactly
        color_map <- color_map[as.character(unique_keys)]
      }
      
      return(color_map)
    })
    
    # spec_rotation_data
    spec_rotation_data <- reactive({
      # Add invalidation trigger for Area_range_sec and Crops_sec
      req(input$Area_range_spec, input$Crop_spec)
      transform_rotation_data(
        All_rot_big      = CropRotViz_intersection,
        distribution_df  = distribution_df,
        input_area_range = input$Area_range_spec,
        type             = "specific",
        specific_crop    = input$Crop_spec
      )
    })
    
    
    # district_rotation_data
    district_rotation_data <- reactive({
      # Add invalidation trigger for Area_range_areas and Crops_kreis
      req(input$Area_range_areas, input$Crop_spec)
      transform_rotation_data(
        All_rot_big      = district_CropRotViz_intersection,
        distribution_df  = distribution_df,
        choices          = Crop_choices,
        selected_crops   = input$Crops_kreis,
        input_area_range = input$Area_range_areas,
        type             = "district",
        district         = input$District_sel
      )
    })
    
    # basin_rotation_data
    basin_rotation_data <- reactive({
      # Add invalidation trigger for Area_range_areas and Crops_kreis
      req(input$Area_range_areas, input$Crop_spec)
      transform_rotation_data(
        All_rot_big      = EZG_CropRotViz_intersection,
        distribution_df  = distribution_df,
        choices          = Crop_choices,
        selected_crops   = input$Crops_kreis,
        input_area_range = input$Area_range_areas,
        type             = "basin",
        EZG              = input$EZG_sel
      )
    })
    
    #--------------------------------------------------------------------------------------------
    
    # Create reactive expression for the plot
    create_perc_plot <- reactive({
      req(rotation_data())
      # Create a data frame for the plot
      plot_data <- data.frame(
        Category   = c("Selected Cropping Area", "Remaining Cropping Area"),
        Percentage = c((sum(rotation_data()[[1]]$Area)/length(years) / (cropping_area)) * 100,
                       100-(sum(rotation_data()[[1]]$Area)/length(years) / (cropping_area)) * 100)
      )
      
      ggplot(plot_data, aes(x = 0.1, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", width = 10) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_stack(vjust = 0.5), # Centers the text within the bars
                  color = "white", fontface = "bold",
                  size = 5) + # Adjust size as needed
        coord_flip() +
        scale_fill_manual(values = c("Selected Cropping Area" = "darkgreen", "Remaining Cropping Area" = "darkred")) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme_minimal() +
        theme(
          axis.text.y  = element_blank(),
          axis.text.x  = element_text(color = "white", size = 15,face="bold"),
          legend.title = element_blank(),
          legend.text  = element_text(color = "white", size = 15, face="bold"),
          axis.ticks.y = element_blank(),
          panel.grid   = element_blank(),
          panel.background  = element_rect(fill = "transparent", color = NA),
          plot.background   = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA)
        ) +
        scale_y_continuous(labels = function(x) paste0(x, "%"))
    })
    
    # Render the plot
    output$perc_plot <- renderPlot({
      Sys.sleep(1.5)
      create_perc_plot()
    }, height = 65, bg = "transparent")
    
    #--------------------------------------------------------------------------------------------
    
    # Create reactive expression for the plot
    create_spec_perc_plot <- reactive({
      req(spec_rotation_data())
      # Create a data frame for the plot
      plot_data <- data.frame(
        Category = c("Selected Cropping Area", "Remaining Cropping Area"),
        Percentage = c((sum(spec_rotation_data()[[1]]$Area)/length(years) / (cropping_area)) * 100,
                       100-(sum(spec_rotation_data()[[1]]$Area)/length(years) / (cropping_area)) * 100)
      )
      
      ggplot(plot_data, aes(x = 0.1, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", width = 10) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_stack(vjust = 0.5), # Centers the text within the bars
                  color = "white", fontface = "bold",
                  size = 5) + # Adjust size as needed
        coord_flip() +
        scale_fill_manual(values = c("Selected Cropping Area" = "darkgreen", "Remaining Cropping Area" = "darkred")) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "white", size = 15, face="bold"),
          legend.title = element_blank(),
          legend.text = element_text(color = "white", size = 15, face="bold"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA)
        ) +
        scale_y_continuous(labels = function(x) paste0(x, "%"))
    })
    
    output$perc_spec_plot <- renderPlot({
      Sys.sleep(1.5)
      create_spec_perc_plot()
    }, height = 65, bg = "transparent")
    

    #--------------------------------------------------------------------------------------------
    
    # Create reactive expression for the plot
    create_district_perc_plot <- reactive({
      req(district_rotation_data())
      cropping_area <- sum(district_CropRotViz_intersection[[input$District_sel]]$freq)

      # Create a data frame for the plot
      plot_data <- data.frame(
        Category = c("Selected Cropping Area", "Remaining Cropping Area"),
        Percentage = c((sum(district_rotation_data()[[1]]$Area)/length(years) / (cropping_area)) * 100,
                       100-(sum(district_rotation_data()[[1]]$Area)/length(years) / (cropping_area)) * 100)
      )
      
      ggplot(plot_data, aes(x = 0.1, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", width = 10) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_stack(vjust = 0.5), # Centers the text within the bars
                  color = "white", fontface = "bold",
                  size = 5) + # Adjust size as needed
        coord_flip() +
        scale_fill_manual(values = c("Selected Cropping Area" = "darkgreen", "Remaining Cropping Area" = "darkred")) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "white", size = 15, face="bold"),
          legend.title = element_blank(),
          legend.text = element_text(color = "white", size = 15, face="bold"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA)
        ) +
        scale_y_continuous(labels = function(x) paste0(x, "%"))
    })
    
    output$perc_district_plot <- renderPlot({
      Sys.sleep(1.5)
      create_district_perc_plot()
    }, height = 65, bg = "transparent")
    
    
    #--------------------------------------------------------------------------------------------
    
    # Create reactive expression for the plot
    create_basin_perc_plot <- reactive({
      req(basin_rotation_data())
      cropping_area <- sum(EZG_CropRotViz_intersection[[input$EZG_sel]]$freq)

      # Create a data frame for the plot
      plot_data <- data.frame(
        Category = c("Selected Cropping Area", "Remaining Cropping Area"),
        Percentage = c((sum(basin_rotation_data()[[1]]$Area)/length(years) / (cropping_area)) * 100,
                       100-(sum(basin_rotation_data()[[1]]$Area)/length(years) / (cropping_area)) * 100)
      )
      
      ggplot(plot_data, aes(x = 0.1, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", width = 10) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_stack(vjust = 0.5), # Centers the text within the bars
                  color = "white", fontface = "bold",
                  size = 5) + # Adjust size as needed
        coord_flip() +
        scale_fill_manual(values = c("Selected Cropping Area" = "darkgreen", "Remaining Cropping Area" = "darkred")) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "white", size = 15, face="bold"),
          legend.title = element_blank(),
          legend.text = element_text(color = "white", size = 15, face="bold"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA)
        ) +
        scale_y_continuous(labels = function(x) paste0(x, "%"))
    })
    
    output$perc_basin_plot <- renderPlot({
      Sys.sleep(1.5)
      create_basin_perc_plot()
    }, height = 65, bg = "transparent")
    
    #--------------------------------------------------------------------------------------------
    
    # Create reactive expression for the plot
    create_sankey_plot <- reactive({
      req(rotation_data())
      req(crop_colors())
      
      base_plot <- ggplot(
        rotation_data()[[1]], 
        aes(
          x = year, y = Area, stratum = key, 
          fill = key, alluvium = id, label = key
        )
      ) +
        geom_stratum(alpha = .90, show.legend = TRUE, color = NA) +
        theme_linedraw() +
        theme(
          axis.text = element_text(color = "white", size = 15, face="bold"),
          axis.title.y = element_text(color = "white", size = 16, face="bold", margin = margin(r = 10)),
          legend.background = element_rect(fill = "lightgrey", color = "black"),
          panel.background = element_rect(fill = 'lightgrey', color = 'black'),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          plot.background = element_rect(fill = "#1f1b1b", color = NA),
          legend.box.background = element_rect(fill = "#1f1b1b", color = NA)
        ) +
        geom_text(stat = "stratum", size = 4, check_overlap = TRUE) +
        scale_fill_manual(name = "Crops", values = crop_colors()) +
        ylab("Area [km²]") + 
        xlab("") + 
        scale_x_continuous(breaks = unique(rotation_data()[[1]]$year))+
        geom_flow(show.legend = FALSE)
      
      # Add conditional legend rows
      if (input$Area_range_sec[1] < 1) {
        base_plot <- base_plot + guides(fill = guide_legend(nrow = 8))
      } else {
        base_plot <- base_plot + guides(fill = guide_legend(nrow = 5))
      }
      
      base_plot
    })
    
    # Render the plot
    output$sankey_plot <- renderPlot({
      Sys.sleep(1.5)
      create_sankey_plot()
    }, height = 800, bg = "transparent")
    
    
    output$Save_sankey_plot <- downloadHandler(
      filename = function(file) {
        "sankey_box_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = create_sankey_plot(), width = 590, height = 365, units = "mm", device = "png")
      }
    )
    
    #--------------------------------------------------------------------------------------------
    
    create_specific_sankey_plot <- reactive({
      req(spec_rotation_data())
      req(crop_colors())
      
      # make basic transition graph
      specific <- ggplot(spec_rotation_data()[[1]], aes(x = year, y = Area,
                                                   stratum = key, fill = key,
                                                   alluvium = id, label = key)) +
        geom_stratum(alpha = .90 , show.legend = T, color=NA) +
        theme_linedraw()+
        theme(axis.text = element_text(color = "white", size = 15, face="bold"),
              axis.title.y = element_text(color = "white", size = 16, face="bold", margin = margin(r = 10)),
              legend.background = element_rect(fill = "lightgrey", color = "black"),
              panel.background = element_rect(fill = 'lightgrey', color = 'black'),
              legend.direction = "horizontal",
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.title = element_blank(),
              legend.text = element_text(size = 15),
              plot.background = element_rect(fill = "#1f1b1b", color = NA),
              legend.box.background = element_rect(fill = "#1f1b1b", color = NA))+
        geom_text(stat = "stratum", size = 4, check_overlap=T) +
        scale_fill_manual(name = "Crops", values = crop_colors()) +
        ylab("Area [km²]")+  xlab("")+ 
        scale_x_continuous(breaks = unique(spec_rotation_data()[[1]]$year))+
        geom_flow(show.legend = F)
      
      unique_key_count <- length(unique(spec_rotation_data()[[1]]$key))
      
      # Apply guides based on the unique count
      if (length(unique_key_count) > 20) {
        specific + guides(fill = guide_legend(nrow = 8))
      } else {
        specific + guides(fill = guide_legend(nrow = 2))
      }
      specific
      
    })
    
    # Render the specific sankey plot
    output$sankey_specific <- renderPlot({
      Sys.sleep(1.5)
      create_specific_sankey_plot()
    }, height = 800, bg = "transparent")
    
    output$Save_sankey_specific_plot <- downloadHandler(
      filename = function(file) {
        "sankey_box_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = create_specific_sankey_plot(), width = 590, height = 365, units = "mm", device = "png")
      }
    )
    
    #--------------------------------------------------------------------------------------------
    
    create_district_sankey_plot <- reactive({
      req(district_rotation_data())
      req(crop_colors())

      # make basic transition graph
      specific <- ggplot(district_rotation_data()[[1]], aes(x = year, y = Area,
                                                            stratum = key, fill = key,
                                                   alluvium = id, label = key)) +
        geom_stratum(alpha = .90 , show.legend = T, color=NA) +
        theme_linedraw()+
        theme(axis.text = element_text(color = "white", size = 15, face="bold"),
              axis.title.y = element_text(color = "white", size = 16, face="bold", margin = margin(r = 10)),
              legend.background = element_rect(fill = "lightgrey", color = "black"),
              panel.background = element_rect(fill = 'lightgrey', color = 'black'),
              legend.direction = "horizontal",
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.title = element_blank(),
              legend.text = element_text(size = 15),
              plot.background = element_rect(fill = "#1f1b1b", color = NA),
              legend.box.background = element_rect(fill = "#1f1b1b", color = NA))+
        geom_text(stat = "stratum", size = 4, check_overlap=T) +
        scale_fill_manual(name = "Crops", values = crop_colors()) +
        ylab("Area [km²]")+  xlab("")+ 
        scale_x_continuous(breaks = unique(district_rotation_data()[[1]]$year))+
        geom_flow(show.legend = F)
      
      unique_key_count <- length(unique(district_rotation_data()[[1]]$key))
      
      # Apply guides based on the unique count
      if (length(unique_key_count) > 20) {
        specific + guides(fill = guide_legend(nrow = 8))
      } else {
        specific + guides(fill = guide_legend(nrow = 2))
      }
      specific
      
    })
    
    # Render the specific sankey plot
    output$sankey_district <- renderPlot({
      Sys.sleep(1.5)
      create_district_sankey_plot()
    }, height = 800, bg = "transparent")
    
    output$Save_sankey_district_plot <- downloadHandler(
      filename = function(file) {
        "sankey_box_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = create_district_sankey_plot(), width = 590, height = 365, units = "mm", device = "png")
      }
    )
    #--------------------------------------------------------------------------------------------
    
    create_basin_sankey_plot <- reactive({
      req(basin_rotation_data())
      req(crop_colors())

      # make basic transition graph
      specific <- ggplot(basin_rotation_data()[[1]], aes(x = year, y = Area,
                                                       stratum = key, fill = key,
                                                       alluvium = id, label = key)) +
        geom_stratum(alpha = .90 , show.legend = T, color=NA) +
        theme_linedraw()+
        theme(axis.text = element_text(color = "white", size = 15, face="bold"),
              axis.title.y = element_text(color = "white", size = 16, face="bold", margin = margin(r = 10)),
              legend.background = element_rect(fill = "lightgrey", color = "black"),
              panel.background = element_rect(fill = 'lightgrey', color = 'black'),
              legend.direction = "horizontal",
              legend.position = "bottom",
              legend.box = "horizontal",
              legend.title = element_blank(),
              legend.text = element_text(size = 15),
              plot.background = element_rect(fill = "#1f1b1b", color = NA),
              legend.box.background = element_rect(fill = "#1f1b1b", color = NA))+
        geom_text(stat = "stratum", size = 4, check_overlap=T) +
        scale_fill_manual(name = "Crops", values = crop_colors()) +
        ylab("Area [km²]")+  xlab("")+ 
        scale_x_continuous(breaks = unique(basin_rotation_data()[[1]]$year))+
        geom_flow(show.legend = F)
      
      unique_key_count <- length(unique(basin_rotation_data()[[1]]$key))
      
      # Apply guides based on the unique count
      if (length(unique_key_count) > 20) {
        specific + guides(fill = guide_legend(nrow = 8))
      } else {
        specific + guides(fill = guide_legend(nrow = 2))
      }
      specific
      
    })
    
    # Render the specific sankey plot
    output$sankey_basin <- renderPlot({
      Sys.sleep(1.5)
      create_basin_sankey_plot()
    }, height = 800, bg = "transparent")
    
    
    output$Save_sankey_basin_plot <- downloadHandler(
      filename = function(file) {
        "sankey_box_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = create_basin_sankey_plot(), width = 590, height = 365, units = "mm", device = "png")
      }
    )
    
    #--------------------------------------------------------------------------------------------
    # table
    output$table <- renderDT({
      Sys.sleep(1.5)
      # coloring
      crop_color_mapping_df <- as.data.frame(crop_colors())
      crop_color_mapping_df$crop <- names(crop_colors())
      crop_color_mapping_df <- crop_color_mapping_df[complete.cases(crop_color_mapping_df),]
      names(crop_color_mapping_df) <- c("color", "crop")
      
      All_rot_clean <- transform_rotation_summary(
        All_rot_big = CropRotViz_intersection,
        area_range = input$Area_range_sec,
        choices = Crop_choices,
        selected_crops = input$Crops_sec,
        max_rows = 3000,
        years = years
      )
      
      # Generate the datatable with dynamic year formatting
      datatable(All_rot_clean, options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': '#fff'});",
          "}")
      )) %>% 
        formatStyle(2, 
                    target = 'row', color = "black",
                    backgroundColor = "lightgrey") %>%
        # Loop through the years vector to apply formatting
        purrr::reduce(years, 
                      .init = ., 
                      ~ .x %>%
                        formatStyle(paste0('Name_', .y),
                                    backgroundColor = styleEqual(crop_color_mapping_df$crop, 
                                                                 crop_color_mapping_df$color))) %>%
        formatStyle('area_km2', color = "black",
                    backgroundColor = "lightgrey")
      
      
    })
    #--------------------------------------------------------------------------------------------
    
    # table specific
    output$table_spec <- renderDT({
      Sys.sleep(1.5)
      # coloring
      crop_color_mapping_df <- as.data.frame(crop_colors())
      crop_color_mapping_df$crop <- names(crop_colors())
      crop_color_mapping_df <- crop_color_mapping_df[complete.cases(crop_color_mapping_df),]
      names(crop_color_mapping_df) <- c("color", "crop")
      
      All_rot_clean <- transform_rotation_summary(
        All_rot_big = CropRotViz_intersection,
        area_range = input$Area_range_spec,
        specific_crop = input$Crop_spec,
        type = "specific",
        max_rows = 3000,
        years = years
      )
      
      All_rot_clean <- st_drop_geometry(All_rot_clean)
      
      datatable(All_rot_clean, options = list(
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'color': '#fff'});",
          "}")
      )) %>% 
        formatStyle(2, 
                    target = 'row', color = "black",
                    backgroundColor = "lightgrey") %>%
        # Loop through the years vector to apply formatting
        purrr::reduce(years, 
                      .init = ., 
                      ~ .x %>%
                        formatStyle(paste0('Name_', .y),
                                    backgroundColor = styleEqual(crop_color_mapping_df$crop, 
                                                                 crop_color_mapping_df$color))) %>%
        formatStyle('area_km2', color = "black",
                    backgroundColor = "lightgrey")
      
    })
    
    #--------------------------------------------------------------------------------------------
    observe({
      req(data_loaded())
      req(rotation_data())
      
      if (any(grepl("Aggregated_", names(rotation_data()[[2]])))) {
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
    
    #--------------------------------------------------------------------------------------------
    output$sankey_plotly_specific <- renderPlotly({
      Sys.sleep(1.5)

      summarized_transitions <- process_specific_transitions(CropRotViz_intersection, input$Crop_spec)
      
      
      # Get unique node names and their colors
      unique_nodes <- unique(c(summarized_transitions$source, summarized_transitions$target))
      node_colors <- sapply(unique_nodes, function(name) {
        base_name <- gsub(" \\(before\\)| \\(after\\)", "", name)
        crop_color_mapping[base_name] %||% "#808080"
      })
      
      # Create the Sankey plot
      plot_ly(
        type = "sankey",
        orientation = "h",
        
        node = list(
          pad = 15,
          thickness = 20,
          line = list(
            color = "black",
            width = 0.5
          ),
          label = unique_nodes,
          color = node_colors
        ),
        
        link = list(
          source = match(summarized_transitions$source, unique_nodes) - 1,
          target = match(summarized_transitions$target, unique_nodes) - 1,
          value = summarized_transitions$value
        )
      ) %>%
        layout(
          title = list(
            text = paste("Crop proportion before and after", input$Crop_spec),
            x = 0.5,
            y = 0.95
          ),
          font = list(size = 15, color = "black"),
          paper_bgcolor = 'lightgrey',  
          plot_bgcolor = 'lightgrey',
          height = 1450
          )%>%
          config(
            toImageButtonOptions = list(
              format = "png",
              filename = "combined_rotation",
              width = 1000,
              height = 1450
            )
          )
 
    })
    #--------------------------------------------------------------------------------------------

    output$leaflet_district <- renderLeaflet({
      District_sel <- subset(Districts, NAME_3 == input$District_sel)
      # Transform to WGS84 if needed (Leaflet requires WGS84)
      if (st_crs(Districts) != 4326) {
        District_sel <- st_transform(District_sel, 4326)
      }
      
      # Create map with the bounding box
      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data = District_sel,
          fillColor = "rgb(0,86,157)",
          fillOpacity = 0.3,
          weight = 2,
          color = "rgb(0,86,157)",
          dashArray = "5,5"
        )
      
    })
    #--------------------------------------------------------------------------------------------
    
    output$leaflet_basin <- renderLeaflet({
      EZG_sel <- subset(EZGs, EZG == input$EZG_sel)
      # Transform to WGS84 if needed (Leaflet requires WGS84)
      if (st_crs(EZG_sel) != 4326) {
        EZG_sel <- st_transform(EZG_sel, 4326)
      }
      
      # Create map with the bounding box
      leaflet() %>%
        addTiles() %>%
        addPolygons(
          data = EZG_sel,
          fillColor = "rgb(0,86,157)",
          fillOpacity = 0.3,
          weight = 2,
          color = "rgb(0,86,157)",
          dashArray = "5,5"
        )
      
    })
    #--------------------------------------------------------------------------------------------

    output$district_ridges <- renderPlot({
      data <- district_rotation_data()[[2]]
      if (any(grepl("Aggregated_", names(data)))) {
        column <- paste0("Aggregated_" , input$year_select)
        colors <- crop_colors()
      }else{
        column <- paste0("Name_" , input$year_select)
        colors <- replicate(50, generate_hex_color())
        }
      
      ggplot(data %>%
               group_by(!!sym(column)) %>%
               filter(n() > 1) %>%
               ungroup(),
             aes(x = area/10000, y = !!sym(column), fill = !!sym(column))) +
        geom_boxplot(alpha = 0.7, width = 0.5) +
        theme_minimal() +
        theme(axis.text = element_text(color = "white", size = 15, face="bold"),
              axis.title.y = element_text(color = "white", size = 16, face="bold", margin = margin(r = 10)),
              legend.background = element_rect(fill = "lightgrey", color = "black"),
              panel.background = element_rect(fill = 'lightgrey', color = 'black'),
              legend.position = "none",
              plot.background = element_rect(fill = "#1f1b1b", color = NA),
              legend.box.background = element_rect(fill = "#1f1b1b", color = NA),
              plot.title = element_text(color = "white", hjust = 0.5, size = 18, face="bold")) +
        labs(title = paste("Area Distribution per Crop (n() > 1)\n",  input$year_select, "in", input$District_sel),
             x = "Area (ha)",
             y = "") +
        scale_fill_manual(values = colors) +
        scale_x_continuous(labels = scales::comma)
    })
    
    
    
    #--------------------------------------------------------------------------------------------
    
    output$basin_ridges <- renderPlot({
      data <- basin_rotation_data()[[2]]
      if (any(grepl("Aggregated_", names(data)))) {
        column <- paste0("Aggregated_" , input$year_select)
        colors <- crop_colors()
      }else{
        column <- paste0("Name_" , input$year_select)
        colors <- replicate(50, generate_hex_color())
      }
      
      ggplot(data %>%
               group_by(!!sym(column)) %>%
               filter(n() > 1) %>%
               ungroup(),
             aes(x = area/10000, y = !!sym(column), fill = !!sym(column))) +
        geom_boxplot(alpha = 0.7, width = 0.5) +
        theme_minimal() +
        theme(axis.text = element_text(color = "white", size = 15, face="bold"),
              axis.title.y = element_text(color = "white", size = 16, face="bold", margin = margin(r = 10)),
              legend.background = element_rect(fill = "lightgrey", color = "black"),
              panel.background = element_rect(fill = 'lightgrey', color = 'black'),
              legend.position = "none",
              plot.background = element_rect(fill = "#1f1b1b", color = NA),
              legend.box.background = element_rect(fill = "#1f1b1b", color = NA),
              plot.title = element_text(color = "white", hjust = 0.5, size = 18, face="bold")) +
        labs(title = paste("Area Distribution per Crop (n() > 1)\n",  input$year_select, "in", input$EZG_sel),
             x = "Area (ha)",
             y = "") +
        scale_fill_manual(values = colors) +
        scale_x_continuous(labels = scales::comma)
    })
    
    #--------------------------------------------------------------------------------------------
    
    
    output$district_donut_plot <- renderPlotly({
      data <- district_rotation_data()[[2]]
      
      if (any(grepl("Aggregated_", names(data)))) {
        prefix <- "Aggregated_"
      }else{
        prefix <- "Name_"
      }    
      
      donut_plot <- create_multi_year_donut(data, 
                                            year_columns = paste0(prefix, years),
                                            title = paste("Crop Distribution", min(years), "-", max(years), "in", input$District_sel),
                                            highlight_year = paste0(prefix, input$year_select),
                                            colors = crop_colors()
      )
    })
    
    output$basin_donut_plot <- renderPlotly({
      data <- basin_rotation_data()[[2]]
      View(data)
      if (any(grepl("Aggregated_", names(data)))) {
        prefix <- "Aggregated_"
      }else{
        prefix <- "Name_"
      }    
      
      donut_plot <- create_multi_year_donut(data, 
                                            year_columns = paste0(prefix, years),
                                            title = paste("Crop Distribution", min(years), "-", max(years), "in", input$EZG_sel),
                                            highlight_year = paste0(prefix, input$year_select),
                                            colors = crop_colors()
      )
      
    })
    
  })
}
