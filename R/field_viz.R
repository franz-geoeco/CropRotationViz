#' Field-Level Crop Rotation Visualization Server
#' 
#' @description Advanced server logic for field-level crop rotation visualization
library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(ggalluvial)
library(shinyWidgets)
library(plotly)
library(shinyBS)
library(DT)
library(sf)
library(leaflet.minicharts)
library(shinycssloaders)
library(shinyalert)

field_level_server <- function(input, output, session, app_data) {
  options(shiny.maxRequestSize = 10000*1024^2)
  
  # Initialize reactive values
  vector_data <- reactiveVal(NULL)
  selected_field <- reactiveVal(NULL)
  
  # Add at the beginning of the server function
  observe({
    shinyalert(
      title = "Welcome!",
      text = paste("This application allows you to explore crop rotations at the field level.",
                   "Select a vector file processed by CropRotationViz and click on individual fields in the districts to view their rotation patterns."),
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
  
  # Handle file upload
  observeEvent(input$vector_file, {
    req(input$vector_file)
    
    tryCatch({
      sf::sf_use_s2(FALSE)
      
      # Get file extension
      file_ext <- tolower(tools::file_ext(input$vector_file$datapath))
      
      # Read spatial data
      data <- if(file_ext == "shp") {
        temp_dir <- tempdir()
        file_base <- tools::file_path_sans_ext(input$vector_file$name)
        
        for(ext in c("shp", "dbf", "shx", "prj")) {
          from_file <- file.path(dirname(input$vector_file$datapath), paste0(file_base, ".", ext))
          to_file <- file.path(temp_dir, paste0(file_base, ".", ext))
          if(file.exists(from_file)) file.copy(from_file, to_file, overwrite = TRUE)
        }
        
        st_read(file.path(temp_dir, paste0(file_base, ".shp")), quiet = TRUE)
      } else {
        st_read(input$vector_file$datapath, quiet = TRUE)
      }
      
      vector_data(data)
      
      # Update district selector choices
      updateSelectInput(session, "district", choices = unique(data$District), selected = unique(data$District)[1])
      
      showNotification("Vector file loaded successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
    })
  })
  
  # Reactive expression for filtered data based on selected district
  filtered_data <- reactive({
    req(vector_data(), input$district)
    vector_data()[vector_data()$District == input$district, ]
  })
  
  output$field_map <- renderLeaflet({
    req(filtered_data())
    Sys.sleep(1.5)
    
    data <- filtered_data()
    
    tryCatch({
      if (!all(st_geometry_type(data) %in% c("POLYGON", "MULTIPOLYGON"))) {
        data <- st_cast(data, "MULTIPOLYGON")
      }
      
      if (st_crs(data) != 4326) {
        data <- st_transform(data, 4326)
      }
      
      data$id <- seq_len(nrow(data))
      
      # Create hover text containing all crop years and area info
      data$hover_info <- apply(data, 1, function(field) {
        crop_cols <- grep("^Aggregated_\\d{4}$", names(field), value = TRUE)
        if(length(crop_cols) == 0) {
          crop_cols <- grep("^Name_\\d{4}$", names(field), value = TRUE)
        }
        
        # Format area with 2 decimal places
        area_text <- sprintf("%.2f", field[["area"]]/10000) # Convert to hectares
        
        hover_text <- paste0(
          "<b>Field ID:</b> ", field["id"], "<br>",
          "<b>Area:</b> ", area_text, " ha<br>",
          "<b>Crop Sequence:</b><br>",
          paste(sapply(crop_cols, function(col) {
            year <- gsub("(Aggregated_|Name_)", "", col)
            crop <- field[[col]]
            paste0(year, ": ", crop)
          }), collapse = "<br>")
        )
        
        return(hover_text)
      })
      
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(
          color = "#444444", weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 0.1, fillColor = NA,
          layerId = ~id,
          highlightOptions = highlightOptions(
            color = "white", weight = 2, bringToFront = TRUE
          ),
          label = lapply(data$hover_info, HTML)
        )
      
    }, error = function(e) {
      showNotification(paste("Error processing spatial data:", e$message), type = "error")
      leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2)
    })
  })
  
  
  # Handle map clicks
  observeEvent(input$field_map_shape_click, {
    click <- input$field_map_shape_click
    if (!is.null(click$id)) {
      selected_field(filtered_data()[click$id, ])
    }
  })
  
  # Field selection visibility
  output$field_selected <- reactive({ !is.null(selected_field()) })
  outputOptions(output, "field_selected", suspendWhenHidden = FALSE)
  
  # Crop rotation plots
  extract_crop_rotation_data <- function(field) {
    agg_cols <- grep("^Aggregated_\\d{4}$", names(field), value = TRUE)
    if (length(agg_cols) == 0) agg_cols <- grep("^Name_\\d{4}$", names(field), value = TRUE)
    
    data.frame(
      Year = as.numeric(sub(".*_(\\d{4})$", "\\1", agg_cols)),
      Crop = sapply(agg_cols, function(col) field[[col]])
    ) %>%
      arrange(Year)
  }
  
  output$rotation_plotly <- renderPlotly({
    req(selected_field())
    
    plot_data <- extract_crop_rotation_data(selected_field())
    
    # Format area for title
    area_ha <- sprintf("%.2f", selected_field()$area/10000)
    plot_title <- sprintf("Crop Rotation Pattern (Area: %s ha)", area_ha)
    
    plot_ly(plot_data, x = ~Year, y = ~Crop, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#1f77b4'), marker = list(size = 10, color = '#1f77b4')) %>%
      layout(
        title = plot_title, 
        xaxis = list(title = "Year", tickangle = 45),
        yaxis = list(title = "Crop Type"), 
        showlegend = FALSE
      )
  })
}

# UI Function
field_level_ui <- function(app_data) {
  fluidPage(
    theme = shinytheme("cyborg"),
    
    # Header
    fluidRow(
      column(4, h2("Field-Level Crop Sequences")), 
      column(5),
      column(2, tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
    ),
    br(),
    
    # File Upload & District Selection
    fluidRow(
      column(4, fileInput("vector_file", "Choose Processed Vector File", accept = c(".shp", ".gpkg", ".fgb"), multiple = FALSE)),
      column(4, selectInput("district", "Select District:", choices = NULL))
    ),
    
    # Map and Chart Section
    fluidRow(
      column(8,         # Add loading spinner to map
             shinycssloaders::withSpinner(
               leafletOutput("field_map", height = "600px")
             )
      ),
      column(4, 
             conditionalPanel(
               condition = "output.field_selected",
               wellPanel(
                 tabsetPanel(
                   tabPanel("Line Plot", plotlyOutput("rotation_plotly", height = "400px"))
                 )
               )
             ),
             conditionalPanel(
               condition = "!output.field_selected",
               wellPanel(h4("Click a field to view its rotation pattern", style = "color: white; text-align: center;"))
             )
      )
    ),
    
    # Footer
    tags$footer(
      style = "position: fixed; bottom: 0; width: 100%; background-color: #f5f5f5; padding: 10px; text-align: center;",
      div(
        style = "display: inline-block;",
        p(tags$span("Author: "), tags$a(href = "https://github.com/franz-geoeco/CropRotationViz", "Franz Schulze")),
        p(tags$span("Institution: "), tags$a(href = "https://geooeko.geo.uni-halle.de/", "University of Halle-Wittenberg"))
      )
    )
  )
}
