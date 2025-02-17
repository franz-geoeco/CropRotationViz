#' Field-Level Crop Rotation Server Function
#' 
#' @description Server-side function that handles the core logic for visualizing field-level crop rotations.
#' The function processes uploaded spatial data files, manages interactive map displays, and generates
#' crop rotation visualizations. It includes features such as:
#' - File upload handling for spatial data (.shp, .gpkg, .fgb formats)
#' - Interactive map generation with field boundaries
#' - District-based filtering of field data
#' - Click-based field selection and rotation pattern display
#' - Dynamic plot generation for crop sequences
#' 
#' @param input Shiny input object containing user inputs
#' @param output Shiny output object for rendering UI elements
#' @param session Shiny session object for managing the current session
#' @param app_data Additional application data passed to the server
#' 
#' @return None (modifies Shiny reactive context)
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
    
    # Show loading message for large files
    showNotification(
      "Loading vector file... This may take several minutes for large files.",
      type = "message",
      duration = NULL,
      id = "loading"
    )
    
    tryCatch({
      sf::sf_use_s2(FALSE)
      
      if (any(tools::file_ext(input$vector_file$name) == "shp")) {
        # Create temporary directory
        temp_dir <- tempfile()
        dir.create(temp_dir)
        
        # Check if all required files are present
        required_extensions <- c("shp", "dbf", "shx")
        uploaded_files <- tools::file_ext(input$vector_file$name)
        
        missing_files <- required_extensions[!required_extensions %in% uploaded_files]
        if (length(missing_files) > 0) {
          stop(paste("Missing required shapefile components:", 
                     paste(missing_files, collapse = ", "), 
                     ". Please upload all required files."))
        }
        
        # Copy all files to temp directory with consistent base name
        base_name <- "shapefile"
        
        for (i in seq_along(input$vector_file$datapath)) {
          ext <- tools::file_ext(input$vector_file$name[i])
          file.copy(input$vector_file$datapath[i], 
                    file.path(temp_dir, paste0(base_name, ".", ext)),
                    overwrite = TRUE)
        }
        
        # Read the shapefile from temp directory
        data <- st_read(file.path(temp_dir, paste0(base_name, ".shp")), quiet = TRUE)
        
        # Clean up temporary directory
        unlink(temp_dir, recursive = TRUE)
        
        # Handle abbreviated column names for shapefiles
        names(data) <- ifelse(names(data) == "Distrct", "District",
                              ifelse(grepl("^Ag_", names(data)), 
                                     sub("^Ag_", "Aggregated_", names(data)),
                                     ifelse(grepl("^Nm_", names(data)),
                                            sub("^Nm_", "Name_", names(data)),
                                            names(data))))
        
        # Update district selector choices
        updateSelectInput(session, "district", 
                          choices = unique(data$District), 
                          selected = unique(data$District)[1])
        
      } else {
        # For non-shapefile formats (gpkg, fgb), read directly
        data <- st_read(input$vector_file$datapath[1], quiet = TRUE)
        # Update district selector choices
        updateSelectInput(session, "district", 
                          choices = unique(data$District), 
                          selected = unique(data$District)[1])
      }
      
      vector_data(data)
      
      removeNotification(id = "loading")
      showNotification("Vector file loaded successfully!", type = "message")
      
    }, error = function(e) {
      removeNotification(id = "loading")
      showNotification(
        paste("Error reading file:", e$message), 
        type = "error",
        duration = NULL
      )
    })
  })
  
  # Reactive expression for filtered data based on selected district
  filtered_data <- reactive({
    req(vector_data(), input$district)
    vector_data()[vector_data()$District == input$district, ]
  })
  
  # Field map output
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
        # First check for Aggregated/Ag columns
        agg_cols <- grep("^(Aggregated_|Ag_)\\d{4}$", names(field), value = TRUE)
        
        # If no Aggregated/Ag columns found, check for Name/Nm columns
        crop_cols <- if(length(agg_cols) > 0) agg_cols else {
          grep("^(Name_|Nm_)\\d{4}$", names(field), value = TRUE)
        }
        
        # Format area with 2 decimal places
        area_text <- sprintf("%.2f", field[["area"]]/10000) # Convert to hectares
        
        hover_text <- paste0(
          "<b>Field ID:</b> ", field["id"], "<br>",
          "<b>Area:</b> ", area_text, " ha<br>",
          "<b>Crop Sequence:</b><br>",
          paste(sapply(crop_cols, function(col) {
            year <- gsub("(Aggregated_|Ag_|Name_|Nm_)", "", col)
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
    # First check for Aggregated/Ag columns
    agg_cols <- grep("^(Aggregated_|Ag_)\\d{4}$", names(field), value = TRUE)
    
    # If no Aggregated/Ag columns found, check for Name/Nm columns
    crop_cols <- if(length(agg_cols) > 0) agg_cols else {
      grep("^(Name_|Nm_)\\d{4}$", names(field), value = TRUE)
    }
    
    data.frame(
      Year = as.numeric(sub(".*_(\\d{4})$", "\\1", crop_cols)),
      Crop = sapply(crop_cols, function(col) field[[col]])
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
        title = list(
          text = plot_title,
          y = 0.95  # Moves the title up by adding more space above
        ),
        xaxis = list(title = "Year", tickangle = 45),
        yaxis = list(title = ""), 
        showlegend = FALSE
      )
  })
}

#' Field-Level Crop Rotation UI Function
#' 
#' @description User interface function that creates the layout for the field-level crop rotation
#' visualization tool. The UI includes:
#' - File upload interface for vector data
#' - District selection dropdown
#' - Interactive map display for field visualization
#' - Conditional panels for displaying crop rotation patterns
#' - Responsive layout with bootstrap grid system
#' - Footer with author and institutional information
#' 
#' @param app_data Application data passed to the UI for initialization
#' 
#' @return A Shiny UI definition that creates a fluid page layout with multiple components
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
      column(4, fileInput("vector_file", 
                          "Choose Vector File (if shapefile select all corresponding files)",
                          accept = c(".shp", ".dbf", ".shx", ".prj", ".cpg", ".gpkg", ".fgb"),
                          multiple = TRUE)
             ),
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
