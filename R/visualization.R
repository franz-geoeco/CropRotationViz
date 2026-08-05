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
#' @import shiny shinythemes shinyBS shinyWidgets shinycssloaders DT plotly ggplot2 dplyr sf leaflet
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column plotOutput uiOutput renderUI verbatimTextOutput fileInput actionButton wellPanel
#' @importFrom shinythemes shinytheme
#' @importFrom shinyBS bsButton bsPopover
#' @importFrom shinyWidgets pickerInput pickerOptions
#' @importFrom shinycssloaders withSpinner
#' @importFrom htmltools tags HTML
#' @importFrom tidyr pivot_longer
#' @importFrom purrr reduce
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
  # Add Bootstrap dependencies
  shiny::addResourcePath(
    "shinyBS", 
    system.file("www", package = "shinyBS")
  )
  
  # Extract years and create span string outside the fluidRow
  year_span <- paste0(
    min(as.numeric(gsub(".*?(\\d+).*", "\\1",
                        names(district_CropRotViz_intersection[[1]])[grepl("\\d", names(district_CropRotViz_intersection[[1]]))]))),
    "-",
    max(as.numeric(gsub(".*?(\\d+).*", "\\1",
                        names(district_CropRotViz_intersection[[1]])[grepl("\\d", names(district_CropRotViz_intersection[[1]]))])))
  )

  # Maximum sequence area (km²) present in the current dataset - used as the
  # upper bound and default value for the Sequence Area Range inputs, so that
  # by default no sequence is excluded from the plots. The Sankey/table data
  # (rotation_data) is filtered on the aggregated per-rotation "freq" (km²),
  # not on the raw per-field "area" used for the distribution/export data, and
  # freq can be much larger than any single field's area, so both have to be
  # taken into account.
  max_freq_km2 <- function(lst) {
    if (is.null(lst) || (length(lst) == 1 && identical(lst, NA))) return(-Inf)
    max(vapply(lst, function(x) {
      if (is.null(x) || is.null(x[["freq"]]) || length(x[["freq"]]) == 0) return(-Inf)
      max(x[["freq"]], na.rm = TRUE)
    }, numeric(1)))
  }

  max_area_km2 <- ceiling(max(
    max(distribution_df$area, na.rm = TRUE) / 1e6,
    max_freq_km2(district_CropRotViz_intersection),
    max_freq_km2(EZG_CropRotViz_intersection),
    max_freq_km2(AOI_CropRotViz_intersection)
  ))

  fluidPage(
    Crop_choices <- "",
    District_choices <- "",
    EZG_choices <- "",
    theme = shinytheme("cyborg"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "shinyBS/shinyBS.css"),
      tags$script(src = "shinyBS/shinyBS.js")
    ),
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
      .metric-box {
        background-color: #222222;
        border: 2px solid #74961E;
        border-radius: 5px;
        padding: 15px;
        margin: 10px 0;
        text-align: center;
      }
      .metric-value {
        font-size: 32px;
        font-weight: bold;
        color: #74961E;
      }
      .metric-label {
        font-size: 14px;
        color: #999;
        margin-top: 5px;
      }
    ")),
    
    # Define the tabs
    tabsetPanel(id = "tabs",
                tabPanel("1. Plot Sequence",
                         br(),
                         fluidRow(
                           #-------------------------------------------------------------------------------
                           # Header Section
                           #-------------------------------------------------------------------------------
                           column(style = "margin-top: -30px; margin-bottom: 0px;", width = 4, h2("Crop Sequences"), p("Analyze the Rotation Sequences of the Complete Processed Area")), 
                           column(5),
                           column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
                         ),
                         br(),
                         fluidRow(
                           #-------------------------------------------------------------------------------
                           # Step 1: Settings
                           #-------------------------------------------------------------------------------
                           column(2,
                                  shinyWidgets::numericRangeInput("Area_range_sec", "Sequence Area Range (km²):",
                                                    min = 0, max = max_area_km2,
                                                    value = c(0, max_area_km2))
                           ),
                           column(1, 
                                  bsButton("range-info",
                                           label = "",
                                           icon = icon("info"),
                                           style = "default",
                                           size = "extra-small"
                                  ),
                                  bsPopover(
                                    id = "range-info",
                                    title = "Sequence Size",
                                    content ="Here you can select the size of a full sequence to be displayed. Therfore a selection from 100-200 means that a full 7 year sequence has to be between 100 and 200 km².",
                                    placement = "right",
                                    trigger = "hover",
                                    options = list(
                                      container = 'body',
                                      html = TRUE,
                                      delay = list(show = 0, hide = 0) 
                                    )
                                  ),
                           ),
                           column(2, 
                                  shinyWidgets::pickerInput(
                                    inputId = "Crops_sec", 
                                    label = "Show/hide crops:", 
                                    choices = Crop_choices, 
                                    selected = Crop_choices, 
                                    options = shinyWidgets::pickerOptions(
                                      actionsBox = TRUE, 
                                      size = 15,
                                      selectedTextFormat = "count > 3"
                                    ), multiple = TRUE)
                           ),
                           column(1, bsButton("crops-info",
                                              label = "",
                                              icon = icon("info"),
                                              style = "default", size = "extra-small"),
                                  bsPopover(
                                    id = "crops-info",
                                    title = "Crops Selection",
                                    content = "Here you can include or exclude crops from charting if you want to highlight specific crops or disable to dominant wants.",
                                    placement = "right",
                                    trigger = "hover",
                                    options = list(
                                      container = 'body',
                                      html = TRUE,
                                      delay = list(show = 0, hide = 0) 
                                    )
                                  ),
                           ),
                           column(4, plotOutput("perc_plot", height = 80))
                         ),
                         fluidRow(
                           column(2),
                           column(2,
                                  actionButton("update_plot_sequence",
                                               "Load Visualization",
                                               class = "btn-primary btn-block",
                                               icon = icon("sync"))
                           )
                         ),
                         br(),
                         #-------------------------------------------------------------------------------
                         # Step 2: Major Sankey plot
                         #-------------------------------------------------------------------------------
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
                                  downloadButton("Save_sankey_plot", label = "Save Plot"
                                  ),
                                  br(), br(),
                                  shinyWidgets::switchInput(
                                    inputId = "sankey_theme_sec",
                                    label = "Theme",
                                    value = TRUE,
                                    onLabel = "Dark",
                                    offLabel = "Light",
                                    size = "mini"
                                  ),
                                  sliderInput("sankey_textsize_sec", "Text size",
                                              min = 8, max = 24, value = 15, step = 1)
                           )
                         ),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                         
                         # Horizontal line
                         tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                         
                         #-------------------------------------------------------------------------------
                         # Step 3: Data Table
                         #-------------------------------------------------------------------------------
                         fluidRow(
                           column(1),
                           column(3,
                                  h4("Unique Sequences and Sizes")),
                           column(1, bsButton("table-info",
                                              label = "",
                                              icon = icon("info"),
                                              style = "default", size = "extra-small"),
                                  bsPopover(
                                    id = "table-info",
                                    title = "Table with the Largest Crop Sequence Patterns",
                                    content = "Here you can see the largest crop sequence pattern in MWP in a 7 year sequence (or selected range). You can search, sort or expand the table as you like.",
                                    placement = "left",
                                    trigger = "hover",
                                    options = list(
                                      container = 'body',
                                      html = TRUE,
                                      delay = list(show = 0, hide = 0) 
                                    )
                                  )
                           )
                         ),br(),
                         
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    DTOutput('table', width = "100%", height = "auto", fill = TRUE)
                                  )
                           )
                         ),
                         fluidRow(
                           column(1),
                           column(10,
                                  div(style = "margin-top: 15px;",
                                      downloadButton("export_table_csv", "Export as CSV",
                                                     style = "margin-right: 10px;margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)"),
                                      downloadButton("export_table_xlsx", "Export as Excel",
                                                     style = "margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)")
                                  )
                           )
                         ),
                         #---------------------------------------------------------------
                         # Transition Probability Heatmap
                         #---------------------------------------------------------------
                         tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                         fluidRow(
                           column(1),
                           column(3,
                                  h4("Crop Transition Probability Heatmap")),
                           column(1, shinyBS::bsButton("transition_heatmap-info",
                                                       label = "",
                                                       icon = icon("info"),
                                                       style = "default", size = "extra-small"),
                                  shinyBS::bsPopover(
                                    id = "transition_heatmap-info",
                                    title = "Transition Probabilities",
                                    content = "Shows the area-weighted probability of each crop following another across all consecutive year transitions. Each row sums to 100%. Read as: given crop A was grown, what is the probability crop B follows?",
                                    placement = "right",
                                    trigger = "hover",
                                    options = list(
                                      container = 'body',
                                      html = TRUE,
                                      delay = list(show = 0, hide = 0)
                                    )
                                  )
                           ),
                           column(5,
                                  div(style = "margin-top: 5px;",
                                      downloadButton("export_heatmap_csv", "Export as CSV",
                                                     style = "margin-right: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)"),
                                      downloadButton("export_heatmap_xlsx", "Export as Excel",
                                                     style = "color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)")
                                  )
                           )
                         ),
                         br(),
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    plotlyOutput("transition_heatmap", height = "900px")
                                  )
                           )
                         ),
                         #-------------------------------------------------------------------------------
                         # Aggregation Visualization
                         #-------------------------------------------------------------------------------
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
                         ),
                         br(), br(), br(), br(), br()
                ),
                tabPanel("2. Plot Crop Specific Sequence",
                         br(),
                         fluidRow(
                           column(style = "margin-top: -30px; margin-bottom: 0px;", width = 4, h2("Crop Specific Sequences"), p("Analyze the Rotation with a Specific Fokus Crop")), 
                           column(5),
                           column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
                         ),
                         br(),
                         fluidRow(column(2,
                                         shinyWidgets::numericRangeInput("Area_range_spec", "Sequence Area Range (km²):",
                                                           min = 0, max = max_area_km2,
                                                           value = c(0, max_area_km2))
                         ),
                         column(1, shinyBS::bsButton("range_spec-info",
                                                     label = "",
                                                     icon = icon("info"),
                                                     style = "default", size = "extra-small"),
                                shinyBS::bsPopover(
                                  id = "range_spec-info",
                                  title = "sequence Size",
                                  content = "Here you can select the size of a full sequence to be displayed. Therfore a selection from 100-200 means that a full 7 year sequence has to be between 100 and 200 km².",
                                  placement = "right",
                                  trigger = "hover",
                                  options = list(
                                    container = 'body',
                                    html = TRUE,
                                    delay = list(show = 0, hide = 0) 
                                  )
                                )
                         ),
                         column(2, 
                                shinyWidgets::pickerInput(
                                  inputId = "Crop_spec", 
                                  label = "Specific crop:", 
                                  choices = Crop_choices, 
                                  selected = "protein plants", 
                                  options = shinyWidgets::pickerOptions(
                                    actionsBox = TRUE, 
                                    size = 15,
                                    selectedTextFormat = "count > 3"
                                  ), multiple = F)
                         ),
                         column(1, shinyBS::bsButton("crops_spec-info",
                                                     label = "",
                                                     icon = icon("info"),
                                                     style = "default", size = "extra-small"),
                                shinyBS::bsPopover(
                                  id = "crops_spec-info",
                                  title = "Crop Selection",
                                  content = "Here you can choose a crop for charting. The crop has to be in one of the seven years (in the sequence)",
                                  placement = "right",
                                  trigger = "hover",
                                  options = list(
                                    container = 'body',
                                    html = TRUE,
                                    delay = list(show = 0, hide = 0) 
                                  )
                                )
                         ),
                         column(4, plotOutput("perc_spec_plot", height = 80))
                         ),
                         fluidRow(
                           column(2),
                           column(2,
                                  actionButton("update_crop_specific",
                                               "Load Visualization",
                                               class = "btn-primary btn-block",
                                               icon = icon("sync"))
                           )
                         ),
                         br(),
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
                                  downloadButton("Save_sankey_specific_plot", label = "Save Plot"
                                  ),
                                  br(), br(),
                                  shinyWidgets::switchInput(
                                    inputId = "sankey_theme_spec",
                                    label = "Theme",
                                    value = TRUE,
                                    onLabel = "Dark",
                                    offLabel = "Light",
                                    size = "mini"
                                  ),
                                  sliderInput("sankey_textsize_spec", "Text size",
                                              min = 8, max = 24, value = 15, step = 1)
                           )
                         ),
                         br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                         # Horizontal line
                         tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                         fluidRow(
                           column(1),
                           column(8,
                                  h4("Crop Area Distribution Map"))
                         ),
                         fluidRow(
                           column(1),
                           column(3,
                                  radioButtons(
                                    inputId = "map_type_spec",
                                    label = "Select Map Type:",
                                    choices = c("Districts" = "districts", "Catchments" = "catchments"),
                                    selected = "districts",
                                    inline = TRUE
                                  )
                           ),
                           column(3,
                                  radioButtons(
                                    inputId = "year_mode_spec",
                                    label = "Display Mode:",
                                    choices = c("Complete Average" = "average", "Annual" = "annual"),
                                    selected = "average",
                                    inline = TRUE
                                  )
                           ),
                           column(4,
                                  conditionalPanel(
                                    condition = "input.year_mode_spec == 'annual'",
                                    sliderInput(
                                      inputId = "year_select_spec",
                                      label = "Select Year:",
                                      min = 2000,
                                      max = 2024,
                                      value = 2008,
                                      step = 1,
                                      sep = ""
                                    )
                                  )
                           )
                         ),
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    leafletOutput("leaflet_crop_spec", height = "600px")
                                  )
                           )
                         ),
                         fluidRow(
                           column(1),
                           column(4,
                                  p(class = "text-muted",
                                    "The download includes every year as a separate column ",
                                    "(pct_<year>) plus the complete average (pct_avg), regardless ",
                                    "of which display mode is currently selected above."
                                  ),
                                  downloadButton(
                                    "crop_spec_download_shp",
                                    "Download Map (Shapefile)",
                                    class = "btn-success"
                                  )
                           )
                         ),
                         br(),br(),br(),
                         # Horizontal line
                         tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                         fluidRow(
                           column(1),
                           column(2,
                                  h4("Unique Sequences and Size")),
                           column(1, shinyBS::bsButton("table_spec-info",
                                                       label = "",
                                                       icon = icon("info"),
                                                       style = "default", size = "extra-small"),
                                  shinyBS::bsPopover(
                                    id = "table_spec-info",
                                    title = "Table with the Largest Crop Sequence Patterns",
                                    content = "Here you can see the largest crop sequence pattern in MWP in a 7 year sequence (or selected range). You can search, sort or expand the table as you like.",
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
                         br(),
                         fluidRow(
                           column(1),
                           column(10,
                                  shinycssloaders::withSpinner(
                                    DTOutput('table_spec', width = "100%", height = "auto", fill = TRUE)
                                  )
                           )
                         ),
                         fluidRow(
                           column(1),
                           column(10,
                                  div(style = "margin-top: 15px;",
                                      downloadButton("export_table_spec_csv", "Export as CSV",
                                                     style = "margin-right: 10px; margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)"),
                                      downloadButton("export_table_spec_xlsx", "Export as Excel",
                                                    style = "margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)")
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
                                    plotlyOutput("sankey_plotly_specific", height = "1500px")
                                  )
                           )
                         ),
                         br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                         br(), br(), br(), br(), br(), br(), br(), br(), br(), br()
                         
                ),
                tabPanel("3. Plot Sequence per Area",
                         br(),
                         fluidRow(
                           column(width = 4, style = "margin-top: -30px; margin-bottom: 0px;", h2("Crop Sequences Per Area"), p("Analyze the Rotation Situation in Specific Area")), 
                           column(5),
                           column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
                         ),
                         fluidRow(
                           if(sum(is.na(EZGs))==0 || (exists("AOI_CropRotViz_intersection") && !is.null(AOI_CropRotViz_intersection) && !identical(AOI_CropRotViz_intersection, NA))){
                             column(1,
                                    radioButtons(inputId = "district_or_ezg",
                                                 label = "Select",
                                                 selected = "District",
                                                 choices = c(
                                                   "District",
                                                   if(sum(is.na(EZGs))==0) "River Basin",
                                                   if(exists("AOI_CropRotViz_intersection") && !is.null(AOI_CropRotViz_intersection) && !identical(AOI_CropRotViz_intersection, NA)) "Custom Area"
                                                 ))
                             )
                           },
                           conditionalPanel(
                             condition = "input.district_or_ezg != 'River Basin'",
                             column(2,
                                    shinyWidgets::pickerInput(
                                      inputId = "District_sel", 
                                      label = "District you wanna display", 
                                      choices = District_choices, 
                                      selected = District_choices[1],
                                      options = shinyWidgets::pickerOptions(
                                        actionsBox = TRUE, 
                                        size = 15,
                                        selectedTextFormat = "count > 3"
                                      ), multiple = F)
                             )
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             column(2,
                                    shinyWidgets::pickerInput(
                                      inputId = "EZG_sel",
                                      label = "River basin you wanna display",
                                      choices = EZG_choices,
                                      selected = EZG_choices[1],
                                      options = shinyWidgets::pickerOptions(
                                        actionsBox = TRUE,
                                        size = 15,
                                        selectedTextFormat = "count > 3"
                                      ), multiple = F)
                             )
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'Custom Area'",
                             column(2,
                                    shinyWidgets::pickerInput(
                                      inputId = "AOI_sel",
                                      label = "Custom area to display",
                                      choices = character(0),
                                      selected = NULL,
                                      options = shinyWidgets::pickerOptions(
                                        actionsBox = TRUE,
                                        size = 15,
                                        selectedTextFormat = "count > 3"
                                      ), multiple = F)
                             )
                           ),
                           column(2,
                                  shinyWidgets::numericRangeInput("Area_range_areas", "Sequence Area Range (km²):",
                                                    min = 0, max = max_area_km2,
                                                    value = c(0, max_area_km2))
                           ),
                           column(2,
                                  shinyWidgets::pickerInput(
                                    inputId = "Crops_kreis",
                                    label = "Show/hide crops:",
                                    choices = Crop_choices,
                                    selected = Crop_choices,
                                    options = shinyWidgets::pickerOptions(
                                      actionsBox = TRUE,
                                      size = 15,
                                      selectedTextFormat = "count > 3"
                                    ), multiple = TRUE)
                           ),
                           column(1,
                                  actionButton("update_plot_areas",
                                               "Reload",
                                               width = '100%',
                                               style = "color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157); margin-top: 25px")
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'District'",
                             column(3, plotOutput("perc_district_plot", height = 80))
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             column(3, plotOutput("perc_basin_plot", height = 80))
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'Custom Area'",
                             column(3, plotOutput("perc_aoi_plot", height = 80))
                           )
                         ),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'District'",
                             column(2,
                                    shinycssloaders::withSpinner(
                                      leafletOutput("leaflet_district")
                                    )
                             )
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'District'",
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
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'Custom Area'",
                             column(2,
                                    shinycssloaders::withSpinner(
                                      leafletOutput("leaflet_aoi")
                                    )
                             )
                           ),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'Custom Area'",
                             column(10,
                                    shinycssloaders::withSpinner(
                                      plotOutput("sankey_aoi")
                                    )
                             )
                           )
                         ),
                         fluidRow(
                           br(), br(),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'District'",
                             fluidRow(
                               column(1, style = "margin-left: 10px;",
                                      downloadButton("Save_sankey_district_plot", label = "Save Plot"),
                                      br(), br(),
                                      shinyWidgets::switchInput(
                                        inputId = "sankey_theme_district",
                                        label = "Theme",
                                        value = TRUE,
                                        onLabel = "Dark",
                                        offLabel = "Light",
                                        size = "mini"
                                      ),
                                      sliderInput("sankey_textsize_district", "Text size",
                                                  min = 8, max = 24, value = 15, step = 1)
                               )
                             )
                           )
                         ),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'District'",
                             column(3,
                                    div(style = "margin-left: 0px;",
                                        downloadButton("export_district_csv", "Export as CSV",
                                                       style = "margin-right: 10px; margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)"),
                                        downloadButton("export_district_xlsx", "Export as Excel",
                                                       style = "margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)")
                                    )
                             )
                           )
                         ),
                         fluidRow(
                           br(), br(),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             fluidRow(
                               column(1, style = "margin-left: 10px;",
                                      downloadButton("Save_sankey_basin_plot", label = "Save Plot"),
                                      br(), br(),
                                      shinyWidgets::switchInput(
                                        inputId = "sankey_theme_basin",
                                        label = "Theme",
                                        value = TRUE,
                                        onLabel = "Dark",
                                        offLabel = "Light",
                                        size = "mini"
                                      ),
                                      sliderInput("sankey_textsize_basin", "Text size",
                                                  min = 8, max = 24, value = 15, step = 1)
                               )
                             )
                           )
                         ),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'River Basin'",
                             column(3,
                                    div(style = "margin-left: 0px;",
                                        downloadButton("export_basin_csv", "Export as CSV",
                                                       style = "margin-right: 10px; margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)"),
                                        downloadButton("export_basin_xlsx", "Export as Excel",
                                                       style = "margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)")
                                    )
                             )
                           )
                         ),
                         fluidRow(
                           br(), br(),
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'Custom Area'",
                             fluidRow(
                               column(1, style = "margin-left: 10px;",
                                      downloadButton("Save_sankey_aoi_plot", label = "Save Plot"),
                                      br(), br(),
                                      shinyWidgets::switchInput(
                                        inputId = "sankey_theme_aoi",
                                        label = "Theme",
                                        value = TRUE,
                                        onLabel = "Dark",
                                        offLabel = "Light",
                                        size = "mini"
                                      ),
                                      sliderInput("sankey_textsize_aoi", "Text size",
                                                  min = 8, max = 24, value = 15, step = 1)
                               )
                             )
                           )
                         ),
                         fluidRow(
                           conditionalPanel(
                             condition = "input.district_or_ezg == 'Custom Area'",
                             column(3,
                                    div(style = "margin-left: 0px;",
                                        downloadButton("export_aoi_csv", "Export as CSV",
                                                       style = "margin-right: 10px; margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)"),
                                        downloadButton("export_aoi_xlsx", "Export as Excel",
                                                       style = "margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)")
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
                         ),
                          br(), br(), br()
                ),
                if (exists("diversity_data") && is.list(diversity_data) && length(diversity_data) > 0) {
                  tabPanel("4. Plot Diversity",
                           br(),
                           fluidRow(
                            column(width = 8,
                              style = "margin-top: -30px; margin-bottom: 0px;",
                              h2("Interactive District/River Catchment Crop Diversity Map"),
                              p("Map and Compare the Structural Rotation Diversity compared th Soil Potential ")), 
                            column(1),
                            column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
                           ),
                           fluidRow(column(3,
                                           radioButtons("AreaType",
                                                        "Select Map Type:", inline = TRUE,
                                                        choices = c("Districts" = "districts",
                                                                    "Catchment" = "ezg",
                                                                    "Custom Area" = "aoi"))),
                                    column(5,
                                           h4(paste0("The Structural Diversity is defined by the number of transitions and unique crops over the period ",
                                                     year_span,
                                                     ". The map shows the area weighted mean value per area."))
                                    )),
                           fluidRow(column(1),
                                    column(10,
                                           div(
                                             style = "background-color: #f8f9fa; padding: 10px; margin-bottom: 15px; border-left: 4px solid #007bff;",
                                             strong("Soil Potential Layer Source: "),
                                             "Bodenschätzung (German soil assessment) data provides information about soil quality and agricultural potential. ",
                                             "This layer can be toggled on/off using the layer control in the top-right corner of the map."
                                           ),
                                           leafletOutput("diversity", height = "650px")
                                    )), br(), br(),
                           fluidRow(column(1),
                                    column(10,
                                           plotlyOutput("diversity_soil", height = "650px" )
                                    )),
                           br(),
                           fluidRow(column(1),
                                    column(10,
                                           div(style = "margin-left: 0px;",
                                               downloadButton("export_diversity_csv", "Export as CSV",
                                                              style = "margin-right: 10px; margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)"),
                                               downloadButton("export_diversity_xlsx", "Export as Excel",
                                                              style = "margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)")
                                           )
                                    )),
                           #---------------------------------------------------------------
                           # Shannon Diversity Section
                           #---------------------------------------------------------------
                           tags$hr(style = "border-top: 1px solid white; margin-top: 20px; margin-bottom: 20px;"),
                           fluidRow(
                             column(1),
                             column(3,
                                    h4("Shannon-Wiener Crop Diversity Index")),
                             column(1, shinyBS::bsButton("shannon-info",
                                                         label = "",
                                                         icon = icon("info"),
                                                         style = "default", size = "extra-small"),
                                    shinyBS::bsPopover(
                                      id = "shannon-info",
                                      title = "Shannon-Wiener Index",
                                      content = paste0(
                                        "The Shannon-Wiener index <b>H = -&Sigma;(p<sub>i</sub> &middot; ln p<sub>i</sub>)</b> ",
                                        "quantifies crop diversity per area unit, where p<sub>i</sub> is the area proportion of crop i.<br><br>",
                                        "<b>H = 0</b>: Monoculture (single crop covers 100% of area).<br>",
                                        "<b>H &gt; 2</b>: High diversity with many crops at similar proportions.<br><br>",
                                        "Unlike a simple crop count, Shannon H accounts for <b>evenness</b>: ",
                                        "an area with 5 crops where one dominates at 90% scores lower than ",
                                        "an area with 5 crops at equal shares.<br><br>",
                                        "Values are computed per year and averaged across all years in the time series. ",
                                        "The map shows H per district/catchment, the bar chart ranks areas from lowest to highest diversity."
                                      ),
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
                           fluidRow(
                             column(1),
                             column(5,
                                    shinycssloaders::withSpinner(
                                      leafletOutput("shannon_map", height = "500px")
                                    )
                             ),
                             column(5,
                                    shinycssloaders::withSpinner(
                                      plotlyOutput("shannon_ranking", height = "650px")
                                    )
                             )
                           ),
                           br(), br(), br(), br(),

                  )
                },
                # Rotation Ranking Tab
                tabPanel("5. Rotation Ranking",
                         br(),
                         fluidRow(
                          column(width = 6,
                            style = "margin-top: -30px; margin-bottom: 0px;",
                            h2("Crop Rotation Ranking Analysis"),
                            p("Analyze and rank crop rotation patterns by length and year range")
                           ),
                          column(3),
                          column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
                         ),
                         br(),
                         fluidRow(
                           # Sidebar controls
                           column(width = 3,
                                  wellPanel(
                                    h4("Analysis Parameters"),               
                                    sliderInput(
                                      "ranking_rotation_length",
                                      "Rotation Length (Years):",
                                      min = 2,
                                      max = 10,
                                      value = 3,
                                      step = 1
                                    ),
                                    
                                    hr(),
                                    
                                    h4("Year Range Selection"),
                                    
                                    uiOutput("ranking_year_range_ui"),
                                    
                                    hr(),

                                    h4("Filter Options"),
                                    
                                    uiOutput("ranking_area_filter_ui"),
                                    
                                    uiOutput("ranking_crop_filter_ui"),
                                    
                                    uiOutput("ranking_specific_crop_ui"),
                                    
                                    uiOutput("ranking_spatial_filter_ui"),
                                    
                                    hr(),
                                    
                                    actionButton(
                                      "ranking_analyze_btn",
                                      "Analyze Rotations",
                                      class = "btn-primary btn-block",
                                      icon = icon("chart-bar")
                                    ),
                                    
                                    hr(),
                                    
                                    downloadButton(
                                      "ranking_download_data",
                                      "Download Results",
                                      class = "btn-success btn-block"
                                    ),

                                    br(), br(),

                                    conditionalPanel(
                                      condition = "input.ranking_spatial_type != 'full'",
                                      downloadButton(
                                        "ranking_download_shp",
                                        "Download Map (Shapefile)",
                                        class = "btn-success btn-block"
                                      )
                                    )
                                  )
                           ),
                           
                           # Main content area
                           column(width = 9,
                                  # Summary Metrics Row
                                  fluidRow(
                                    column(3,
                                           div(class = "metric-box",
                                               div(class = "metric-value", textOutput("ranking_total_rotations")),
                                               div(class = "metric-label", "Unique Rotations")
                                           )
                                    ),
                                    column(3,
                                           div(class = "metric-box",
                                               div(class = "metric-value", textOutput("ranking_total_area")),
                                               div(class = "metric-label", "Total Area (km²)")
                                           )
                                    ),
                                    column(3,
                                           div(class = "metric-box",
                                               div(class = "metric-value", textOutput("ranking_total_fields")),
                                               div(class = "metric-label", "Total Fields")
                                           )
                                    ),
                                    column(3,
                                           div(class = "metric-box",
                                               div(class = "metric-value", textOutput("ranking_avg_area")),
                                               div(class = "metric-label", "Avg. Area per Rotation (km²)")
                                           )
                                    )
                                  ),
                                  
                                  # Tabs for different views
                                  tabsetPanel(
                                    type = "tabs",
                                    
                                    tabPanel(
                                      "Ranking Table",
                                      icon = icon("table"),
                                      br(),
                                      DT::dataTableOutput("ranking_rotation_table")
                                    ),
                                    
                                    tabPanel(
                                      "Area Chart",
                                      icon = icon("chart-bar"),
                                      br(),
                                      plotlyOutput("ranking_area_chart", height = "600px")
                                    ),
                                    
                                    tabPanel(
                                      "Top 20 Distribution",
                                      icon = icon("chart-pie"),
                                      br(),
                                      plotlyOutput("ranking_pie_chart", height = "600px")
                                    )
                                  ),
                                  
                                  hr(),
                                  
                                  # Conditional map display - moved below charts
                                  conditionalPanel(
                                    condition = "input.ranking_spatial_type != 'full'",
                                    fluidRow(
                                      column(12,
                                             h4("Dominant Rotation Map"),
                                             p(class = "text-muted",
                                               "The map shows the dominant crop rotation pattern for each area (district or catchment). ",
                                               "If available, soil potential data (Bodenschätzung) is overlaid as a semi-transparent layer to visualize ",
                                               "the relationship between soil quality and rotation patterns. ",
                                               "Use the \"Download Map (Shapefile)\" button in the sidebar to export this map, including the soil potential ",
                                               "column (if available) and a settings file describing the analysis parameters used."
                                             ),
                                             div(
                                               style = "background-color: #f8f9fa; padding: 10px; margin-bottom: 15px; border-left: 4px solid #007bff;",
                                               strong("Soil Potential Layer Source: "),
                                               "Bodenschätzung (German soil assessment) data provides information about soil quality and agricultural potential. ",
                                               "This layer can be toggled on/off using the layer control in the top-right corner of the map."
                                             ),
                                             leafletOutput("ranking_spatial_map", height = "1000px")
                                      )
                                    )
                                  )
                           )
                         ),
                         br(), br(), br()
                ),
                # Rotation Analysis Tab - Monoculture Detection & Pattern Clustering
                tabPanel("6. Rotation Analysis",
                         br(),
                         fluidRow(
                           column(width = 6,
                                  style = "margin-top: -30px; margin-bottom: 0px;",
                                  h2("Rotation Pattern Analysis"),
                                  p("Detect monocultures and cluster similar rotation patterns")
                           ),
                           column(3),
                           column(width = 2, style = "margin-top: 2vh;", tags$img(src = "www/GeoECO_MLU_LANG_2.png", height = "70px", width = "265px"))
                         ),
                         br(),
                         fluidRow(
                           # Sidebar controls
                           column(width = 3,
                                  wellPanel(
                                    h4("Analysis Settings"),

                                    radioButtons(
                                      "analysis_spatial_type",
                                      "Spatial Scope:",
                                      choices = c("Full Area" = "full",
                                                  "District" = "district",
                                                  "Catchment/Basin" = "catchment",
                                                  "Custom Area" = "aoi"),
                                      selected = "full"
                                    ),

                                    conditionalPanel(
                                      condition = "input.analysis_spatial_type == 'district'",
                                      uiOutput("analysis_district_ui")
                                    ),

                                    conditionalPanel(
                                      condition = "input.analysis_spatial_type == 'catchment'",
                                      uiOutput("analysis_catchment_ui")
                                    ),

                                    conditionalPanel(
                                      condition = "input.analysis_spatial_type == 'aoi'",
                                      uiOutput("analysis_aoi_ui")
                                    ),

                                    hr(),

                                    fluidRow(
                                      column(9, h4("Clustering Parameters")),
                                      column(3,
                                             bsButton("cluster-params-info",
                                                      label = "",
                                                      icon = icon("info"),
                                                      style = "default", size = "extra-small"),
                                             bsPopover(
                                               id = "cluster-params-info",
                                               title = "Clustering Parameters",
                                               content = paste(
                                                 "<b>Number of Clusters:</b><br>",
                                                 "How many groups to divide patterns into. More clusters = finer distinctions.<br><br>",
                                                 "<b>Min. Cluster Area:</b><br>",
                                                 "Only include rotation patterns with at least this much area. Filters out rare/small patterns for cleaner results."
                                               ),
                                               placement = "right",
                                               trigger = "hover",
                                               options = list(container = 'body', html = TRUE)
                                             )
                                      )
                                    ),

                                    sliderInput(
                                      "n_clusters",
                                      "Number of Clusters:",
                                      min = 2,
                                      max = 15,
                                      value = 5,
                                      step = 1
                                    ),

                                    sliderInput(
                                      "min_cluster_area",
                                      "Min. Cluster Area (km²):",
                                      min = 0,
                                      max = 100,
                                      value = 1,
                                      step = 1
                                    ),

                                    hr(),

                                    h4("Crop Filters"),

                                    uiOutput("analysis_exclude_crops_ui"),

                                    uiOutput("analysis_include_crops_ui"),

                                    hr(),

                                    actionButton(
                                      "run_rotation_analysis",
                                      "Run Analysis",
                                      class = "btn-primary btn-block",
                                      icon = icon("search")
                                    ),

                                    hr(),

                                    downloadButton(
                                      "download_monoculture_data",
                                      "Download Monoculture Data",
                                      style = "margin-right: 10px; margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)",
                                      class = "btn-success btn-block"
                                    ),
                                    br(),
                                    downloadButton(
                                      "download_cluster_data",
                                      "Download Cluster Data",
                                      style = "margin-right: 10px; margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)",
                                      class = "btn-info btn-block"
                                    )
                                  )
                           ),

                           # Main content area
                           column(width = 9,
                                  # Summary Metrics Row
                                  fluidRow(
                                    column(3,
                                           div(class = "metric-box",
                                               div(class = "metric-value", textOutput("total_fields_analyzed")),
                                               div(class = "metric-label", "Total Fields")
                                           )
                                    ),
                                    column(3,
                                           div(class = "metric-box",
                                               div(class = "metric-value", textOutput("monoculture_count")),
                                               div(class = "metric-label", "Monoculture Fields")
                                           )
                                    ),
                                    column(3,
                                           div(class = "metric-box",
                                               div(class = "metric-value", textOutput("monoculture_percentage")),
                                               div(class = "metric-label", "Monoculture Area %")
                                           )
                                    ),
                                    column(3,
                                           div(class = "metric-box",
                                               div(class = "metric-value", textOutput("monoculture_area")),
                                               div(class = "metric-label", "Monoculture Area (km²)")
                                           )
                                    )
                                  ),

                                  # Tabs for different analysis views
                                  tabsetPanel(
                                    type = "tabs",
                                    id = "analysis_tabs",

                                    tabPanel(
                                      "Monoculture Detection",
                                      icon = icon("seedling"),
                                      br(),
                                      fluidRow(
                                        column(11, h4("Monoculture Summary by Crop")),
                                        column(1,
                                               bsButton("monoculture-info",
                                                        label = "",
                                                        icon = icon("info"),
                                                        style = "default", size = "extra-small"),
                                               bsPopover(
                                                 id = "monoculture-info",
                                                 title = "Monoculture Detection",
                                                 content = paste(
                                                   "<b>What it detects:</b> Fields where the SAME crop is grown every year (zero transitions).<br><br>",
                                                   "<b>Why it matters:</b> Continuous monoculture can lead to:<br>",
                                                   "- Soil degradation<br>",
                                                   "- Pest/disease buildup<br>",
                                                   "- Reduced biodiversity<br><br>",
                                                   "<b>Metrics shown:</b><br>",
                                                   "- <b>Monoculture Fields:</b> number of field polygons with zero crop transitions<br>",
                                                   "- <b>Monoculture Area %:</b> share of total analysed area (km\u00b2) under monoculture &mdash; area-weighted, so large fields count more than small ones<br>",
                                                   "- <b>Monoculture Area (km\u00b2):</b> absolute area under monoculture<br>",
                                                   "- Breakdown by dominant crop type"
                                                 ),
                                                 placement = "left",
                                                 trigger = "hover",
                                                 options = list(container = 'body', html = TRUE)
                                               )
                                        )
                                      ),
                                      p(class = "text-muted",
                                        "Monocultures are fields where the same crop is grown continuously across all analyzed years (zero transitions)."),
                                      br(),
                                      fluidRow(
                                        column(6,
                                               shinycssloaders::withSpinner(
                                                 plotlyOutput("monoculture_bar_chart", height = "400px")
                                               )
                                        ),
                                        column(6,
                                               shinycssloaders::withSpinner(
                                                 plotlyOutput("monoculture_pie_chart", height = "400px")
                                               )
                                        )
                                      ),
                                      br(),
                                      h4("Monoculture Details"),
                                      shinycssloaders::withSpinner(
                                        DT::dataTableOutput("monoculture_table")
                                      )
                                    ),

                                    tabPanel(
                                      "Pattern Clustering",
                                      icon = icon("project-diagram"),
                                      br(),
                                      fluidRow(
                                        column(11, h4("Rotation Pattern Clusters")),
                                        column(1,
                                               bsButton("clustering-info",
                                                        label = "",
                                                        icon = icon("info"),
                                                        style = "default", size = "extra-small"),
                                               bsPopover(
                                                 id = "clustering-info",
                                                 title = "Pattern Clustering",
                                                 content = paste(
                                                   "<b>What it does:</b> Groups rotation patterns by structural diversity using hierarchical clustering.<br><br>",
                                                   "<b>Clustering features:</b><br>",
                                                   "- <b>Number of transitions</b> (crop changes)<br>",
                                                   "- <b>Number of unique crops</b><br><br>",
                                                   "<b>Interpretation categories:</b><br>",
                                                   "- <i>Monoculture:</i> ~0 trans, 1 crop<br>",
                                                   "- <i>Very low diversity:</i> <2 trans, <=2 crops<br>",
                                                   "- <i>Low diversity:</i> <4 trans, <=3 crops<br>",
                                                   "- <i>Medium diversity:</i> <6 trans, <=4 crops<br>",
                                                   "- <i>High diversity:</i> 6+ trans, 5+ crops<br><br>",
                                                   "<b>Scatter plot:</b> Shows cluster separation in the 2D feature space"
                                                 ),
                                                 placement = "left",
                                                 trigger = "hover",
                                                 options = list(container = 'body', html = TRUE)
                                               )
                                        )
                                      ),
                                      p(class = "text-muted",
                                        "Rotation patterns are clustered based on structural diversity: number of transitions and unique crops."),
                                      br(),
                                      fluidRow(
                                        column(6,
                                               shinycssloaders::withSpinner(
                                                 plotlyOutput("cluster_distribution_chart", height = "400px")
                                               )
                                        ),
                                        column(6,
                                               shinycssloaders::withSpinner(
                                                 plotlyOutput("cluster_area_chart", height = "400px")
                                               )
                                        )
                                      ),
                                      br(),
                                      fluidRow(
                                        column(11, h4("Cluster Separability")),
                                        column(1,
                                               bsButton("separability-info",
                                                        label = "",
                                                        icon = icon("info"),
                                                        style = "default", size = "extra-small"),
                                               bsPopover(
                                                 id = "separability-info",
                                                 title = "Cluster Separability Plot",
                                                 content = paste(
                                                   "<b>What it shows:</b> How well clusters are separated in feature space.<br><br>",
                                                   "<b>Axes:</b><br>",
                                                   "- X: Number of transitions (rotation diversity)<br>",
                                                   "- Y: Number of unique crops<br><br>",
                                                   "<b>Points:</b> Each point is a rotation pattern<br>",
                                                   "<b>Color:</b> Cluster assignment<br>",
                                                   "<b>Size:</b> Area of that pattern<br><br>",
                                                   "<b>Good clustering:</b> Points of same color are grouped together<br>",
                                                   "<b>Poor clustering:</b> Colors are mixed/overlapping"
                                                 ),
                                                 placement = "left",
                                                 trigger = "hover",
                                                 options = list(container = 'body', html = TRUE)
                                               )
                                        )
                                      ),
                                      p(class = "text-muted",
                                        "Scatter plot showing how well clusters separate based on transitions and crop diversity."),
                                      shinycssloaders::withSpinner(
                                        plotlyOutput("cluster_scatter_plot", height = "500px")
                                      ),
                                      br(),
                                      h4("Cluster Details"),
                                      shinycssloaders::withSpinner(
                                        DT::dataTableOutput("cluster_table")
                                      ),
                                      br(),
                                      h4("Top Patterns per Cluster"),
                                      shinycssloaders::withSpinner(
                                        DT::dataTableOutput("cluster_patterns_table")
                                      )
                                    ),

                                    tabPanel(
                                      "Transition Analysis",
                                      icon = icon("exchange-alt"),
                                      br(),
                                      fluidRow(
                                        column(11, h4("Transition Distribution")),
                                        column(1,
                                               bsButton("transition-info",
                                                        label = "",
                                                        icon = icon("info"),
                                                        style = "default", size = "extra-small"),
                                               bsPopover(
                                                 id = "transition-info",
                                                 title = "Transition Analysis",
                                                 content = paste(
                                                   "<b>What it measures:</b> Number of crop changes between consecutive years per field.<br><br>",
                                                   "<b>Example (7 years):</b><br>",
                                                   "wheat-wheat-wheat-wheat = 0 transitions<br>",
                                                   "wheat-barley-wheat-barley = 3 transitions<br>",
                                                   "wheat-rapeseed-barley-maize = 3 transitions<br><br>",
                                                   "<b>Categories:</b><br>",
                                                   "- <i>0 transitions:</i> Monoculture<br>",
                                                   "- <i>1 transition:</i> Low rotation<br>",
                                                   "- <i>2 transitions:</i> Medium rotation<br>",
                                                   "- <i>3+ transitions:</i> High rotation<br><br>",
                                                   "<b>Interpretation:</b><br>",
                                                   "Higher transitions = more diverse rotations = generally better for soil health"
                                                 ),
                                                 placement = "left",
                                                 trigger = "hover",
                                                 options = list(container = 'body', html = TRUE)
                                               )
                                        )
                                      ),
                                      p(class = "text-muted",
                                        "Distribution of the number of crop transitions (changes) across all fields."),
                                      br(),
                                      fluidRow(
                                        column(10,
                                               shinycssloaders::withSpinner(
                                                 plotlyOutput("transition_histogram", height = "400px")
                                               )
                                        ),
                                        column(2,
                                               div(style = "margin-top: 50px;",
                                                   h5("Transition Statistics"),
                                                   tableOutput("transition_stats_table")
                                               )
                                        )
                                      ),
                                      br(),
                                      h4("Transition Categories"),
                                      shinycssloaders::withSpinner(
                                        DT::dataTableOutput("transition_categories_table")
                                      )
                                    ),

                                    tabPanel(
                                      "Network Analysis",
                                      icon = icon("share-alt"),
                                      br(),
                                      fluidRow(
                                        column(11, h4("Crop Rotation Network")),
                                        column(1,
                                               bsButton("network-info",
                                                        label = "", icon = icon("info"),
                                                        style = "default", size = "extra-small"),
                                               bsPopover(
                                                 id = "network-info",
                                                 title = "Crop Rotation Network",
                                                 content = paste(
                                                   "<b>Method:</b> Network analysis of crop-to-crop transitions, following the approach of Nowak et al. (2022).<br><br>",
                                                   "<b>Nodes</b> are crops; <b>edges</b> are transitions from one crop to the next on the same field, weighted by area. Crops are arranged on a spiral by importance (total involved area) -- the most important crop sits closest to the center, tapering smoothly outward, with no hard boundary between \"important\" and \"minor\" crops. Dotted loops show monoculture continuation (a crop following itself).<br><br>",
                                                   "<b>Density:</b> share of all possible crop-to-*different*-crop transitions actually observed.<br>",
                                                   "<b>Precedents per crop:</b> mean number of distinct crops that can precede each crop (in-degree).<br>",
                                                   "<b>Return time:</b> simulated number of years before a chosen crop reappears on the same field, drawn from the observed transition probabilities (100 simulated 100-year rotations).<br><br>",
                                                   "<b>Reference:</b> Nowak, B., Michaud, A., Marliac, G. (2022). Assessment of the diversity of crop rotations based on network analysis indicators. <i>Agricultural Systems</i>, 199, 103402. <a href='https://doi.org/10.1016/j.agsy.2022.103402' target='_blank'>doi.org/10.1016/j.agsy.2022.103402</a>"
                                                 ),
                                                 placement = "left",
                                                 trigger = "hover",
                                                 options = list(container = 'body', html = TRUE)
                                               )
                                        )
                                      ),
                                      p(class = "text-muted",
                                        "Uses the same spatial scope and crop filters as the sidebar on the left. Click \"Run Analysis\" again after changing them."),
                                      br(),
                                      fluidRow(
                                        column(3,
                                               sliderInput("network_min_share",
                                                           "Minimum edge share to display (%):",
                                                           min = 0, max = 5, value = 0.5, step = 0.1)
                                        ),
                                        column(3, style = "margin-top: 25px;",
                                               checkboxInput("network_show_selfloops",
                                                             "Show monoculture loops", value = TRUE)
                                        ),
                                        column(3,
                                               uiOutput("network_focus_crop_ui")
                                        ),
                                        column(3, style = "margin-top: 25px;",
                                               actionButton("run_return_time_sim", "Simulate Return Time",
                                                            class = "btn-info", icon = icon("dice"))
                                        )
                                      ),
                                      br(),
                                      fluidRow(
                                        column(3,
                                               div(class = "metric-box",
                                                   div(class = "metric-value", textOutput("network_density_metric")),
                                                   div(class = "metric-label", "Network Density")
                                               )
                                        ),
                                        column(3,
                                               div(class = "metric-box",
                                                   div(class = "metric-value", textOutput("network_precedents_metric")),
                                                   div(class = "metric-label", "Mean Precedents / Crop")
                                               )
                                        ),
                                        column(3,
                                               div(class = "metric-box",
                                                   div(class = "metric-value", textOutput("network_nodes_metric")),
                                                   div(class = "metric-label", "Crops (Nodes)")
                                               )
                                        ),
                                        column(3,
                                               div(class = "metric-box",
                                                   div(class = "metric-value", textOutput("network_edges_metric")),
                                                   div(class = "metric-label", "Transition Types (Edges)")
                                               )
                                        )
                                      ),
                                      br(),
                                      fluidRow(
                                        column(8,
                                               shinycssloaders::withSpinner(
                                                 plotlyOutput("rotation_network_plot", height = "600px")
                                               )
                                        ),
                                        column(4,
                                               fluidRow(
                                                 column(9, h5("Return Time")),
                                                 column(3,
                                                        bsButton("return-time-info",
                                                                 label = "", icon = icon("info"),
                                                                 style = "default", size = "extra-small"),
                                                        bsPopover(
                                                          id = "return-time-info",
                                                          title = "Return Time Simulation",
                                                          content = paste(
                                                            "<b>Method:</b> Simulated Markov-chain rotation (100 runs of 100 years each): starting from the selected crop, each subsequent year's crop is drawn at random from the observed area-weighted transition probabilities. The gaps between successive appearances of the crop are pooled across all runs; the median is shown as \"Median Years to Return\".<br><br>",
                                                            "This is simulated, not directly observed -- your uploaded data typically spans only a handful of years, not enough to measure a real multi-year return time. Simulating from the observed transition probabilities is the same workaround used in the reference study below.<br><br>",
                                                            "There, the simulated return time of soft winter wheat was found to be strongly negatively correlated with phytosanitary product use: shorter return times (wheat returning sooner) came with higher pesticide use.<br><br>",
                                                            "<b>Reference:</b> Nowak, B., Michaud, A., Marliac, G. (2022). Assessment of the diversity of crop rotations based on network analysis indicators. <i>Agricultural Systems</i>, 199, 103402. <a href='https://doi.org/10.1016/j.agsy.2022.103402' target='_blank'>doi.org/10.1016/j.agsy.2022.103402</a>"
                                                          ),
                                                          placement = "left",
                                                          trigger = "hover",
                                                          options = list(container = 'body', html = TRUE)
                                                        )
                                                 )
                                               ),
                                               div(class = "metric-box",
                                                   div(class = "metric-value", textOutput("return_time_median")),
                                                   div(class = "metric-label", "Median Years to Return")
                                               ),
                                               br(),
                                               shinycssloaders::withSpinner(
                                                 plotlyOutput("return_time_histogram", height = "300px")
                                               )
                                        )
                                      ),
                                      br(),
                                      h4("Precedents per Crop"),
                                      shinycssloaders::withSpinner(
                                        DT::dataTableOutput("network_precedents_table")
                                      ),
                                      br(),
                                      downloadButton(
                                        "download_network_edges",
                                        "Download Network Edge Data",
                                        style = "margin-right: 10px; margin-top: 10px; color: #fff; background-color: rgb(0,86,157); border-color: rgb(0,86,157)",
                                        class = "btn-success"
                                      )
                                    )
                                  )
                           )
                         ),
                         br(), br(), br()
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
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column plotOutput uiOutput renderUI verbatimTextOutput fileInput actionButton wellPanel
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
#' @importFrom shiny fluidPage tabsetPanel tabPanel fluidRow column uiOutput renderUI verbatimTextOutput fileInput actionButton wellPanel
#' @import data.table
#' @import ggalluvial
#' @importFrom RColorBrewer brewer.pal
#' @examples
#' \dontrun{
#' # Run the complete Shiny application
#' shinyApp(ui = ui, server = viz_server)
#' }
#' 
#' Extract rotation sequences of specific length for ranking analysis
#'
#' @param data Data frame with Aggregated_YYYY columns and freq column
#' @param rotation_length Number of years in the rotation
#' @param start_year First year of the rotation
#' @param end_year Last year of the rotation
#' @param min_area Minimum area filter for aggregated rotations (in km²)
#' @param max_area Maximum area filter for aggregated rotations (in km²)
#' @param excluded_crops Vector of crop names to exclude
#' @return Data frame with unique rotations and their areas
extract_rotations_ranking <- function(data, rotation_length, start_year, end_year,
                                      min_area = NULL, max_area = NULL, excluded_crops = NULL,
                                      specific_crop = NULL) {
  
  # Find Aggregated_YYYY or Name_YYYY columns
  crop_cols <- grep("^(Aggregated|Name)_[0-9]{4}$", names(data), value = TRUE)
  
  if (length(crop_cols) == 0) {
    return(data.frame(
      rotation = character(0),
      area_ha = numeric(0),
      n_fields = integer(0),
      percentage = numeric(0)
    ))
  }
  
  # Extract years from column names
  col_years <- as.numeric(gsub("^(Aggregated|Name)_", "", crop_cols))
  
  # Sort columns by year
  sorted_indices <- order(col_years)
  crop_cols <- crop_cols[sorted_indices]
  col_years <- col_years[sorted_indices]
  
  # Get area column (freq is already in ha)
  area_col <- "freq"
  if (!area_col %in% names(data)) {
    return(data.frame(
      rotation = character(0),
      area_ha = numeric(0),
      n_fields = integer(0),
      percentage = numeric(0)
    ))
  }
  
  # Select only the relevant year columns using sliding window approach
  if (!is.null(start_year) && !is.null(end_year)) {
    year_range <- start_year:end_year
    
    # Filter columns to those within the year range
    matching_indices <- which(col_years %in% year_range)
    
    if (length(matching_indices) > 0) {
      crop_cols <- crop_cols[matching_indices]
      col_years <- col_years[matching_indices]
    } else {
      return(data.frame(
        rotation = character(0),
        area_ha = numeric(0),
        n_fields = integer(0),
        percentage = numeric(0)
      ))
    }
  }
  
  # Check if we have enough years for the rotation length
  if (length(crop_cols) < rotation_length) {
    return(data.frame(
      rotation = character(0),
      area_ha = numeric(0),
      n_fields = integer(0),
      percentage = numeric(0)
    ))
  }
  
  # Create rotation sequences using sliding window
  rotation_sequences <- list()
  rotation_areas <- list()
  
  for (i in 1:(length(crop_cols) - rotation_length + 1)) {
    seq_cols <- crop_cols[i:(i + rotation_length - 1)]
    
    # Build rotation strings from the selected columns
    rotation_str <- apply(data[, ..seq_cols, drop = FALSE], 1, function(x) {
      paste(x, collapse = " -> ")
    })
    
    rotation_sequences[[i]] <- rotation_str
    rotation_areas[[i]] <- data[[area_col]]
  }
  
  # Combine all sequences
  all_rotations <- unlist(rotation_sequences)
  all_areas <- unlist(rotation_areas)
  
  # Aggregate by unique rotation
  rotation_summary <- data.frame(
    rotation = all_rotations,
    area_ha = all_areas,
    stringsAsFactors = FALSE
  ) %>%
    group_by(rotation) %>%
    summarise(
      area_ha = sum(area_ha, na.rm = TRUE),
      n_fields = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(area_ha)) %>%
    mutate(
      percentage = round(area_ha / sum(area_ha) * 100, 2),
      rank = row_number()
    )

  # Apply area filters to aggregated rotations
  if (!is.null(min_area)) {
    rotation_summary <- rotation_summary %>%
      filter(area_ha >= min_area)
  }
  if (!is.null(max_area)) {
    rotation_summary <- rotation_summary %>%
      filter(area_ha <= max_area)
  }

  # Filter out rotations containing excluded crops
  if (!is.null(excluded_crops) && length(excluded_crops) > 0) {
    # Create regex pattern to match any of the excluded crops
    exclude_pattern <- paste(excluded_crops, collapse = "|")
    rotation_summary <- rotation_summary %>%
      filter(!grepl(exclude_pattern, rotation, ignore.case = TRUE))
  }
  
  # Filter for rotations containing specific crop(s)
  # All specified crops must be present in the rotation
  if (!is.null(specific_crop) && length(specific_crop) > 0) {
    # For each required crop, check if it's present in the rotation
    for (crop in specific_crop) {
      rotation_summary <- rotation_summary %>%
        filter(grepl(crop, rotation, ignore.case = TRUE))
    }
  }
  
  # Recalculate percentages and ranks after filtering
  if (nrow(rotation_summary) > 0) {
    rotation_summary <- rotation_summary %>%
      mutate(
        percentage = round(area_ha / sum(area_ha) * 100, 2),
        rank = row_number()
      )
  }
  
  return(rotation_summary)
}


#' @export
viz_server <- function(input, output, session, app_data, input_dir, crop_colors = NULL) {
  # Normalize path separators (accept both / and \)
  if (!is.na(input_dir)) input_dir <- normalizePath(input_dir, winslash = "/", mustWork = FALSE)

  options(warn = -1)
  
  all_wrap_number_count_small <- app_data$Input_App_data$all_wrap_number_count_small
  loaded_env <- app_data$Input_App_data$loaded_env
  
  options(shiny.maxRequestSize=1024^3 )
  # Reactive value to track if data is loaded
  data_loaded <- reactiveVal(FALSE)
  
  # Render dynamic UI based on whether data is loaded
  output$dynamic_ui <- renderUI({
    if (!data_loaded() & (is.null(input_dir) || is.na(input_dir))) {
      
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
    } else if (!is.null(input_dir) && !is.na(input_dir)){
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
    data.table::set(CropRotViz_intersection, j = "id", value = seq_len(nrow(CropRotViz_intersection)))
    
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
    AOI_choices      <- if (exists("AOI_CropRotViz_intersection") &&
                             !is.null(AOI_CropRotViz_intersection) &&
                             !identical(AOI_CropRotViz_intersection, NA))
                          names(AOI_CropRotViz_intersection) else character(0)
    
    if(language == "English"){
      crop_color_mapping <- app_data$Input_App_data$crop_color_mapping$en
    }else{
      crop_color_mapping <- app_data$Input_App_data$crop_color_mapping$de
    }
    # Merge user-supplied colors: user values take precedence over the built-in palette
    if (!is.null(crop_colors) && length(crop_colors) > 0) {
      if (is.null(crop_color_mapping)) {
        crop_color_mapping <- crop_colors
      } else {
        crop_color_mapping[names(crop_colors)] <- crop_colors
      }
    }
    #--------------------------------------------------------------------------------------------
    
    # Update the pickerInput Crops_sec
    shinyWidgets::updatePickerInput(
      session,
      inputId = "Crops_sec",
      choices = setNames(as.character(Crop_choices), Crop_choices),
      selected = Crop_choices
    )
    
    # Update the pickerInput Crop_spec
    shinyWidgets::updatePickerInput(
      session,
      inputId = "Crop_spec",
      choices = setNames(as.character(Crop_choices), Crop_choices),
      selected = Crop_choices[7]
    )
    
    # Update the pickerInput Crops_kreis
    shinyWidgets::updatePickerInput(
      session,
      inputId = "Crops_kreis",
      choices = setNames(as.character(Crop_choices), Crop_choices),
      selected = Crop_choices
    )
    
    # Update the pickerInput District_sel
    shinyWidgets::updatePickerInput(
      session,
      inputId = "District_sel",
      choices = setNames(as.character(District_choices), District_choices),
      selected = District_choices[1]
    )
    
    # Update the pickerInput EZG_sel
    shinyWidgets::updatePickerInput(
      session,
      inputId = "EZG_sel",
      choices = setNames(as.character(EZG_choices), EZG_choices),
      selected = EZG_choices[1]
    )

    # Update the pickerInput AOI_sel
    if (length(AOI_choices) > 0) {
      shinyWidgets::updatePickerInput(
        session,
        inputId = "AOI_sel",
        choices = setNames(as.character(AOI_choices), AOI_choices),
        selected = AOI_choices[1]
      )
    }

    # Update the sliderInput year_select
    updateSliderInput(
      session,
      inputId = "year_select",
      min = min(years),
      max = max(years)
    )

    # Update the sliderInput year_select_spec with available years
    crop_cols_spec <- grep("^(Aggregated_|Name_)[0-9]{4}$", names(CropRotViz_intersection), value = TRUE)
    if (length(crop_cols_spec) > 0) {
      available_years_spec <- sort(unique(as.numeric(gsub("^(Aggregated_|Name_)", "", crop_cols_spec))))
      if (length(available_years_spec) > 0) {
        updateSliderInput(
          session,
          inputId = "year_select_spec",
          min = min(available_years_spec),
          max = max(available_years_spec),
          value = min(available_years_spec)
        )
      }
    }
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
      }else if(input$tabs == "Plot Diversity"){
        text_1 <- "Plot Crop Diversity Map"
        text_2 <- "Here you can plot and inspect the structural crop diversity per catchment or district. The diversity coloring is defined by the number of transition and the number of unique crops per area."
      }else if(input$tabs == "Rotation Ranking"){
        text_1 <- "Rotation Ranking"
        text_2 <- "Analyze and rank crop rotation patterns by length and year range. Filter by area, crops, and spatial units."
      }else if(input$tabs == "Rotation Analysis"){
        text_1 <- "Rotation Pattern Analysis"
        text_2 <- "Detect monoculture patterns (fields with zero crop transitions) and cluster similar rotation sequences. Use this to identify continuous cropping and group similar rotation strategies."
      } else {
        text_1 <- "Information"
        text_2 <- "Navigate through the tabs to explore different visualization options."
      }
      shinyalert(text_1, text_2, type = "info", confirmButtonCol = "#5d9bd9")
    })
    
    #--------------------------------------------------------------------------------------------
    
    # rotation_data with caching for performance
    rotation_data <- reactiveVal(NULL)
    # Cache for the expensive base grouped rotation summary (by = rotation)
    base_grouped_cache <- reactiveVal(NULL)

    observeEvent(input$update_plot_sequence, {
      req(input$Area_range_sec, input$Crops_sec)

      withProgress(message = 'Updating visualization...', value = 0, {
        incProgress(0.2, detail = "Processing data")

        result <- transform_rotation_data(
          All_rot_big = CropRotViz_intersection,
          distribution_df  = distribution_df,
          input_area_range = input$Area_range_sec,
          choices = Crop_choices,
          selected_crops = input$Crops_sec,
          type = "basic"
        )

        incProgress(0.5, detail = "Computing rotation summary")

        # Compute the expensive base grouped data once and cache it
        base_grouped <- compute_rotation_base_grouped(
          CropRotViz_intersection,
          input$Area_range_sec,
          years
        )
        base_grouped_cache(base_grouped)

        incProgress(0.8, detail = "Finalizing")
        rotation_data(result)
      })
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

      # Always set "Not named" to white color
      if ("Not named" %in% names(color_map)) {
        color_map["Not named"] <- "#FFFFFF"
      }

      return(color_map)
    })
    
    # spec_rotation_data with caching for performance
    spec_rotation_data <- reactiveVal(NULL)
    # Fixed c(min, max) color-scale range for the crop-specific distribution
    # map, computed once per crop selection across every district/catchment,
    # every year, and their averages - see leaflet_crop_spec below.
    crop_spec_color_range <- reactiveVal(NULL)
    # The crop the map/shapefile download currently reflect, frozen at the
    # last "Load Visualization" click (matches the isolate() used in the map
    # render itself, so the export always matches what's on screen).
    crop_spec_selected_crop <- reactiveVal(NULL)

    observeEvent(input$update_crop_specific, {
      req(input$Area_range_spec, input$Crop_spec)

      withProgress(message = 'Updating visualization...', value = 0, {
        incProgress(0.3, detail = "Processing data")

        result <- transform_rotation_data(
          All_rot_big      = CropRotViz_intersection,
          distribution_df  = distribution_df,
          input_area_range = input$Area_range_spec,
          type             = "specific",
          specific_crop    = input$Crop_spec
        )

        incProgress(0.7, detail = "Finalizing")
        spec_rotation_data(result)
        crop_spec_selected_crop(input$Crop_spec)

        crop_spec_color_range(
          compute_crop_percentage_range(
            district_list = if (exists("district_CropRotViz_intersection")) district_CropRotViz_intersection else NULL,
            ezg_list      = if (exists("EZG_CropRotViz_intersection") && is.list(EZG_CropRotViz_intersection)) EZG_CropRotViz_intersection else NULL,
            selected_crop = input$Crop_spec
          )
        )

        # Extract available years from the data and update year slider
        if (!is.null(CropRotViz_intersection) && nrow(CropRotViz_intersection) > 0) {
          crop_cols <- grep("^(Aggregated_|Name_)[0-9]{4}$", names(CropRotViz_intersection), value = TRUE)
          if (length(crop_cols) > 0) {
            available_years <- sort(unique(as.numeric(gsub("^(Aggregated_|Name_)", "", crop_cols))))
            if (length(available_years) > 0) {
              updateSliderInput(session, "year_select_spec",
                              min = min(available_years),
                              max = max(available_years),
                              value = if (input$year_select_spec %in% available_years) {
                                input$year_select_spec
                              } else {
                                min(available_years)
                              })
            }
          }
        }
      })
    })
    
    
    # district_rotation_data with caching for performance
    district_rotation_data <- reactiveVal(NULL)

    observeEvent(input$update_plot_areas, {
      req(input$Area_range_areas, input$Crops_kreis)

      withProgress(message = 'Updating district visualization...', value = 0, {
        incProgress(0.3, detail = "Processing data")

        result <- transform_rotation_data(
          All_rot_big      = district_CropRotViz_intersection,
          distribution_df  = distribution_df,
          choices          = Crop_choices,
          selected_crops   = input$Crops_kreis,
          input_area_range = input$Area_range_areas,
          type             = "district",
          district         = input$District_sel
        )

        incProgress(0.7, detail = "Finalizing")
        district_rotation_data(result)
      })
    })
    
    # basin_rotation_data with caching for performance
    basin_rotation_data <- reactiveVal(NULL)

    observeEvent(input$update_plot_areas, {
      req(input$Area_range_areas, input$Crops_kreis)

      withProgress(message = 'Updating basin visualization...', value = 0, {
        incProgress(0.3, detail = "Processing data")

        result <- transform_rotation_data(
          All_rot_big      = EZG_CropRotViz_intersection,
          distribution_df  = distribution_df,
          choices          = Crop_choices,
          selected_crops   = input$Crops_kreis,
          input_area_range = input$Area_range_areas,
          type             = "basin",
          EZG              = input$EZG_sel
        )

        incProgress(0.7, detail = "Finalizing")
        basin_rotation_data(result)
      })
    })

    # aoi_rotation_data with caching for performance
    aoi_rotation_data <- reactiveVal(NULL)

    observeEvent(input$update_plot_areas, {
      req(input$Area_range_areas, input$Crops_kreis, input$AOI_sel)
      req(exists("AOI_CropRotViz_intersection") && !is.null(AOI_CropRotViz_intersection) &&
          !identical(AOI_CropRotViz_intersection, NA))

      withProgress(message = 'Updating custom area visualization...', value = 0, {
        incProgress(0.3, detail = "Processing data")

        result <- tryCatch(
          transform_rotation_data(
            All_rot_big      = AOI_CropRotViz_intersection,
            distribution_df  = distribution_df,
            choices          = Crop_choices,
            selected_crops   = input$Crops_kreis,
            input_area_range = input$Area_range_areas,
            type             = "aoi",
            AOI_name         = input$AOI_sel
          ),
          error = function(e) { warning(e$message); NULL }
        )

        incProgress(0.7, detail = "Finalizing")
        aoi_rotation_data(result)
      })
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
      Sys.sleep(0.1)
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
      Sys.sleep(0.1)
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
      Sys.sleep(0.1)
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
      Sys.sleep(0.1)
      create_basin_perc_plot()
    }, height = 65, bg = "transparent")

    #--------------------------------------------------------------------------------------------

    # Create reactive expression for the AOI percentage plot
    create_aoi_perc_plot <- reactive({
      req(aoi_rotation_data(), input$AOI_sel)
      req(exists("AOI_CropRotViz_intersection") && !is.null(AOI_CropRotViz_intersection))
      cropping_area_aoi <- sum(AOI_CropRotViz_intersection[[input$AOI_sel]]$freq)

      plot_data <- data.frame(
        Category = c("Selected Cropping Area", "Remaining Cropping Area"),
        Percentage = c((sum(aoi_rotation_data()[[1]]$Area) / length(years) / cropping_area_aoi) * 100,
                       100 - (sum(aoi_rotation_data()[[1]]$Area) / length(years) / cropping_area_aoi) * 100)
      )

      ggplot(plot_data, aes(x = 0.1, y = Percentage, fill = Category)) +
        geom_bar(stat = "identity", width = 10) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")),
                  position = position_stack(vjust = 0.5),
                  color = "white", fontface = "bold", size = 5) +
        coord_flip() +
        scale_fill_manual(values = c("Selected Cropping Area" = "darkgreen", "Remaining Cropping Area" = "darkred")) +
        labs(x = NULL, y = NULL, title = NULL) +
        theme_minimal() +
        theme(
          axis.text.y = element_blank(),
          axis.text.x = element_text(color = "white", size = 15, face = "bold"),
          legend.title = element_blank(),
          legend.text = element_text(color = "white", size = 15, face = "bold"),
          axis.ticks.y = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA)
        ) +
        scale_y_continuous(labels = function(x) paste0(x, "%"))
    })

    output$perc_aoi_plot <- renderPlot({
      Sys.sleep(0.1)
      create_aoi_perc_plot()
    }, height = 65, bg = "transparent")

    #--------------------------------------------------------------------------------------------

    # Create reactive expression for the plot
    create_sankey_plot <- reactive({
      req(rotation_data())
      req(crop_colors())

      text_size <- input$sankey_textsize_sec

      base_plot <- build_sankey_plot(
        rotation_data()[[1]], crop_colors(),
        text_size = text_size, dark = isTRUE(input$sankey_theme_sec)
      )
      
      # Add conditional legend rows
      if (input$Area_range_sec[1] < 1) {
        base_plot <- base_plot + guides(fill = guide_legend(nrow = 9))
      } else {
        base_plot <- base_plot + guides(fill = guide_legend(nrow = 5))
      }
      
      base_plot
    })
    
    # Render the plot
    output$sankey_plot <- renderPlot({
      Sys.sleep(0.1)
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

      text_size <- input$sankey_textsize_spec

      # make basic transition graph
      specific <- build_sankey_plot(
        spec_rotation_data()[[1]], crop_colors(),
        text_size = text_size, dark = isTRUE(input$sankey_theme_spec)
      )

      unique_key_count <- length(unique(spec_rotation_data()[[1]]$key))

      # Apply guides based on the unique count
      if (unique_key_count > 20) {
        specific + guides(fill = guide_legend(nrow = 8))
      } else {
        specific + guides(fill = guide_legend(nrow = 2))
      }
    })
    
    # Render the specific sankey plot
    output$sankey_specific <- renderPlot({
      Sys.sleep(0.1)
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

      text_size <- input$sankey_textsize_district

      # make basic transition graph
      specific <- build_sankey_plot(
        district_rotation_data()[[1]], crop_colors(),
        text_size = text_size, dark = isTRUE(input$sankey_theme_district)
      )

      unique_key_count <- length(unique(district_rotation_data()[[1]]$key))

      # Apply guides based on the unique count
      if (unique_key_count > 20) {
        specific + guides(fill = guide_legend(nrow = 8))
      } else {
        specific + guides(fill = guide_legend(nrow = 2))
      }
    })
    
    # Render the specific sankey plot
    output$sankey_district <- renderPlot({
      Sys.sleep(0.1)
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

      text_size <- input$sankey_textsize_basin

      # make basic transition graph
      specific <- build_sankey_plot(
        basin_rotation_data()[[1]], crop_colors(),
        text_size = text_size, dark = isTRUE(input$sankey_theme_basin)
      )

      unique_key_count <- length(unique(basin_rotation_data()[[1]]$key))

      # Apply guides based on the unique count
      if (unique_key_count > 20) {
        specific + guides(fill = guide_legend(nrow = 8))
      } else {
        specific + guides(fill = guide_legend(nrow = 2))
      }
    })
    
    # Render the specific sankey plot
    output$sankey_basin <- renderPlot({
      Sys.sleep(0.1)
      create_basin_sankey_plot()
    }, height = 800, bg = "transparent")

    #--------------------------------------------------------------------------------------------

    create_aoi_sankey_plot <- reactive({
      req(aoi_rotation_data())
      req(crop_colors())

      text_size <- input$sankey_textsize_aoi

      specific <- build_sankey_plot(
        aoi_rotation_data()[[1]], crop_colors(),
        text_size = text_size, dark = isTRUE(input$sankey_theme_aoi)
      )

      unique_key_count <- length(unique(aoi_rotation_data()[[1]]$key))
      if (unique_key_count > 20) {
        specific + guides(fill = guide_legend(nrow = 8))
      } else {
        specific + guides(fill = guide_legend(nrow = 2))
      }
    })

    output$sankey_aoi <- renderPlot({
      Sys.sleep(0.1)
      create_aoi_sankey_plot()
    }, height = 800, bg = "transparent")

    output$Save_sankey_aoi_plot <- downloadHandler(
      filename = function(file) { "sankey_aoi_plot.png" },
      content = function(file) {
        ggsave(file, plot = create_aoi_sankey_plot(), width = 590, height = 365, units = "mm", device = "png")
      }
    )

    # CSV export for Custom Area
    output$export_aoi_csv <- downloadHandler(
      filename = function() {
        aoi_name <- gsub(" ", "_", input$AOI_sel)
        paste0("aoi_sequences_", aoi_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(aoi_rotation_data())
        export_data <- aoi_rotation_data()[[2]]
        if (inherits(export_data, "sf")) export_data <- sf::st_drop_geometry(export_data)
        write.csv(export_data, file, row.names = FALSE)
      }
    )

    # Excel export for Custom Area
    output$export_aoi_xlsx <- downloadHandler(
      filename = function() {
        aoi_name <- gsub(" ", "_", input$AOI_sel)
        paste0("aoi_sequences_", aoi_name, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        req(aoi_rotation_data())
        export_data <- aoi_rotation_data()[[2]]
        if (inherits(export_data, "sf")) export_data <- sf::st_drop_geometry(export_data)
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(export_data, file, rowNames = FALSE)
        } else if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(export_data, file)
        } else {
          write.csv(export_data, file, row.names = FALSE)
          showNotification("Excel packages not available. File saved as CSV format.", type = "warning")
        }
      }
    )

    output$Save_sankey_basin_plot <- downloadHandler(
      filename = function(file) {
        "sankey_box_plot.png"
      },
      content = function(file) {
        ggsave(file, plot = create_basin_sankey_plot(), width = 590, height = 365, units = "mm", device = "png")
      }
    )

    #--------------------------------------------------------------------------------------------
    # Export handlers for Tab 3 (Plot Sequence per Area)
    #--------------------------------------------------------------------------------------------

    # CSV export for District
    output$export_district_csv <- downloadHandler(
      filename = function() {
        district_name <- gsub(" ", "_", input$District_sel)
        paste0("district_sequences_", district_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(district_rotation_data())

        export_data <- district_rotation_data()[[2]]

        if (inherits(export_data, "sf")) {
          export_data <- sf::st_drop_geometry(export_data)
        }

        write.csv(export_data, file, row.names = FALSE)
      }
    )

    # Excel export for District
    output$export_district_xlsx <- downloadHandler(
      filename = function() {
        district_name <- gsub(" ", "_", input$District_sel)
        paste0("district_sequences_", district_name, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        req(district_rotation_data())

        export_data <- district_rotation_data()[[2]]

        if (inherits(export_data, "sf")) {
          export_data <- sf::st_drop_geometry(export_data)
        }

        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(export_data, file, rowNames = FALSE)
        } else if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(export_data, file)
        } else {
          write.csv(export_data, file, row.names = FALSE)
          showNotification("Excel packages not available. File saved as CSV format.", type = "warning")
        }
      }
    )

    # CSV export for River Basin
    output$export_basin_csv <- downloadHandler(
      filename = function() {
        basin_name <- gsub(" ", "_", input$EZG_sel)
        paste0("basin_sequences_", basin_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(basin_rotation_data())

        export_data <- basin_rotation_data()[[2]]

        if (inherits(export_data, "sf")) {
          export_data <- sf::st_drop_geometry(export_data)
        }

        write.csv(export_data, file, row.names = FALSE)
      }
    )

    # Excel export for River Basin
    output$export_basin_xlsx <- downloadHandler(
      filename = function() {
        basin_name <- gsub(" ", "_", input$EZG_sel)
        paste0("basin_sequences_", basin_name, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        req(basin_rotation_data())

        export_data <- basin_rotation_data()[[2]]

        if (inherits(export_data, "sf")) {
          export_data <- sf::st_drop_geometry(export_data)
        }

        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(export_data, file, rowNames = FALSE)
        } else if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(export_data, file)
        } else {
          write.csv(export_data, file, row.names = FALSE)
          showNotification("Excel packages not available. File saved as CSV format.", type = "warning")
        }
      }
    )

    #--------------------------------------------------------------------------------------------
    # table
    output$table <- renderDT({
      Sys.sleep(0.1)
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
        years = years,
        pre_grouped_data = base_grouped_cache()
      )
      
      # Generate the datatable with dynamic year formatting and performance optimizations
      datatable(All_rot_clean,
                options = list(
                  pageLength = 25,
                  serverSide = FALSE,  # Keep FALSE for now due to styling complexity
                  deferRender = TRUE,  # Render rows only when needed
                  scroller = TRUE,     # Enable virtual scrolling for performance
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
    # Export handlers for Tab 1 (Plot Sequence)
    #--------------------------------------------------------------------------------------------

    # CSV export for Tab 1
    output$export_table_csv <- downloadHandler(
      filename = function() {
        paste0("crop_sequences_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(input$Area_range_sec, input$Crops_sec)

        export_data <- transform_rotation_summary(
          All_rot_big = CropRotViz_intersection,
          area_range = input$Area_range_sec,
          choices = Crop_choices,
          selected_crops = input$Crops_sec,
          max_rows = 10000,
          years = years,
          pre_grouped_data = base_grouped_cache()
        )

        if (inherits(export_data, "sf")) {
          export_data <- sf::st_drop_geometry(export_data)
        }

        write.csv(export_data, file, row.names = FALSE)
      }
    )

    # Excel export for Tab 1
    output$export_table_xlsx <- downloadHandler(
      filename = function() {
        paste0("crop_sequences_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        req(input$Area_range_sec, input$Crops_sec)

        export_data <- transform_rotation_summary(
          All_rot_big = CropRotViz_intersection,
          area_range = input$Area_range_sec,
          choices = Crop_choices,
          selected_crops = input$Crops_sec,
          max_rows = 10000,
          years = years,
          pre_grouped_data = base_grouped_cache()
        )

        if (inherits(export_data, "sf")) {
          export_data <- sf::st_drop_geometry(export_data)
        }

        # Use openxlsx if available, otherwise fall back to CSV with xlsx extension warning
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(export_data, file, rowNames = FALSE)
        } else if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(export_data, file)
        } else {
          # Fallback: save as CSV but warn user
          write.csv(export_data, file, row.names = FALSE)
          showNotification("Excel packages not available. File saved as CSV format.", type = "warning")
        }
      }
    )

    #--------------------------------------------------------------------------------------------

    # Shared reactive: compute unique sequences summary once for table + return time boxplot
    spec_summary_data <- reactive({
      req(spec_rotation_data())
      selected_crop <- isolate(input$Crop_spec)
      area_range <- isolate(input$Area_range_spec)

      transform_rotation_summary(
        All_rot_big = CropRotViz_intersection,
        area_range = area_range,
        specific_crop = selected_crop,
        type = "specific",
        max_rows = Inf,
        years = years
      )
    })

    # table specific
    output$table_spec <- renderDT({
      req(spec_summary_data())
      Sys.sleep(0.1)
      # coloring
      crop_color_mapping_df <- as.data.frame(crop_colors())
      crop_color_mapping_df$crop <- names(crop_colors())
      crop_color_mapping_df <- crop_color_mapping_df[complete.cases(crop_color_mapping_df),]
      names(crop_color_mapping_df) <- c("color", "crop")

      All_rot_clean <- st_drop_geometry(head(spec_summary_data(), 3000))
      
      # Generate datatable with performance optimizations
      datatable(All_rot_clean,
                options = list(
                  pageLength = 25,
                  serverSide = FALSE,  # Keep FALSE for now due to styling complexity
                  deferRender = TRUE,  # Render rows only when needed
                  scroller = TRUE,     # Enable virtual scrolling for performance
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
    # Export handlers for Tab 2 (Plot Crop Specific Sequence)
    #--------------------------------------------------------------------------------------------

    # CSV export for Tab 2
    output$export_table_spec_csv <- downloadHandler(
      filename = function() {
        crop_name <- gsub(" ", "_", input$Crop_spec)
        paste0("crop_specific_sequences_", crop_name, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(input$Area_range_spec, input$Crop_spec)

        export_data <- transform_rotation_summary(
          All_rot_big = CropRotViz_intersection,
          area_range = input$Area_range_spec,
          specific_crop = input$Crop_spec,
          type = "specific",
          max_rows = 10000,
          years = years
        )

        if (inherits(export_data, "sf")) {
          export_data <- sf::st_drop_geometry(export_data)
        }

        write.csv(export_data, file, row.names = FALSE)
      }
    )

    # Excel export for Tab 2
    output$export_table_spec_xlsx <- downloadHandler(
      filename = function() {
        crop_name <- gsub(" ", "_", input$Crop_spec)
        paste0("crop_specific_sequences_", crop_name, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        req(input$Area_range_spec, input$Crop_spec)

        export_data <- transform_rotation_summary(
          All_rot_big = CropRotViz_intersection,
          area_range = input$Area_range_spec,
          specific_crop = input$Crop_spec,
          type = "specific",
          max_rows = 10000,
          years = years
        )

        if (inherits(export_data, "sf")) {
          export_data <- sf::st_drop_geometry(export_data)
        }

        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(export_data, file, rowNames = FALSE)
        } else if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(export_data, file)
        } else {
          write.csv(export_data, file, row.names = FALSE)
          showNotification("Excel packages not available. File saved as CSV format.", type = "warning")
        }
      }
    )

    #--------------------------------------------------------------------------------------------
    observe({
      req(data_loaded())
      req(rotation_data())

      if (any(grepl("Aggregated_", names(rotation_data()[[2]])))) {
        output$sankey_plotly <- renderPlotly({
          Sys.sleep(0.1)
          
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
    # Transition Probability Heatmap (Tab 1)
    #--------------------------------------------------------------------------------------------
    transition_matrix_data <- reactive({
      req(data_loaded())
      req(rotation_data())
      compute_transition_matrix(
        data = CropRotViz_intersection,
        area_range = isolate(input$Area_range_sec)
      )
    })

    output$transition_heatmap <- renderPlotly({
      create_transition_heatmap(transition_matrix_data(), crop_colors = crop_colors())
    })

    output$export_heatmap_csv <- downloadHandler(
      filename = function() {
        paste0("crop_transition_probabilities_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        write.csv(transition_matrix_data(), file, row.names = FALSE)
      }
    )

    output$export_heatmap_xlsx <- downloadHandler(
      filename = function() {
        paste0("crop_transition_probabilities_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        export_data <- transition_matrix_data()
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(export_data, file, rowNames = FALSE)
        } else if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(export_data, file)
        } else {
          write.csv(export_data, file, row.names = FALSE)
          showNotification("Excel packages not available. File saved as CSV format.", type = "warning")
        }
      }
    )

    #--------------------------------------------------------------------------------------------
    output$sankey_plotly_specific <- renderPlotly({
      req(spec_rotation_data())
      Sys.sleep(0.1)

      # Get the selected crop from spec_rotation_data to ensure it only updates after button press
      selected_crop <- isolate(input$Crop_spec)
      summarized_transitions <- process_specific_transitions(CropRotViz_intersection, selected_crop)
      
      
      # Get unique node names and their colors
      unique_nodes <- unique(c(summarized_transitions$source, summarized_transitions$target))
      node_colors <- sapply(unique_nodes, function(name) {
        base_name <- gsub(" \\(before\\)| \\(after\\)", "", name)
        if (is.null(crop_colors()[base_name])) "#808080" else crop_colors()[base_name]
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
            text = paste("Crop proportion before and after", selected_crop),
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
      leaflet(options = leafletOptions(zoomSnap = 0.5, zoomDelta = 0.5, wheelPxPerZoomLevel = 120)) %>%
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

      leaflet(options = leafletOptions(zoomSnap = 0.5, zoomDelta = 0.5, wheelPxPerZoomLevel = 120)) %>%
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

    output$leaflet_aoi <- renderLeaflet({
      req(input$AOI_sel)
      req(exists("AOIs") && !is.null(AOIs) && !identical(AOIs, NA) && inherits(AOIs, "sf"))

      AOI_sel_sf <- subset(AOIs, AOI_name == input$AOI_sel)
      if (sf::st_crs(AOI_sel_sf)$epsg != 4326) {
        AOI_sel_sf <- sf::st_transform(AOI_sel_sf, 4326)
      }

      leaflet(options = leafletOptions(zoomSnap = 0.5, zoomDelta = 0.5, wheelPxPerZoomLevel = 120)) %>%
        addTiles() %>%
        addPolygons(
          data = AOI_sel_sf,
          fillColor = "rgb(220,80,0)",
          fillOpacity = 0.3,
          weight = 2,
          color = "rgb(220,80,0)",
          dashArray = "5,5"
        )
    })
    #--------------------------------------------------------------------------------------------

    # Leaflet map for crop-specific area distribution
    output$leaflet_crop_spec <- renderLeaflet({
      req(spec_rotation_data(), crop_spec_color_range(), input$map_type_spec, input$year_mode_spec)

      # Get the selected crop and year mode using isolate to prevent reactive updates
      selected_crop <- isolate(input$Crop_spec)
      year_mode <- input$year_mode_spec
      selected_year <- input$year_select_spec
      
      # Calculate crop area percentages by district or catchment
      if (input$map_type_spec == "districts") {
        # Check if Districts and district_CropRotViz_intersection exist
        if (!exists("Districts") || is.null(Districts) ||
            !exists("district_CropRotViz_intersection") || is.null(district_CropRotViz_intersection)) {
          return(leaflet(options = leafletOptions(zoomSnap = 0.5, zoomDelta = 0.5, wheelPxPerZoomLevel = 120)) %>% addTiles() %>%
                   addControl(html = "<div style='background: white; padding: 10px;'>District data not available</div>",
                              position = "topright"))
        }
        
        spatial_data <- Districts
        data_list <- district_CropRotViz_intersection
        name_col <- "NAME_3"
        
      } else {
        # Catchments/EZGs
        if (!exists("EZGs") || is.null(EZGs) || identical(EZGs, NA) ||
            !exists("EZG_CropRotViz_intersection") || is.null(EZG_CropRotViz_intersection)) {
          return(leaflet(options = leafletOptions(zoomSnap = 0.5, zoomDelta = 0.5, wheelPxPerZoomLevel = 120)) %>% addTiles() %>%
                   addControl(html = "<div style='background: white; padding: 10px;'>Catchment data not available</div>",
                              position = "topright"))
        }
        
        spatial_data <- EZGs
        data_list <- EZG_CropRotViz_intersection
        name_col <- "EZG"
      }
      
      # Transform to WGS84 if needed (Leaflet requires WGS84)
      if (st_crs(spatial_data) != 4326) {
        spatial_data <- st_transform(spatial_data, 4326)
      }
      
      # Calculate crop area percentages for each district/catchment
      crop_percentages <- list()

      for (name in names(data_list)) {
        region_data <- data_list[[name]]

        # Find crop columns (either Aggregated_ or Name_ columns)
        crop_cols <- grep("^(Aggregated_|Name_)[0-9]{4}$", names(region_data), value = TRUE)

        if (length(crop_cols) > 0 && nrow(region_data) > 0) {
          if (year_mode == "annual") {
            # Annual mode: calculate percentage for selected year only
            year_col <- grep(paste0("_(Aggregated_|Name_)", selected_year, "$"), crop_cols, value = TRUE)

            if (length(year_col) == 0) {
              # Try alternative pattern
              year_col <- grep(paste0(selected_year, "$"), crop_cols, value = TRUE)
            }

            if (length(year_col) > 0) {
              # Use first matching column
              col <- year_col[1]

              # Total area for this year (all crops)
              total_area_year <- sum(region_data$freq, na.rm = TRUE)

              # Area with selected crop in this year
              crop_area_year <- sum(region_data$freq[region_data[[col]] == selected_crop], na.rm = TRUE)

              # Calculate percentage for this year
              if (total_area_year > 0) {
                crop_percentages[[name]] <- (crop_area_year / total_area_year) * 100
              } else {
                crop_percentages[[name]] <- 0
              }
            } else {
              crop_percentages[[name]] <- 0
            }
          } else {
            # Average mode: calculate mean percentage across all years
            # For each year, calculate: (area with selected crop) / (total area in that year)
            year_percentages <- sapply(crop_cols, function(col) {
              # Total area for this year (all crops)
              total_area_year <- sum(region_data$freq, na.rm = TRUE)

              # Area with selected crop in this year
              crop_area_year <- sum(region_data$freq[region_data[[col]] == selected_crop], na.rm = TRUE)

              # Calculate percentage for this year
              if (total_area_year > 0) {
                (crop_area_year / total_area_year) * 100
              } else {
                0
              }
            })

            # Calculate mean percentage across all years
            crop_percentages[[name]] <- mean(year_percentages, na.rm = TRUE)
          }
        } else {
          crop_percentages[[name]] <- 0
        }
      }
      
      # Add percentages to spatial data
      spatial_data$crop_percentage <- sapply(spatial_data[[name_col]], function(x) {
        if (x %in% names(crop_percentages)) {
          crop_percentages[[x]]
        } else {
          0
        }
      })
      
      # Create color palette using a range fixed across every district/catchment
      # and every year (not just the values shown in this particular render),
      # so the same color always means the same area percentage and maps stay
      # visually comparable when switching year, district/catchment, or mode.
      color_range <- crop_spec_color_range()
      if (color_range[2] > color_range[1]) {
        legend_domain <- color_range
        pal <- colorNumeric(
          palette = c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c",
                      "#fc4e2a", "#e31a1c", "#bd0026", "#800026"),
          domain = legend_domain,
          na.color = "transparent"
        )
      } else {
        # If this crop never occurs anywhere, use a gray palette
        legend_domain <- c(0, 1)
        pal <- colorNumeric(
          palette = c("#e0e0e0", "#e0e0e0"),
          domain = legend_domain,
          na.color = "transparent"
        )
      }

      # colorNumeric() returns na.color (transparent) for any value outside
      # its domain -- since legend_domain's upper bound is now the 95th
      # percentile (not the true max), districts/catchments above the cap
      # would otherwise render with no fill at all. Clamp the value fed to
      # the palette so anything above the cap just saturates to the darkest
      # color instead of disappearing; the popup still shows the true,
      # unclamped percentage.
      spatial_data$crop_percentage_color <- pmin(pmax(spatial_data$crop_percentage, legend_domain[1]), legend_domain[2])

      # Create popup text with year information
      year_info <- if (year_mode == "annual") {
        paste0("<div style='margin: 5px 0;'>",
               "<span style='color: #34495e;'><strong>Year:</strong></span> ",
               "<span style='color: #34495e; font-weight: bold;'>", selected_year, "</span>",
               "</div>")
      } else {
        paste0("<div style='margin: 5px 0;'>",
               "<span style='color: #34495e;'><strong>Period:</strong></span> ",
               "<span style='color: #34495e; font-weight: bold;'>Complete Average</span>",
               "</div>")
      }

      spatial_data$popup_text <- paste0(
        "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.4; min-width: 200px;'>",
        "<strong style='color: #2c3e50; font-size: 16px;'>", spatial_data[[name_col]], "</strong><br>",
        "<hr style='margin: 8px 0; border: none; border-top: 1px solid #bdc3c7;'>",
        "<div style='margin: 5px 0;'>",
        "<span style='color: #34495e;'><strong>Crop:</strong></span> ",
        "<span style='color: #34495e; font-weight: bold;'>", selected_crop, "</span>",
        "</div>",
        year_info,
        "<div style='margin: 5px 0;'>",
        "<span style='color: #34495e;'><strong>Area Percentage:</strong></span> ",
        "<span style='color: #34495e; font-weight: bold;'>", round(spatial_data$crop_percentage, 2), "%</span>",
        "</div>",
        "</div>"
      )
      
      # Create leaflet map
      map <- leaflet(spatial_data, options = leafletOptions(zoomSnap = 0.5, zoomDelta = 0.5, wheelPxPerZoomLevel = 120)) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(crop_percentage_color),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          popup = ~popup_text,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          # Pass the fixed domain (not ~crop_percentage) so the legend's
          # displayed breaks/labels stay identical across year, district/
          # catchment, and average/annual switches. leaflet::addLegend()
          # derives its cuts from range(values), not from pal's own domain,
          # so handing it the live per-render data would make the legend
          # rescale every time even though fillColor already uses `pal`
          # (and therefore legend_domain) consistently.
          values = legend_domain,
          title = if (year_mode == "annual") {
            paste0(selected_crop, " (", selected_year, ")<br>Area (%)")
          } else {
            paste0(selected_crop, " (Avg)<br>Area (%)")
          },
          opacity = 0.7
        )
      
      return(map)
    })
    #--------------------------------------------------------------------------------------------

    # Shapefile export for the crop-specific area distribution map. Unlike the
    # map itself (which shows either one selected year or the average), the
    # export always includes every year as its own pct_<year> column plus
    # pct_avg, regardless of which display mode is currently selected - the
    # geometry/region set (districts vs catchments) still follows map_type_spec.
    output$crop_spec_download_shp <- downloadHandler(
      filename = function() {
        map_type_label <- if (identical(input$map_type_spec, "districts")) "districts" else "catchments"
        crop_label <- crop_spec_selected_crop()
        crop_label <- if (is.null(crop_label)) "crop" else gsub("[^A-Za-z0-9]+", "_", crop_label)
        paste0("crop_distribution_", map_type_label, "_", crop_label, "_",
               format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      contentType = "application/zip",
      content = function(file) {
        req(crop_spec_selected_crop(), input$map_type_spec)
        selected_crop <- crop_spec_selected_crop()

        if (input$map_type_spec == "districts") {
          req(exists("Districts"), exists("district_CropRotViz_intersection"))
          spatial_data <- Districts
          data_list <- district_CropRotViz_intersection
          name_col <- "NAME_3"
          spatial_label <- "Districts"
        } else {
          req(exists("EZGs"), exists("EZG_CropRotViz_intersection"), is.list(EZG_CropRotViz_intersection))
          spatial_data <- EZGs
          data_list <- EZG_CropRotViz_intersection
          name_col <- "EZG"
          spatial_label <- "Catchments"
        }

        if (st_crs(spatial_data) != 4326) {
          spatial_data <- st_transform(spatial_data, 4326)
        }

        pct_table <- compute_crop_percentage_table(data_list, selected_crop, name_col = name_col)
        export_data <- merge(spatial_data, pct_table, by = name_col, all.x = TRUE)

        export_dir <- tempfile("crop_spec_shp_")
        dir.create(export_dir)
        on.exit(unlink(export_dir, recursive = TRUE), add = TRUE)

        shp_path <- file.path(export_dir, "crop_distribution_map.shp")
        sf::st_write(sf::st_make_valid(export_data), shp_path,
                     driver = "ESRI Shapefile", quiet = TRUE, append = FALSE, delete_layer = TRUE)

        pct_year_cols <- grep("^pct_[0-9]{4}$", names(pct_table), value = TRUE)
        meta_lines <- c(
          "CropRotationViz - Crop-Specific Area Distribution Map Export",
          paste("Export date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "",
          paste("Crop:", selected_crop),
          paste("Spatial extent type:", spatial_label),
          "",
          "Columns:",
          paste0("  ", paste(sort(pct_year_cols), collapse = ", "),
                 " - percentage of total area occupied by the selected crop in that year"),
          "  pct_avg - average of the above across all available years",
          "",
          "All years and the average are included regardless of which single",
          "year or display mode was selected in the app when this was exported."
        )
        writeLines(meta_lines, file.path(export_dir, "export_settings.txt"))

        zip_files <- list.files(export_dir, full.names = FALSE)

        if (requireNamespace("zip", quietly = TRUE)) {
          zip::zip(file, files = zip_files, root = export_dir)
        } else {
          old_wd <- getwd()
          setwd(export_dir)
          on.exit(setwd(old_wd), add = TRUE, after = FALSE)
          utils::zip(file, files = zip_files)
        }
      }
    )
    #--------------------------------------------------------------------------------------------

    output$district_ridges <- renderPlot({
      req(district_rotation_data())
      data <- district_rotation_data()[[2]]
      if (any(grepl("Aggregated_", names(data)))) {
        column <- paste0("Aggregated_" , input$year_select)
        colors <- crop_colors()
      }else{
        column <- paste0("Name_" , input$year_select)
        # Generate deterministic colors based on unique crop names
        unique_crops <- unique(data[[column]])
        colors <- setNames(sapply(unique_crops, generate_hex_color), unique_crops)
        # Always set "Not named" to white
        if ("Not named" %in% names(colors)) {
          colors["Not named"] <- "#FFFFFF"
        }
      }
      
      ggplot(data %>%
               group_by(!!sym(column)) %>%
               filter(n() > 1) %>%
               ungroup(),
             aes(x = area/10000, y = !!sym(column), fill = !!sym(column))) +
        geom_boxplot(alpha = 0.7, width = 0.5) +
        theme_minimal() +
        theme(axis.text = element_text(color = "white", size = 15, face="bold"),
              axis.title.x = element_text(color = "white", size = 16, face="bold", margin = margin(t = 10)),
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
      req(basin_rotation_data())
      data <- basin_rotation_data()[[2]]
      if (any(grepl("Aggregated_", names(data)))) {
        column <- paste0("Aggregated_" , input$year_select)
        colors <- crop_colors()
      }else{
        column <- paste0("Name_" , input$year_select)
        # Generate deterministic colors based on unique crop names
        unique_crops <- unique(data[[column]])
        colors <- setNames(sapply(unique_crops, generate_hex_color), unique_crops)
        # Always set "Not named" to white
        if ("Not named" %in% names(colors)) {
          colors["Not named"] <- "#FFFFFF"
        }
      }
      
      ggplot(data %>%
               group_by(!!sym(column)) %>%
               filter(n() > 1) %>%
               ungroup(),
             aes(x = area/10000, y = !!sym(column), fill = !!sym(column))) +
        geom_boxplot(alpha = 0.7, width = 0.5) +
        theme_minimal() +
        theme(axis.text = element_text(color = "white", size = 15, face="bold"),
              axis.title.x = element_text(color = "white", size = 16, face="bold", margin = margin(t = 10)),
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
      req(district_rotation_data(), crop_colors())
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
      req(basin_rotation_data(), crop_colors())
      data <- basin_rotation_data()[[2]]
      
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
    
    
    # plot diversity maps
    output$diversity <- renderLeaflet({
      if (input$AreaType == "districts") {
        diversity_mapper(data = diversity_data[[1]], type = "District")
      } else if (input$AreaType == "ezg") {
        diversity_mapper(data = diversity_data[[2]], type = "EZG")
      } else if (input$AreaType == "aoi" && length(diversity_data) >= 3 && !is.null(diversity_data[[3]]) && !identical(diversity_data[[3]], NA)) {
        diversity_mapper(data = diversity_data[[3]], type = "AOI_name")
      } else {
        diversity_mapper(data = diversity_data[[1]], type = "District")
      }
    })

    output$diversity_soil <- renderPlotly({
      tryCatch({
        if (input$AreaType == "districts") {
          diversity_soil_plotter(data = diversity_data[[1]], type = "District")
        } else if (input$AreaType == "ezg") {
          diversity_soil_plotter(data = diversity_data[[2]], type = "EZG")
        } else if (input$AreaType == "aoi" && length(diversity_data) >= 3 && !is.null(diversity_data[[3]]) && !identical(diversity_data[[3]], NA)) {
          diversity_soil_plotter(data = diversity_data[[3]], type = "AOI_name")
        } else {
          diversity_soil_plotter(data = diversity_data[[1]], type = "District")
        }
      }, error = function(e) {
        # Return an empty plotly with error message if BS data is invalid
        plotly::plot_ly() %>%
          plotly::layout(
            title = list(text = "Soil Potential Data Unavailable",
                         font = list(size = 16, color = "#d9534f")),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              text = paste("Error:", e$message, "\n\nThis usually means soil potential data is missing or invalid."),
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              xanchor = "center",
              yanchor = "middle",
              showarrow = FALSE,
              font = list(size = 14, color = "#666")
            )
          )
      })
    })

    # CSV export for Diversity data
    output$export_diversity_csv <- downloadHandler(
      filename = function() {
        area_type <- switch(input$AreaType, "districts" = "districts", "ezg" = "catchments", "aoi" = "custom_area", "districts")
        paste0("diversity_soil_potential_", area_type, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        export_data <- if (input$AreaType == "districts") {
          diversity_data[[1]]$BISCALE
        } else if (input$AreaType == "ezg") {
          diversity_data[[2]]$BISCALE
        } else if (input$AreaType == "aoi" && length(diversity_data) >= 3) {
          diversity_data[[3]]$BISCALE
        } else {
          diversity_data[[1]]$BISCALE
        }
        if (inherits(export_data, "sf")) export_data <- sf::st_drop_geometry(export_data)
        write.csv(export_data, file, row.names = FALSE)
      }
    )

    # Excel export for Diversity data
    output$export_diversity_xlsx <- downloadHandler(
      filename = function() {
        area_type <- switch(input$AreaType, "districts" = "districts", "ezg" = "catchments", "aoi" = "custom_area", "districts")
        paste0("diversity_soil_potential_", area_type, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
      },
      content = function(file) {
        export_data <- if (input$AreaType == "districts") {
          diversity_data[[1]]$BISCALE
        } else if (input$AreaType == "ezg") {
          diversity_data[[2]]$BISCALE
        } else if (input$AreaType == "aoi" && length(diversity_data) >= 3) {
          diversity_data[[3]]$BISCALE
        } else {
          diversity_data[[1]]$BISCALE
        }
        if (inherits(export_data, "sf")) {
          export_data <- sf::st_drop_geometry(export_data)
        }

        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(export_data, file, rowNames = FALSE)
        } else if (requireNamespace("writexl", quietly = TRUE)) {
          writexl::write_xlsx(export_data, file)
        } else {
          write.csv(export_data, file, row.names = FALSE)
          showNotification("Excel packages not available. File saved as CSV format.", type = "warning")
        }
      }
    )

    #--------------------------------------------------------------------------------------------
    # Shannon Diversity Index (Tab 4)
    #--------------------------------------------------------------------------------------------
    shannon_data <- reactive({
      req(data_loaded())

      first_elem <- district_CropRotViz_intersection[[1]]
      agg_cols <- grep("^(Aggregated_|Name_)[0-9]{4}$", names(first_elem), value = TRUE)
      req(length(agg_cols) > 0)

      if (input$AreaType == "districts") {
        compute_shannon_diversity(district_CropRotViz_intersection, agg_cols, "District", Districts)
      } else if (input$AreaType == "ezg") {
        compute_shannon_diversity(EZG_CropRotViz_intersection, agg_cols, "EZG", EZGs)
      } else if (input$AreaType == "aoi" &&
                 exists("AOI_CropRotViz_intersection") &&
                 !is.null(AOI_CropRotViz_intersection) &&
                 !identical(AOI_CropRotViz_intersection, NA) &&
                 exists("AOIs") && !is.null(AOIs) && !identical(AOIs, NA)) {
        compute_shannon_diversity(AOI_CropRotViz_intersection, agg_cols, "AOI_name", AOIs)
      } else {
        compute_shannon_diversity(district_CropRotViz_intersection, agg_cols, "District", Districts)
      }
    })

    output$shannon_map <- renderLeaflet({
      req(shannon_data())
      group <- switch(input$AreaType,
                      "districts" = "District",
                      "ezg"       = "EZG",
                      "aoi"       = "AOI_name",
                      "District")
      create_shannon_map(shannon_data(), group_col = group)
    })

    output$shannon_ranking <- renderPlotly({
      req(shannon_data())
      group <- switch(input$AreaType,
                      "districts" = "District",
                      "ezg"       = "EZG",
                      "aoi"       = "AOI_name",
                      "District")
      create_shannon_ranking(shannon_data(), group_col = group)
    })

    # ==============================================================================
    # ROTATION RANKING TAB LOGIC
    # ==============================================================================
    
    # Reactive values for ranking analysis
    ranking_rotation_results <- reactiveVal(NULL)
    ranking_available_years <- reactiveVal(NULL)
    ranking_spatial_label <- reactiveVal("Full Area")
    ranking_map_data <- reactiveVal(NULL)  # Pre-computed spatial data for the dominant rotation map
    
    # Extract available years from CropRotViz_intersection
    observe({
      if (!is.null(CropRotViz_intersection)) {
        crop_cols <- grep("^(Aggregated|Name)_[0-9]{4}$", names(CropRotViz_intersection), value = TRUE)
        years <- as.numeric(gsub("^(Aggregated|Name)_", "", crop_cols))
        ranking_available_years(sort(years))
      }
    })
    
    # Dynamic UI for year range selection
    output$ranking_year_range_ui <- renderUI({
      if (is.null(ranking_available_years())) {
        div(
          p("Please load data first to see available years",
            style = "color: gray; font-style: italic;")
        )
      } else {
        years <- ranking_available_years()
        min_year <- min(years)
        max_year <- max(years)
        
        sliderInput(
          "ranking_year_range",
          "Select Year Range:",
          min = min_year,
          max = max_year,
          value = c(min_year, min(min_year + 2, max_year)),
          step = 1,
          sep = ""
        )
      }
    })
    
    # Reactive expression to calculate area range from aggregated rotations
    # Always uses the full dataset so the slider range stays stable when changing district/catchment
    rotation_area_range <- reactive({
      req(CropRotViz_intersection)
      req(input$ranking_rotation_length)
      req(input$ranking_year_range)

      tryCatch({
        # Calculate rotations without area filter to get the full range
        start_year <- input$ranking_year_range[1]
        end_year <- input$ranking_year_range[2]

        results <- extract_rotations_ranking(
          CropRotViz_intersection,
          input$ranking_rotation_length,
          start_year,
          end_year,
          min_area = NULL,
          max_area = NULL,
          excluded_crops = NULL,
          specific_crop = NULL
        )

        if (nrow(results) > 0) {
          list(
            min = floor(min(results$area_ha, na.rm = TRUE)),
            max = ceiling(max(results$area_ha, na.rm = TRUE))
          )
        } else {
          list(min = 0, max = 100)
        }
      }, error = function(e) {
        list(min = 0, max = 100)
      })
    })

    # Dynamic UI for area filter
    output$ranking_area_filter_ui <- renderUI({
      if (is.null(CropRotViz_intersection)) {
        div(
          p("Load data to enable area filter",
            style = "color: gray; font-style: italic;")
        )
      } else {
        range <- rotation_area_range()

        sliderInput(
          "ranking_area_range",
          "Filter by Area (km²):",
          min = range$min,
          max = range$max,
          value = c(range$min, range$max),
          step = 0.1,
          round = FALSE
        )
      }
    })
    
    # Dynamic UI for crop filter
    output$ranking_crop_filter_ui <- renderUI({
      if (is.null(CropRotViz_intersection)) {
        div(
          p("Load data to enable crop filter",
            style = "color: gray; font-style: italic;")
        )
      } else {
        crop_cols <- grep("^(Aggregated|Name)_[0-9]{4}$", names(CropRotViz_intersection), value = TRUE)
        
        if (length(crop_cols) > 0) {
          all_crops <- unique(unlist(CropRotViz_intersection[, ..crop_cols]))
          all_crops <- sort(all_crops[!is.na(all_crops)])
          
          selectInput(
            "ranking_excluded_crops",
            "Exclude Crops:",
            choices = all_crops,
            multiple = TRUE,
            selectize = TRUE
          )
        } else {
          p("No crop columns found", style = "color: gray; font-style: italic;")
        }
      }
    })
    
    # Dynamic UI for specific crop requirement
    output$ranking_specific_crop_ui <- renderUI({
      if (is.null(CropRotViz_intersection)) {
        div(
          p("Load data to enable specific crop filter",
            style = "color: gray; font-style: italic;")
        )
      } else {
        crop_cols <- grep("^(Aggregated|Name)_[0-9]{4}$", names(CropRotViz_intersection), value = TRUE)
        
        if (length(crop_cols) > 0) {
          all_crops <- unique(unlist(CropRotViz_intersection[, ..crop_cols]))
          all_crops <- sort(all_crops[!is.na(all_crops)])
          
          shinyWidgets::pickerInput(
            "ranking_specific_crop",
            "Require Specific Crop(s):",
            choices = all_crops,
            selected = NULL,
            multiple = TRUE,
            options = shinyWidgets::pickerOptions(
              noneSelectedText = "No specific crop required",
              selectedTextFormat = "count > 2",
              actionsBox = TRUE,
              liveSearch = TRUE
            )
          )
        } else {
          p("No crop columns found", style = "color: gray; font-style: italic;")
        }
      }
    })
    
    # Dynamic UI for spatial filter (district/catchment/AOI)
    output$ranking_spatial_filter_ui <- renderUI({
      has_districts  <- !is.null(district_CropRotViz_intersection) && length(district_CropRotViz_intersection) > 0
      has_catchments <- exists("EZG_CropRotViz_intersection") && !is.null(EZG_CropRotViz_intersection) &&
                        !identical(EZG_CropRotViz_intersection, NA) && length(EZG_CropRotViz_intersection) > 0
      has_aois       <- exists("AOI_CropRotViz_intersection") && !is.null(AOI_CropRotViz_intersection) &&
                        !identical(AOI_CropRotViz_intersection, NA) && length(AOI_CropRotViz_intersection) > 0

      if (!has_districts && !has_catchments && !has_aois) return(NULL)

      tagList(
        br(),
        h5("Spatial Filter (Optional)"),
        radioButtons(
          "ranking_spatial_type",
          "Select Area Type:",
          choices = c("Full Area" = "full",
                      if (has_districts)  "District"        else NULL,
                      if (has_catchments) "Catchment/Basin" else NULL,
                      if (has_aois)       "Custom Area"     else NULL),
          selected = "full",
          inline = TRUE
        ),
        conditionalPanel(
          condition = "input.ranking_spatial_type == 'District'",
          selectInput("ranking_district_sel", "Select District:",
                      choices  = if (has_districts) names(district_CropRotViz_intersection) else NULL,
                      selected = if (has_districts) names(district_CropRotViz_intersection)[1] else NULL)
        ),
        conditionalPanel(
          condition = "input.ranking_spatial_type == 'Catchment/Basin'",
          selectInput("ranking_catchment_sel", "Select Catchment/Basin:",
                      choices  = if (has_catchments) names(EZG_CropRotViz_intersection) else NULL,
                      selected = if (has_catchments) names(EZG_CropRotViz_intersection)[1] else NULL)
        ),
        conditionalPanel(
          condition = "input.ranking_spatial_type == 'Custom Area'",
          selectInput("ranking_aoi_sel", "Select Custom Area:",
                      choices  = if (has_aois) names(AOI_CropRotViz_intersection) else NULL,
                      selected = if (has_aois) names(AOI_CropRotViz_intersection)[1] else NULL)
        )
      )
    })
    
    # Analyze rotations when button is clicked
    observeEvent(input$ranking_analyze_btn, {
      req(CropRotViz_intersection, input$ranking_year_range, input$ranking_rotation_length)
      
      start_year <- input$ranking_year_range[1]
      end_year <- input$ranking_year_range[2]
      
      # Validate year range
      if (start_year >= end_year) {
        showNotification(
          "End year must be greater than start year!",
          type = "error",
          duration = 3
        )
        return()
      }
      
      # Validate rotation length vs year range
      year_span <- end_year - start_year + 1
      if (input$ranking_rotation_length > year_span) {
        showNotification(
          paste("Rotation length (", input$ranking_rotation_length,
                ") cannot exceed year range (", year_span, "years)!",
                "\nPlease adjust the year range slider."),
          type = "error",
          duration = 5
        )
        return()
      }
      
      tryCatch({
        withProgress(message = 'Analyzing rotations...', value = 0, {
          incProgress(0.3, detail = "Extracting rotation sequences")
          
          # Determine which dataset to use based on spatial filter
          data_to_analyze <- CropRotViz_intersection
          spatial_label <- "Full Area"
          
          if (!is.null(input$ranking_spatial_type) && input$ranking_spatial_type != "full") {
            if (input$ranking_spatial_type == "District" && !is.null(input$ranking_district_sel)) {
              data_to_analyze <- district_CropRotViz_intersection[[input$ranking_district_sel]]
              if (!inherits(data_to_analyze, "data.table")) {
                data_to_analyze <- data.table::as.data.table(data_to_analyze)
              }
              spatial_label <- paste("District:", input$ranking_district_sel)
            } else if (input$ranking_spatial_type == "Catchment/Basin" && !is.null(input$ranking_catchment_sel)) {
              data_to_analyze <- EZG_CropRotViz_intersection[[input$ranking_catchment_sel]]
              if (!inherits(data_to_analyze, "data.table")) {
                data_to_analyze <- data.table::as.data.table(data_to_analyze)
              }
              spatial_label <- paste("Catchment:", input$ranking_catchment_sel)
            } else if (input$ranking_spatial_type == "Custom Area" && !is.null(input$ranking_aoi_sel)) {
              data_to_analyze <- AOI_CropRotViz_intersection[[input$ranking_aoi_sel]]
              if (!inherits(data_to_analyze, "data.table")) {
                data_to_analyze <- data.table::as.data.table(data_to_analyze)
              }
              spatial_label <- paste("Custom Area:", input$ranking_aoi_sel)
            }
          }
          
          # Get filter values
          min_area_val <- if (!is.null(input$ranking_area_range)) input$ranking_area_range[1] else NULL
          max_area_val <- if (!is.null(input$ranking_area_range)) input$ranking_area_range[2] else NULL
          excluded_crops_val <- if (!is.null(input$ranking_excluded_crops)) input$ranking_excluded_crops else NULL
          specific_crop_val <- if (!is.null(input$ranking_specific_crop) && length(input$ranking_specific_crop) > 0) input$ranking_specific_crop else NULL
          
          results <- extract_rotations_ranking(
            data_to_analyze,
            input$ranking_rotation_length,
            start_year,
            end_year,
            min_area = min_area_val,
            max_area = max_area_val,
            excluded_crops = excluded_crops_val,
            specific_crop = specific_crop_val
          )
          
          incProgress(0.7, detail = "Calculating statistics")
          
          ranking_rotation_results(results)
          ranking_spatial_label(spatial_label)

          # Pre-compute dominant rotation map data for all districts/catchments
          # This is stored so the map doesn't need to recompute on every input change
          if (!is.null(input$ranking_spatial_type) && input$ranking_spatial_type != "full") {
            tryCatch({
              spatial_data <- NULL
              dominant_rotations <- data.frame()

              if (input$ranking_spatial_type == "District") {
                if (exists("Districts") && !is.null(Districts) && exists("district_CropRotViz_intersection")) {
                  spatial_data <- Districts
                  for (district_name in names(district_CropRotViz_intersection)) {
                    district_data <- district_CropRotViz_intersection[[district_name]]
                    if (!inherits(district_data, "data.table")) {
                      district_data <- data.table::as.data.table(district_data)
                    }
                    dist_results <- extract_rotations_ranking(
                      district_data, input$ranking_rotation_length,
                      input$ranking_year_range[1], input$ranking_year_range[2],
                      min_area = min_area_val, max_area = max_area_val,
                      excluded_crops = excluded_crops_val, specific_crop = specific_crop_val
                    )
                    if (nrow(dist_results) > 0) {
                      dominant_rotations <- rbind(dominant_rotations, data.frame(
                        area_name = district_name,
                        dominant_rotation = dist_results$rotation[1],
                        area_ha = dist_results$area_ha[1],
                        percentage = dist_results$percentage[1],
                        stringsAsFactors = FALSE
                      ))
                    }
                  }
                  if (nrow(dominant_rotations) > 0) {
                    spatial_data <- merge(spatial_data, dominant_rotations,
                                          by.x = "NAME_3", by.y = "area_name", all.x = TRUE)
                  }
                  if (exists("diversity_data") && is.list(diversity_data) && length(diversity_data) >= 1 &&
                      !is.null(diversity_data[[1]]) && is.list(diversity_data[[1]]) &&
                      !is.null(diversity_data[[1]][[1]]) && "BS_mean" %in% names(diversity_data[[1]][[1]])) {
                    bs_data <- sf::st_drop_geometry(diversity_data[[1]][[1]])[, c("District", "BS_mean")]
                    spatial_data <- merge(spatial_data, bs_data, by.x = "NAME_3", by.y = "District", all.x = TRUE)
                  }
                }
              } else if (input$ranking_spatial_type == "Catchment/Basin") {
                if (exists("EZGs") && !is.null(EZGs) && exists("EZG_CropRotViz_intersection")) {
                  spatial_data <- EZGs
                  for (ezg_name in names(EZG_CropRotViz_intersection)) {
                    ezg_data <- EZG_CropRotViz_intersection[[ezg_name]]
                    if (!inherits(ezg_data, "data.table")) {
                      ezg_data <- data.table::as.data.table(ezg_data)
                    }
                    ezg_results <- extract_rotations_ranking(
                      ezg_data, input$ranking_rotation_length,
                      input$ranking_year_range[1], input$ranking_year_range[2],
                      min_area = min_area_val, max_area = max_area_val,
                      excluded_crops = excluded_crops_val, specific_crop = specific_crop_val
                    )
                    if (nrow(ezg_results) > 0) {
                      dominant_rotations <- rbind(dominant_rotations, data.frame(
                        area_name = ezg_name,
                        dominant_rotation = ezg_results$rotation[1],
                        area_ha = ezg_results$area_ha[1],
                        percentage = ezg_results$percentage[1],
                        stringsAsFactors = FALSE
                      ))
                    }
                  }
                  if (nrow(dominant_rotations) > 0) {
                    spatial_data <- merge(spatial_data, dominant_rotations,
                                          by.x = "EZG", by.y = "area_name", all.x = TRUE)
                  }
                  if (exists("diversity_data") && is.list(diversity_data) && length(diversity_data) >= 2 &&
                      !is.null(diversity_data[[2]]) && is.list(diversity_data[[2]]) &&
                      !is.null(diversity_data[[2]][[1]]) && "BS_mean" %in% names(diversity_data[[2]][[1]])) {
                    bs_data <- sf::st_drop_geometry(diversity_data[[2]][[1]])[, c("EZG", "BS_mean")]
                    spatial_data <- merge(spatial_data, bs_data, by = "EZG", all.x = TRUE)
                  }
                }
              } else if (input$ranking_spatial_type == "Custom Area") {
                if (exists("AOIs") && !is.null(AOIs) && !identical(AOIs, NA) &&
                    exists("AOI_CropRotViz_intersection") && !is.null(AOI_CropRotViz_intersection)) {
                  spatial_data <- AOIs
                  for (aoi_name in names(AOI_CropRotViz_intersection)) {
                    aoi_data_r <- AOI_CropRotViz_intersection[[aoi_name]]
                    if (!inherits(aoi_data_r, "data.table")) {
                      aoi_data_r <- data.table::as.data.table(aoi_data_r)
                    }
                    aoi_results <- extract_rotations_ranking(
                      aoi_data_r, input$ranking_rotation_length,
                      input$ranking_year_range[1], input$ranking_year_range[2],
                      min_area = min_area_val, max_area = max_area_val,
                      excluded_crops = excluded_crops_val, specific_crop = specific_crop_val
                    )
                    if (nrow(aoi_results) > 0) {
                      dominant_rotations <- rbind(dominant_rotations, data.frame(
                        area_name = aoi_name,
                        dominant_rotation = aoi_results$rotation[1],
                        area_ha = aoi_results$area_ha[1],
                        percentage = aoi_results$percentage[1],
                        stringsAsFactors = FALSE
                      ))
                    }
                  }
                  if (nrow(dominant_rotations) > 0) {
                    spatial_data <- merge(spatial_data, dominant_rotations,
                                          by.x = "AOI_name", by.y = "area_name", all.x = TRUE)
                  }
                  if (exists("diversity_data") && is.list(diversity_data) && length(diversity_data) >= 3 &&
                      !is.null(diversity_data[[3]]) && is.list(diversity_data[[3]]) &&
                      !is.null(diversity_data[[3]][[1]]) && "BS_mean" %in% names(diversity_data[[3]][[1]])) {
                    bs_data <- sf::st_drop_geometry(diversity_data[[3]][[1]])[, c("AOI_name", "BS_mean")]
                    spatial_data <- merge(spatial_data, bs_data, by = "AOI_name", all.x = TRUE)
                  }
                }
              }

              # Store spatial type alongside the data for the renderLeaflet
              ranking_map_data(list(
                spatial_data = spatial_data,
                spatial_type = input$ranking_spatial_type,
                rotation_length = input$ranking_rotation_length,
                year_start = start_year,
                year_end = end_year,
                min_area = min_area_val,
                max_area = max_area_val,
                excluded_crops = excluded_crops_val,
                specific_crop = specific_crop_val
              ))
            }, error = function(e) {
              warning("Error computing map data: ", e$message)
              ranking_map_data(NULL)
            })
          } else {
            ranking_map_data(NULL)
          }

          showNotification(
            paste("Analysis complete!", nrow(results), "unique rotations found for", spatial_label),
            type = "message",
            duration = 3
          )
        })
      }, error = function(e) {
        showNotification(
          paste("Error during analysis:", e$message),
          type = "error",
          duration = 5
        )
      })
    })
    
    # Summary Metrics
    output$ranking_total_rotations <- renderText({
      req(ranking_rotation_results())
      format(nrow(ranking_rotation_results()), big.mark = ",")
    })
    
    output$ranking_total_area <- renderText({
      req(ranking_rotation_results())
      format(round(sum(ranking_rotation_results()$area_ha, na.rm = TRUE), 2), big.mark = ",")
    })
    
    output$ranking_total_fields <- renderText({
      req(ranking_rotation_results())
      format(sum(ranking_rotation_results()$n_fields, na.rm = TRUE), big.mark = ",")
    })
    
    output$ranking_avg_area <- renderText({
      req(ranking_rotation_results())
      avg <- sum(ranking_rotation_results()$area_ha) / nrow(ranking_rotation_results())
      format(round(avg, 1), big.mark = ",")
    })
    
    # Ranking Table
    output$ranking_rotation_table <- DT::renderDataTable({
      req(ranking_rotation_results())
      
      results <- ranking_rotation_results()
      
      if (nrow(results) == 0) {
        return(DT::datatable(
          data.frame(Message = "No rotations found with current filters"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }
      
      if (!"rank" %in% names(results)) {
        results <- results %>%
          mutate(rank = row_number())
      }
      
      # Get crop color mapping
      crop_color_mapping_df <- as.data.frame(crop_colors())
      crop_color_mapping_df$crop <- names(crop_colors())
      crop_color_mapping_df <- crop_color_mapping_df[complete.cases(crop_color_mapping_df),]
      names(crop_color_mapping_df) <- c("color", "crop")
      
      # Split rotation string into separate crop columns
      rotation_split <- stringr::str_split(results$rotation, " -> ", simplify = TRUE)
      n_crops <- ncol(rotation_split)
      
      # Create column names for crops
      crop_col_names <- paste0("Crop_", 1:n_crops)
      colnames(rotation_split) <- crop_col_names
      
      # Combine with other data
      table_data <- cbind(
        rank = results$rank,
        as.data.frame(rotation_split, stringsAsFactors = FALSE),
        area_ha = results$area_ha,
        n_fields = results$n_fields,
        percentage = results$percentage
      )
      
      # Rename columns for display
      display_col_names <- c("Rank", paste0("Crop ", 1:n_crops), "Area (km²)", "Number of Fields", "Percentage (%)")
      names(table_data) <- c("rank", crop_col_names, "area_ha", "n_fields", "percentage")
      
      # Create datatable
      dt <- DT::datatable(
        table_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          order = list(list(0, 'asc')),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}")
        ),
        colnames = display_col_names,
        rownames = FALSE
      )
      
      # Get column indices for formatting (1-indexed for DT)
      area_col_idx <- which(names(table_data) == "area_ha")
      percentage_col_idx <- which(names(table_data) == "percentage")
      
      # Format numeric columns
      dt <- dt %>%
        DT::formatRound(columns = c(area_col_idx, percentage_col_idx), digits = 2, mark = ",")
      
      # Apply color formatting to each crop column
      for (i in seq_along(crop_col_names)) {
        crop_col <- crop_col_names[i]
        col_idx <- which(names(table_data) == crop_col)
        dt <- dt %>%
          formatStyle(columns = col_idx,
                      backgroundColor = styleEqual(crop_color_mapping_df$crop,
                                                   crop_color_mapping_df$color),
                      color = "black")
      }
      
      # Style the numeric columns
      rank_col_idx <- which(names(table_data) == "rank")
      n_fields_col_idx <- which(names(table_data) == "n_fields")
      
      dt <- dt %>%
        formatStyle(columns = c(rank_col_idx, area_col_idx, n_fields_col_idx, percentage_col_idx),
                    color = "black",
                    backgroundColor = "lightgrey")
      
      dt
    })
    
    # Area Bar Chart (Top 20)
    output$ranking_area_chart <- renderPlotly({
      req(ranking_rotation_results())
      
      top_n_data <- ranking_rotation_results() %>%
        head(20)
      
      p <- ggplot(top_n_data, aes(x = reorder(rotation, area_ha), y = area_ha,
                                  text = paste0("Rotation: ", rotation, "<br>",
                                                "Area: ", round(area_ha, 1), " km²<br>",
                                                "Percentage: ", round(percentage, 1), "%"))) +
        geom_bar(stat = "identity", fill = "#74961E") +
        coord_flip() +
        labs(
          title = paste("Top 20 Crop Rotations by Area (",
                        input$ranking_rotation_length, "-year rotations)"),
          subtitle = paste(ranking_spatial_label(), "| Year Range:", input$ranking_year_range[1], "-", input$ranking_year_range[2]),
          x = "Rotation Sequence",
          y = "Area (km²)"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold", size = 16, color = "white"),
          plot.subtitle = element_text(color = "gray70"),
          axis.text.y = element_text(size = 10, color = "white"),
          axis.text.x = element_text(color = "white"),
          axis.title = element_text(color = "white"),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray30"),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#222222", color = NA),
          panel.background = element_rect(fill = "#222222", color = NA)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15)))
      
      ggplotly(p, tooltip = "text") %>%
        layout(
          paper_bgcolor = "#222222",
          plot_bgcolor = "#222222",
          font = list(color = "white")
        )
    })
    
    # Pie Chart for Top 20
    output$ranking_pie_chart <- renderPlotly({
      req(ranking_rotation_results())
      
      # Get all rotation data
      all_data <- ranking_rotation_results()
      total_area_all <- sum(all_data$area_ha, na.rm = TRUE)
      
      # Get top 20 rotations
      top_20_data <- all_data %>%
        head(20)
      
      # Create clean data frame with only needed columns
      top_20 <- data.frame(
        rotation_full = top_20_data$rotation,
        rotation_short = ifelse(
          nchar(top_20_data$rotation) > 25,
          paste0(substr(top_20_data$rotation, 1, 22), "..."),
          top_20_data$rotation
        ),
        area_ha = top_20_data$area_ha,
        stringsAsFactors = FALSE
      )
      
      # Calculate "Others" area if there are more than 20 rotations
      if (nrow(all_data) > 20) {
        others_area <- sum(all_data$area_ha[21:nrow(all_data)], na.rm = TRUE)
        
        if (others_area > 0) {
          top_20 <- rbind(
            top_20,
            data.frame(
              rotation_full = "Others",
              rotation_short = "Others",
              area_ha = others_area,
              stringsAsFactors = FALSE
            )
          )
        }
      }
      
      # Calculate percentages based on the total area of ALL rotations
      top_20$percentage <- round((top_20$area_ha / total_area_all) * 100, 2)
      
      plot_ly(
        data = top_20,
        labels = ~rotation_full,
        values = ~area_ha,
        type = 'pie',
        textposition = 'inside',
        textinfo = 'percent',
        hoverinfo = 'text',
        text = ~paste0("<b>", rotation_full, "</b><br>",
                       "Area: ", round(area_ha, 1), " km²<br>",
                       "Percentage: ", round(percentage, 1), "%"),
        marker = list(
          line = list(color = '#FFFFFF', width = 2)
        )
      ) %>%
        layout(
          title = list(
            text = paste0("Top 20 Rotation Distribution (",
                          input$ranking_rotation_length, "-year rotations)<br>",
                          "<sub>", ranking_spatial_label(), " | Year Range: ", input$ranking_year_range[1], " - ", input$ranking_year_range[2], "</sub>"),
            font = list(size = 16, color = "white")
          ),
          margin = list(t = 100),
          showlegend = TRUE,
          legend = list(
            orientation = "v",
            x = 1.02,
            y = 0.5,
            font = list(color = "white", size = 10)
          ),
          paper_bgcolor = "#222222",
          plot_bgcolor = "#222222"
        )
    })
    
    # Leaflet map for selected spatial area showing dominant rotations
    # Only re-renders when ranking_map_data() changes (i.e., when Analyze button is clicked)
    output$ranking_spatial_map <- renderLeaflet({
      req(ranking_map_data())

      map_info <- ranking_map_data()
      spatial_data <- map_info$spatial_data
      spatial_type <- map_info$spatial_type
      rotation_length <- map_info$rotation_length

      if (is.null(spatial_data) || nrow(spatial_data) == 0) {
        return(NULL)
      }

      tryCatch({
        # Transform to WGS84 if needed (Leaflet requires WGS84)
        if (sf::st_crs(spatial_data) != 4326) {
          spatial_data <- sf::st_transform(spatial_data, 4326)
        }

        # Create color palette for dominant rotations
        if ("dominant_rotation" %in% names(spatial_data)) {
          unique_rotations <- unique(spatial_data$dominant_rotation[!is.na(spatial_data$dominant_rotation)])

          if (length(unique_rotations) > 0) {
            # Use a color palette
            pal <- colorFactor(
              palette = "Set3",
              domain = unique_rotations,
              na.color = "#808080"
            )

            # Create labels for hover
            labels <- sprintf(
              "<strong>%s</strong><br/>Dominant Rotation: %s<br/>Area: %.1f km²<br/>Percentage: %.1f%%",
              if (spatial_type == "District") spatial_data$NAME_3 else spatial_data$EZG,
              ifelse(!is.na(spatial_data$dominant_rotation), spatial_data$dominant_rotation, "No data"),
              ifelse(!is.na(spatial_data$area_ha), spatial_data$area_ha, 0),
              ifelse(!is.na(spatial_data$percentage), spatial_data$percentage, 0)
            ) %>% lapply(htmltools::HTML)

            # Get the selected area for red border highlight (isolate to prevent re-render on selection change)
            selected_area <- NULL
            if (spatial_type == "District" && !is.null(isolate(input$ranking_district_sel))) {
              selected_area <- spatial_data[spatial_data$NAME_3 == isolate(input$ranking_district_sel), ]
            } else if (spatial_type == "Catchment/Basin" && !is.null(isolate(input$ranking_catchment_sel))) {
              selected_area <- spatial_data[spatial_data$EZG == isolate(input$ranking_catchment_sel), ]
            }

            # Create leaflet map with color-coded areas
            map <- leaflet(spatial_data, options = leafletOptions(zoomSnap = 0.5, zoomDelta = 0.5, wheelPxPerZoomLevel = 120)) %>%
              addTiles(group = "Base Map") %>%
              addPolygons(
                fillColor = ~pal(dominant_rotation),
                fillOpacity = 0.9,
                weight = 2,
                color = "white",
                opacity = 1,
                label = labels,
                group = "Dominant Rotations",
                highlightOptions = highlightOptions(
                  weight = 3,
                  color = "#666",
                  fillOpacity = 0.9,
                  bringToFront = TRUE
                )
              ) %>%
              addLegend(
                pal = pal,
                values = ~dominant_rotation,
                title = paste0("Dominant ", rotation_length, "-year Rotation"),
                position = "bottomright",
                na.label = "No data"
              )

            # Add soil potential (BS) layer if available
            has_BS <- "BS_mean" %in% names(spatial_data)

            if (has_BS) {
              tryCatch({
                bs_pal <- colorNumeric(
                  palette = c(low = "#ffffcc", high = "#800026"),
                  domain = spatial_data$BS_mean,
                  na.color = "transparent"
                )

                map <- map %>%
                  addPolygons(
                    data = spatial_data,
                    fillColor = ~bs_pal(BS_mean),
                    fillOpacity = 0.9,
                    weight = 0.1,
                    color = "black",
                    group = "Soil Potential",
                    popup = ~paste0(
                      "<strong>Soil Potential:</strong> ",
                      round(BS_mean, 2)
                    ),
                    highlightOptions = highlightOptions(
                      weight = 2,
                      color = "#666666",
                      fillOpacity = 1,
                      bringToFront = TRUE
                    ),
                    popupOptions = popupOptions(
                      closeButton = TRUE,
                      autoClose = TRUE,
                      keepInView = TRUE
                    )
                  ) %>%
                  addLegend(
                    pal = bs_pal,
                    values = spatial_data$BS_mean,
                    title = "Soil Potential",
                    position = "bottomleft"
                  )
              }, error = function(e) {
                warning("Could not add BS layer to map: ", e$message)
              })
            }

            # Add red border for selected area
            if (!is.null(selected_area) && nrow(selected_area) > 0) {
              map <- map %>%
                addPolygons(
                  data = selected_area,
                  fillColor = "transparent",
                  fillOpacity = 0,
                  weight = 4,
                  color = "red",
                  opacity = 1,
                  dashArray = NULL,
                  group = "Selected Area"
                )
            }

            # Add layers control - conditional based on BS availability
            if (has_BS) {
              map <- map %>%
                addLayersControl(
                  overlayGroups = c("Dominant Rotations", "Soil Potential"),
                  options = layersControlOptions(collapsed = FALSE),
                  position = "topleft"
                )
            }

            return(map)
          }
        }

        # Fallback: show simple boundaries if no rotation data
        leaflet(spatial_data, options = leafletOptions(zoomSnap = 0.5, zoomDelta = 0.5, wheelPxPerZoomLevel = 120)) %>%
          addTiles() %>%
          addPolygons(
            fillColor = "rgb(0,86,157)",
            fillOpacity = 0.3,
            weight = 2,
            color = "rgb(0,86,157)",
            dashArray = "5,5"
          )

      }, error = function(e) {
        return(NULL)
      })
    })
    
    # Update red border highlight when district/catchment selection changes (without re-rendering entire map)
    observeEvent(input$ranking_district_sel, {
      req(input$ranking_spatial_type == "District")
      proxy <- leafletProxy("ranking_spatial_map")
      proxy %>% clearGroup("Selected Area")
      if (!is.null(input$ranking_district_sel) && exists("Districts") && !is.null(Districts)) {
        selected_area <- Districts[Districts$NAME_3 == input$ranking_district_sel, ]
        if (nrow(selected_area) > 0) {
          if (sf::st_crs(selected_area) != sf::st_crs(4326)) {
            selected_area <- sf::st_transform(selected_area, 4326)
          }
          proxy %>%
            addPolygons(
              data = selected_area,
              fillColor = "transparent",
              fillOpacity = 0,
              weight = 4,
              color = "red",
              opacity = 1,
              dashArray = NULL,
              group = "Selected Area"
            )
        }
      }
    }, ignoreNULL = FALSE)

    observeEvent(input$ranking_catchment_sel, {
      req(input$ranking_spatial_type == "Catchment/Basin")
      proxy <- leafletProxy("ranking_spatial_map")
      proxy %>% clearGroup("Selected Area")
      if (!is.null(input$ranking_catchment_sel) && exists("EZGs") && !is.null(EZGs)) {
        selected_area <- EZGs[EZGs$EZG == input$ranking_catchment_sel, ]
        if (nrow(selected_area) > 0) {
          if (sf::st_crs(selected_area) != sf::st_crs(4326)) {
            selected_area <- sf::st_transform(selected_area, 4326)
          }
          proxy %>%
            addPolygons(
              data = selected_area,
              fillColor = "transparent",
              fillOpacity = 0,
              weight = 4,
              color = "red",
              opacity = 1,
              dashArray = NULL,
              group = "Selected Area"
            )
        }
      }
    }, ignoreNULL = FALSE)

    # Download Handler
    output$ranking_download_data <- downloadHandler(
      filename = function() {
        paste0("rotation_ranking_",
               input$ranking_rotation_length, "years_",
               input$ranking_year_range[1], "-", input$ranking_year_range[2], "_",
               format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(ranking_rotation_results())
        write.csv(ranking_rotation_results(), file, row.names = FALSE)
      }
    )

    # Shapefile export for the Dominant Rotation Map, including the soil potential
    # (BS_mean) column when available and a companion settings file describing the
    # analysis parameters used to generate the map.
    output$ranking_download_shp <- downloadHandler(
      filename = function() {
        paste0("rotation_ranking_map_",
               input$ranking_rotation_length, "years_",
               input$ranking_year_range[1], "-", input$ranking_year_range[2], "_",
               format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      contentType = "application/zip",
      content = function(file) {
        req(ranking_map_data())
        map_info <- ranking_map_data()
        spatial_data <- map_info$spatial_data
        req(!is.null(spatial_data), inherits(spatial_data, "sf"))

        export_dir <- tempfile("ranking_shp_")
        dir.create(export_dir)
        on.exit(unlink(export_dir, recursive = TRUE), add = TRUE)

        shp_path <- file.path(export_dir, "dominant_rotation_map.shp")
        sf::st_write(sf::st_make_valid(spatial_data), shp_path,
                     driver = "ESRI Shapefile", quiet = TRUE, append = FALSE, delete_layer = TRUE)

        has_soil <- "BS_mean" %in% names(spatial_data)

        meta_lines <- c(
          "CropRotationViz - Dominant Rotation Map Export",
          paste("Export date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "",
          "Analysis settings used to generate this map:",
          paste("  Spatial extent type:", map_info$spatial_type),
          paste("  Rotation length (years):", map_info$rotation_length),
          paste("  Year range:", map_info$year_start, "-", map_info$year_end),
          paste("  Minimum area filter (ha):",
                if (is.null(map_info$min_area)) "none" else map_info$min_area),
          paste("  Maximum area filter (ha):",
                if (is.null(map_info$max_area)) "none" else map_info$max_area),
          paste("  Excluded crops:",
                if (is.null(map_info$excluded_crops) || length(map_info$excluded_crops) == 0) "none"
                else paste(map_info$excluded_crops, collapse = ", ")),
          paste("  Specific crop filter:",
                if (is.null(map_info$specific_crop) || length(map_info$specific_crop) == 0) "none"
                else paste(map_info$specific_crop, collapse = ", ")),
          "",
          paste("Soil potential (Bodenschaetzung, column 'BS_mean') included:",
                if (has_soil) "yes" else "no - not available for this dataset")
        )
        writeLines(meta_lines, file.path(export_dir, "export_settings.txt"))

        zip_files <- list.files(export_dir, full.names = FALSE)

        if (requireNamespace("zip", quietly = TRUE)) {
          zip::zip(file, files = zip_files, root = export_dir)
        } else {
          old_wd <- getwd()
          setwd(export_dir)
          on.exit(setwd(old_wd), add = TRUE, after = FALSE)
          utils::zip(file, files = zip_files)
        }
      }
    )

    # ==============================================================================
    # ROTATION ANALYSIS TAB LOGIC - Monoculture Detection & Pattern Clustering
    # ==============================================================================

    # Reactive values for rotation analysis
    analysis_results <- reactiveVal(NULL)
    monoculture_data <- reactiveVal(NULL)
    cluster_data <- reactiveVal(NULL)
    transition_data <- reactiveVal(NULL)
    network_edge_data <- reactiveVal(NULL)
    network_indicators_data <- reactiveVal(NULL)
    return_time_data <- reactiveVal(NULL)

    # UI for district selection in analysis tab
    output$analysis_district_ui <- renderUI({
      req(exists("district_CropRotViz_intersection"))
      selectInput(
        "analysis_district_sel",
        "Select District:",
        choices = names(district_CropRotViz_intersection),
        selected = names(district_CropRotViz_intersection)[1]
      )
    })

    # UI for catchment selection in analysis tab
    output$analysis_catchment_ui <- renderUI({
      req(exists("EZG_CropRotViz_intersection"))
      selectInput(
        "analysis_catchment_sel",
        "Select Catchment:",
        choices = names(EZG_CropRotViz_intersection),
        selected = names(EZG_CropRotViz_intersection)[1]
      )
    })

    # UI for custom area (AOI) selection in analysis tab
    output$analysis_aoi_ui <- renderUI({
      req(exists("AOI_CropRotViz_intersection") && !is.null(AOI_CropRotViz_intersection) &&
          !identical(AOI_CropRotViz_intersection, NA))
      selectInput(
        "analysis_aoi_sel",
        "Select Custom Area:",
        choices = names(AOI_CropRotViz_intersection),
        selected = names(AOI_CropRotViz_intersection)[1]
      )
    })

    # UI for excluding crops in analysis tab
    output$analysis_exclude_crops_ui <- renderUI({
      req(CropRotViz_intersection)

      # Get all unique crops from the data
      agg_cols <- grep("^Aggregated_", names(CropRotViz_intersection), value = TRUE)
      if (length(agg_cols) == 0) {
        agg_cols <- grep("^Name_", names(CropRotViz_intersection), value = TRUE)
      }

      if (length(agg_cols) > 0) {
        all_crops <- unique(unlist(lapply(agg_cols, function(col) {
          unique(CropRotViz_intersection[[col]])
        })))
        all_crops <- sort(all_crops[!is.na(all_crops)])

        selectInput(
          "analysis_exclude_crops",
          "Exclude Crops:",
          choices = all_crops,
          multiple = TRUE,
          selected = NULL
        )
      }
    })

    # UI for including specific crops in analysis tab
    output$analysis_include_crops_ui <- renderUI({
      req(CropRotViz_intersection)

      # Get all unique crops from the data
      agg_cols <- grep("^Aggregated_", names(CropRotViz_intersection), value = TRUE)
      if (length(agg_cols) == 0) {
        agg_cols <- grep("^Name_", names(CropRotViz_intersection), value = TRUE)
      }

      if (length(agg_cols) > 0) {
        all_crops <- unique(unlist(lapply(agg_cols, function(col) {
          unique(CropRotViz_intersection[[col]])
        })))
        all_crops <- sort(all_crops[!is.na(all_crops)])

        selectInput(
          "analysis_include_crops",
          "Must Include Crop:",
          choices = c("All patterns" = "", all_crops),
          multiple = TRUE,
          selected = ""
        )
      }
    })

    # Main analysis observer
    observeEvent(input$run_rotation_analysis, {
      req(CropRotViz_intersection)

      tryCatch({
        withProgress(message = 'Running rotation analysis...', value = 0, {

          # Determine which dataset to use based on spatial filter
          incProgress(0.1, detail = "Selecting data...")

          data_to_analyze <- CropRotViz_intersection

          if (!is.null(input$analysis_spatial_type) && input$analysis_spatial_type != "full") {
            if (input$analysis_spatial_type == "district" && !is.null(input$analysis_district_sel)) {
              data_to_analyze <- district_CropRotViz_intersection[[input$analysis_district_sel]]
              if (!inherits(data_to_analyze, "data.table")) {
                data_to_analyze <- data.table::as.data.table(data_to_analyze)
              }
            } else if (input$analysis_spatial_type == "catchment" && !is.null(input$analysis_catchment_sel)) {
              data_to_analyze <- EZG_CropRotViz_intersection[[input$analysis_catchment_sel]]
              if (!inherits(data_to_analyze, "data.table")) {
                data_to_analyze <- data.table::as.data.table(data_to_analyze)
              }
            } else if (input$analysis_spatial_type == "aoi" && !is.null(input$analysis_aoi_sel)) {
              data_to_analyze <- AOI_CropRotViz_intersection[[input$analysis_aoi_sel]]
              if (!inherits(data_to_analyze, "data.table")) {
                data_to_analyze <- data.table::as.data.table(data_to_analyze)
              }
            }
          }

          # Get crop columns (Aggregated_ or Name_)
          agg_cols <- grep("^Aggregated_", names(data_to_analyze), value = TRUE)
          if (length(agg_cols) == 0) {
            agg_cols <- grep("^Name_", names(data_to_analyze), value = TRUE)
          }

          if (length(agg_cols) == 0) {
            showNotification("No crop columns found in data!", type = "error")
            return()
          }

          incProgress(0.2, detail = "Computing transitions...")

          # Get data frame without geometry
          df <- if (inherits(data_to_analyze, "sf")) {
            sf::st_drop_geometry(data_to_analyze)
          } else {
            as.data.frame(data_to_analyze)
          }

          # Calculate transitions and unique counts for each field
          df$transitions <- apply(df[, agg_cols, drop = FALSE], 1, function(row) {
            if (length(row) < 2) return(0)
            sum(row[-length(row)] != row[-1])
          })

          df$unique_count <- apply(df[, agg_cols, drop = FALSE], 1, function(x) {
            length(unique(x))
          })

          # Create rotation string
          df$rotation <- apply(df[, agg_cols, drop = FALSE], 1, function(x) {
            paste(x, collapse = " -> ")
          })

          # Apply crop filters
          excluded_crops <- input$analysis_exclude_crops
          include_crops <- input$analysis_include_crops

          # Filter out rows containing excluded crops
          if (!is.null(excluded_crops) && length(excluded_crops) > 0 && any(excluded_crops != "")) {
            exclude_pattern <- paste(excluded_crops, collapse = "|")
            rows_to_keep <- !grepl(exclude_pattern, df$rotation, ignore.case = TRUE)
            df <- df[rows_to_keep, ]
          }

          # Filter to only include rows with required crops
          if (!is.null(include_crops) && length(include_crops) > 0 && any(include_crops != "")) {
            include_crops <- include_crops[include_crops != ""]
            if (length(include_crops) > 0) {
              for (crop in include_crops) {
                df <- df[grepl(crop, df$rotation, ignore.case = TRUE), ]
              }
            }
          }

          # Check if any data remains after filtering
          if (nrow(df) == 0) {
            showNotification("No data remaining after applying crop filters!", type = "warning")
            return()
          }

          # Get the dominant crop for monocultures
          df$dominant_crop <- apply(df[, agg_cols, drop = FALSE], 1, function(x) {
            tbl <- table(x)
            names(tbl)[which.max(tbl)]
          })

          # Store full analysis results
          analysis_results(df)

          incProgress(0.4, detail = "Detecting monocultures...")

          # ==== MONOCULTURE DETECTION ====
          # Monoculture = fields with 0 transitions (same crop every year)
          monocultures <- df[df$transitions == 0, ]

          if (nrow(monocultures) > 0) {
            mono_summary <- monocultures %>%
              group_by(dominant_crop) %>%
              summarise(
                n_fields = n(),
                total_area = sum(freq, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              arrange(desc(total_area)) %>%
              mutate(
                percentage = round(total_area / sum(total_area) * 100, 2)
              )

            monoculture_data(list(
              summary = mono_summary,
              details = monocultures,
              total_fields = nrow(df),
              total_area = sum(df$freq, na.rm = TRUE),
              mono_fields = nrow(monocultures),
              mono_area = sum(monocultures$freq, na.rm = TRUE)
            ))
          } else {
            monoculture_data(list(
              summary = data.frame(),
              details = data.frame(),
              total_fields = nrow(df),
              total_area = sum(df$freq, na.rm = TRUE),
              mono_fields = 0,
              mono_area = 0
            ))
          }

          incProgress(0.6, detail = "Clustering rotation patterns...")

          # ==== PATTERN CLUSTERING ====
          # Aggregate rotations first
          rotation_agg <- df %>%
            group_by(rotation) %>%
            summarise(
              n_fields = n(),
              total_area = sum(freq, na.rm = TRUE),
              avg_transitions = mean(transitions, na.rm = TRUE),
              avg_unique_crops = mean(unique_count, na.rm = TRUE),
              dominant_crop = first(dominant_crop),
              .groups = "drop"
            ) %>%
            filter(total_area >= input$min_cluster_area)

          # Calculate unique crops for each pattern
          rotation_agg$unique_crops <- sapply(rotation_agg$rotation, function(rot) {
            crops <- strsplit(rot, " -> ")[[1]]
            length(unique(crops))
          })

          if (nrow(rotation_agg) >= input$n_clusters) {
            # =======================================================
            # CLUSTERING BASED ON STRUCTURAL DIVERSITY ONLY:
            # 1. Number of transitions (rotation frequency)
            # 2. Number of unique crops (crop diversity)
            # =======================================================

            # Prepare features for clustering (normalize to 0-1 scale)
            max_trans <- max(rotation_agg$avg_transitions, na.rm = TRUE)
            max_crops <- max(rotation_agg$unique_crops, na.rm = TRUE)

            # Create normalized feature matrix
            features <- data.frame(
              transitions_norm = rotation_agg$avg_transitions / max(max_trans, 1),
              unique_crops_norm = rotation_agg$unique_crops / max(max_crops, 1)
            )

            # Use hierarchical clustering on the 2D feature space
            dist_matrix <- dist(features, method = "euclidean")
            hc <- hclust(dist_matrix, method = "ward.D2")
            clusters <- cutree(hc, k = min(input$n_clusters, nrow(rotation_agg)))

            rotation_agg$cluster <- clusters

            # Function to get dominant crops from patterns in a cluster
            get_dominant_crops <- function(patterns, n = 3) {
              all_crops <- unlist(strsplit(patterns, " -> "))
              crop_freq <- sort(table(all_crops), decreasing = TRUE)
              head(names(crop_freq), n)
            }

            # Function to generate cluster interpretation based on structural diversity
            generate_interpretation <- function(avg_trans, avg_crops, dominant_crops) {
              top_crops <- paste(head(dominant_crops, 2), collapse = "/")

              # Classification based on structural diversity (transitions + unique crops)
              if (avg_trans < 0.5 && avg_crops <= 1.5) {
                paste0("Monoculture (", top_crops, ")")
              } else if (avg_trans < 2 && avg_crops <= 2) {
                paste0("Very low diversity (", round(avg_trans, 1), " trans, ", round(avg_crops, 1), " crops)")
              } else if (avg_trans < 4 && avg_crops <= 3) {
                paste0("Low diversity (", round(avg_trans, 1), " trans, ", round(avg_crops, 1), " crops)")
              } else if (avg_trans < 6 && avg_crops <= 4) {
                paste0("Medium diversity (", round(avg_trans, 1), " trans, ", round(avg_crops, 1), " crops)")
              } else {
                paste0("High diversity (", round(avg_trans, 1), " trans, ", round(avg_crops, 1), " crops)")
              }
            }

            # Summarize clusters with interpretation
            cluster_summary <- rotation_agg %>%
              group_by(cluster) %>%
              summarise(
                n_patterns = n(),
                n_fields = sum(n_fields, na.rm = TRUE),
                total_area = sum(total_area, na.rm = TRUE),
                avg_transitions = mean(avg_transitions, na.rm = TRUE),
                avg_unique_crops = mean(unique_crops, na.rm = TRUE),
                top_pattern = rotation[which.max(total_area)],
                dominant_crops = list(get_dominant_crops(rotation)),
                .groups = "drop"
              ) %>%
              arrange(desc(total_area)) %>%
              rowwise() %>%
              mutate(
                percentage = round(total_area / sum(.$total_area) * 100, 2),
                interpretation = generate_interpretation(
                  avg_transitions,
                  avg_unique_crops,
                  dominant_crops
                ),
                dominant_crops_str = paste(dominant_crops, collapse = ", ")
              ) %>%
              ungroup() %>%
              select(-dominant_crops)

            cluster_data(list(
              summary = cluster_summary,
              patterns = rotation_agg
            ))
          } else {
            # Not enough patterns for clustering - create single cluster with interpretation
            avg_trans <- if (nrow(rotation_agg) > 0) mean(rotation_agg$avg_transitions, na.rm = TRUE) else 0
            avg_crops <- if (nrow(rotation_agg) > 0) mean(rotation_agg$unique_crops, na.rm = TRUE) else 0
            dom_crops <- if (nrow(rotation_agg) > 0) {
              all_crops <- unlist(strsplit(rotation_agg$rotation, " -> "))
              crop_freq <- sort(table(all_crops), decreasing = TRUE)
              head(names(crop_freq), 3)
            } else {
              c("N/A")
            }

            interp <- if (avg_trans < 0.5 && avg_crops <= 1.5) {
              paste0("Monoculture (", dom_crops[1], ")")
            } else {
              paste0("Mixed (", round(avg_trans, 1), " trans, ", round(avg_crops, 1), " crops)")
            }

            cluster_data(list(
              summary = data.frame(
                cluster = 1,
                n_patterns = nrow(rotation_agg),
                n_fields = sum(rotation_agg$n_fields, na.rm = TRUE),
                total_area = sum(rotation_agg$total_area, na.rm = TRUE),
                avg_transitions = avg_trans,
                avg_unique_crops = avg_crops,
                top_pattern = if (nrow(rotation_agg) > 0) rotation_agg$rotation[1] else NA,
                percentage = 100,
                interpretation = interp,
                dominant_crops_str = paste(dom_crops, collapse = ", ")
              ),
              patterns = if (nrow(rotation_agg) > 0) rotation_agg %>% mutate(cluster = 1) else data.frame()
            ))
          }

          incProgress(0.8, detail = "Computing transition statistics...")

          # ==== TRANSITION ANALYSIS ====
          transition_summary <- df %>%
            group_by(transitions) %>%
            summarise(
              n_fields = n(),
              total_area = sum(freq, na.rm = TRUE),
              .groups = "drop"
            ) %>%
            mutate(
              percentage = round(n_fields / sum(n_fields) * 100, 2),
              area_percentage = round(total_area / sum(total_area) * 100, 2),
              category = case_when(
                transitions == 0 ~ "Monoculture (0)",
                transitions == 1 ~ "Low (1)",
                transitions == 2 ~ "Medium (2)",
                transitions >= 3 ~ paste0("High (", transitions, ")")
              )
            )

          transition_data(list(
            summary = transition_summary,
            stats = list(
              mean = round(mean(df$transitions, na.rm = TRUE), 2),
              median = median(df$transitions, na.rm = TRUE),
              max = max(df$transitions, na.rm = TRUE),
              min = min(df$transitions, na.rm = TRUE),
              sd = round(sd(df$transitions, na.rm = TRUE), 2)
            )
          ))

          incProgress(0.9, detail = "Building rotation network...")

          # ==== NETWORK ANALYSIS ====
          # `df` already carries the Aggregated_/Name_ year columns + freq (area),
          # filtered by spatial scope and crop include/exclude above -- exactly
          # what compute_transition_matrix() expects.
          edges <- compute_transition_matrix(df)
          network_edge_data(edges)

          indicators <- compute_network_indicators(edges)
          network_indicators_data(indicators)

          # Default return-time simulation for the crop with the most involved
          # area, so the panel shows something before the user picks a crop.
          if (nrow(indicators$precedents_table) > 0) {
            default_crop <- indicators$precedents_table$crop[1]
            rt <- simulate_crop_return_time(edges, default_crop, n_sim = 100, n_years = 100, seed = 1)
            return_time_data(list(crop = default_crop, return_times = rt))
          } else {
            return_time_data(NULL)
          }

          incProgress(1.0, detail = "Complete!")

          showNotification(
            paste("Analysis complete!", nrow(df), "fields analyzed."),
            type = "message",
            duration = 3
          )
        })
      }, error = function(e) {
        showNotification(
          paste("Error during analysis:", e$message),
          type = "error",
          duration = 5
        )
      })
    })

    # ==== SUMMARY METRICS ====
    output$total_fields_analyzed <- renderText({
      req(monoculture_data())
      format(monoculture_data()$total_fields, big.mark = ",")
    })

    output$monoculture_count <- renderText({
      req(monoculture_data())
      format(monoculture_data()$mono_fields, big.mark = ",")
    })

    output$monoculture_percentage <- renderText({
      req(monoculture_data())
      total_area <- monoculture_data()$total_area
      mono_area  <- monoculture_data()$mono_area
      if (is.null(total_area) || total_area == 0) return("N/A")
      paste0(round(mono_area / total_area * 100, 1), "%")
    })

    output$monoculture_area <- renderText({
      req(monoculture_data())
      format(round(monoculture_data()$mono_area, 2), big.mark = ",")
    })

    # ==== MONOCULTURE VISUALIZATIONS ====
    output$monoculture_bar_chart <- renderPlotly({
      req(monoculture_data())

      summary_df <- monoculture_data()$summary

      if (nrow(summary_df) == 0) {
        return(plotly_empty() %>%
                 layout(title = "No monocultures detected"))
      }

      # Get colors from crop_colors if available
      colors <- if (exists("crop_colors") && is.function(crop_colors)) {
        sapply(summary_df$dominant_crop, function(crop) {
          cols <- crop_colors()
          if (crop %in% names(cols)) cols[crop] else "#666666"
        })
      } else {
        rep("#74961E", nrow(summary_df))
      }

      plot_ly(
        data = summary_df,
        x = ~reorder(dominant_crop, -total_area),
        y = ~total_area,
        type = "bar",
        marker = list(color = colors),
        text = ~paste0("Crop: ", dominant_crop,
                       "<br>Area: ", round(total_area, 2), " km²",
                       "<br>Fields: ", n_fields),
        hoverinfo = "text"
      ) %>%
        layout(
          title = list(text = "Monoculture Area by Crop", font = list(color = "white")),
          xaxis = list(title = "Crop", tickangle = -45, color = "white"),
          yaxis = list(title = "Area (km²)", color = "white"),
          paper_bgcolor = "#1f1b1b",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "white")
        )
    })

    output$monoculture_pie_chart <- renderPlotly({
      req(monoculture_data())

      summary_df <- monoculture_data()$summary

      if (nrow(summary_df) == 0) {
        return(plotly_empty() %>%
                 layout(title = "No monocultures detected"))
      }

      # Get colors
      colors <- if (exists("crop_colors") && is.function(crop_colors)) {
        sapply(summary_df$dominant_crop, function(crop) {
          cols <- crop_colors()
          if (crop %in% names(cols)) cols[crop] else "#666666"
        })
      } else {
        RColorBrewer::brewer.pal(min(nrow(summary_df), 8), "Set2")
      }

      plot_ly(
        data = summary_df,
        labels = ~dominant_crop,
        values = ~total_area,
        type = "pie",
        marker = list(colors = colors),
        textinfo = "label+percent",
        hoverinfo = "text",
        text = ~paste0(dominant_crop, ": ", round(total_area, 2), " km²")
      ) %>%
        layout(
          title = list(text = "Monoculture Distribution", font = list(color = "white")),
          paper_bgcolor = "#1f1b1b",
          font = list(color = "white"),
          showlegend = FALSE
        )
    })

    output$monoculture_table <- DT::renderDataTable({
      req(monoculture_data())

      summary_df <- monoculture_data()$summary

      if (nrow(summary_df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No monocultures detected in the data"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      table_data <- summary_df %>%
        select(
          Crop = dominant_crop,
          `Number of Fields` = n_fields,
          `Total Area (km²)` = total_area,
          `Percentage (%)` = percentage
        )

      DT::datatable(
        table_data,
        options = list(
          pageLength = 10,
          order = list(list(2, 'desc')),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Total Area (km²)", "Percentage (%)"), digits = 2)
    })

    # ==== CLUSTER VISUALIZATIONS ====

    # Define consistent cluster colors (reactive to be shared across visualizations)
    cluster_colors_palette <- reactive({
      req(cluster_data())
      n_clusters <- nrow(cluster_data()$summary)
      if (n_clusters == 0) return(NULL)
      RColorBrewer::brewer.pal(max(min(n_clusters, 8), 3), "Set2")[1:n_clusters]
    })

    output$cluster_distribution_chart <- renderPlotly({
      req(cluster_data(), cluster_colors_palette())

      summary_df <- cluster_data()$summary

      if (nrow(summary_df) == 0) {
        return(plotly_empty() %>%
                 layout(title = "No clusters generated"))
      }

      colors <- cluster_colors_palette()

      plot_ly(
        data = summary_df,
        x = ~paste("Cluster", cluster),
        y = ~n_patterns,
        type = "bar",
        marker = list(color = colors),
        text = ~paste0("Cluster ", cluster,
                       "<br>Patterns: ", n_patterns,
                       "<br>Fields: ", n_fields),
        hoverinfo = "text"
      ) %>%
        layout(
          title = list(text = "Number of Patterns per Cluster", font = list(color = "white")),
          xaxis = list(title = "Cluster", color = "white"),
          yaxis = list(title = "Number of Patterns", color = "white"),
          paper_bgcolor = "#1f1b1b",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "white")
        )
    })

    output$cluster_area_chart <- renderPlotly({
      req(cluster_data(), cluster_colors_palette())

      summary_df <- cluster_data()$summary

      if (nrow(summary_df) == 0) {
        return(plotly_empty() %>%
                 layout(title = "No clusters generated"))
      }

      colors <- cluster_colors_palette()

      plot_ly(
        data = summary_df,
        labels = ~paste("Cluster", cluster),
        values = ~total_area,
        type = "pie",
        marker = list(colors = colors),
        textinfo = "label+percent",
        hoverinfo = "text",
        text = ~paste0("Cluster ", cluster,
                       "<br>Area: ", round(total_area, 2), " km²",
                       "<br>Top: ", substr(top_pattern, 1, 50), "...")
      ) %>%
        layout(
          title = list(text = "Cluster Area Distribution", font = list(color = "white")),
          paper_bgcolor = "#1f1b1b",
          font = list(color = "white"),
          showlegend = TRUE,
          legend = list(font = list(color = "white"))
        )
    })

    # Cluster Separability Scatter Plot
    output$cluster_scatter_plot <- renderPlotly({
      req(cluster_data(), cluster_colors_palette())

      patterns_df <- cluster_data()$patterns

      if (is.null(patterns_df) || nrow(patterns_df) == 0) {
        return(plotly_empty() %>%
                 layout(title = "No pattern data available"))
      }

      # Calculate unique crop count for each pattern
      patterns_df$unique_crops <- sapply(patterns_df$rotation, function(rot) {
        crops <- strsplit(rot, " -> ")[[1]]
        length(unique(crops))
      })

      # Get cluster colors
      n_clusters <- length(unique(patterns_df$cluster))
      cluster_colors <- cluster_colors_palette()

      # Create scatter plot
      plot_ly(
        data = patterns_df,
        x = ~avg_transitions,
        y = ~unique_crops,
        size = ~total_area,
        color = ~factor(cluster),
        colors = cluster_colors,
        type = "scatter",
        mode = "markers",
        marker = list(
          opacity = 0.7,
          sizemode = "area",
          sizeref = 2 * max(patterns_df$total_area, na.rm = TRUE) / (40^2),
          sizemin = 4
        ),
        text = ~paste0(
          "<b>Cluster ", cluster, "</b><br>",
          "Transitions: ", round(avg_transitions, 1), "<br>",
          "Unique crops: ", unique_crops, "<br>",
          "Area: ", round(total_area, 2), " km²<br>",
          "Pattern: ", substr(rotation, 1, 60), "..."
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          title = list(text = "Cluster Separability", font = list(color = "white")),
          xaxis = list(
            title = "Number of Transitions (Rotation Diversity)",
            color = "white",
            gridcolor = "#444444"
          ),
          yaxis = list(
            title = "Number of Unique Crops",
            color = "white",
            gridcolor = "#444444"
          ),
          paper_bgcolor = "#1f1b1b",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "white"),
          legend = list(
            title = list(text = "Cluster"),
            font = list(color = "white")
          )
        )
    })

    output$cluster_table <- DT::renderDataTable({
      req(cluster_data())

      summary_df <- cluster_data()$summary

      if (nrow(summary_df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No clusters generated"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      table_data <- summary_df %>%
        select(
          Cluster = cluster,
          Interpretation = interpretation,
          `Dominant Crops` = dominant_crops_str,
          `Patterns` = n_patterns,
          `Fields` = n_fields,
          `Area (km²)` = total_area,
          `Avg. Trans.` = avg_transitions,
          `Avg. Crops` = avg_unique_crops,
          `Area %` = percentage
        )

      DT::datatable(
        table_data,
        options = list(
          pageLength = 10,
          order = list(list(5, 'desc')),
          scrollX = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Area (km²)", "Avg. Trans.", "Avg. Crops", "Area %"), digits = 2)
    })

    output$cluster_patterns_table <- DT::renderDataTable({
      req(cluster_data())

      patterns_df <- cluster_data()$patterns

      if (nrow(patterns_df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No patterns available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      # Get top 3 patterns per cluster
      top_patterns <- patterns_df %>%
        group_by(cluster) %>%
        slice_max(order_by = total_area, n = 3) %>%
        ungroup() %>%
        arrange(cluster, desc(total_area)) %>%
        select(
          Cluster = cluster,
          `Rotation Pattern` = rotation,
          `Area (km²)` = total_area,
          `Fields` = n_fields,
          `Transitions` = avg_transitions
        )

      DT::datatable(
        top_patterns,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Area (km²)", "Transitions"), digits = 2)
    })

    # ==== TRANSITION VISUALIZATIONS ====
    output$transition_histogram <- renderPlotly({
      req(transition_data())

      summary_df <- transition_data()$summary

      if (nrow(summary_df) == 0) {
        return(plotly_empty() %>%
                 layout(title = "No transition data available"))
      }

      plot_ly(
        data = summary_df,
        x = ~transitions,
        y = ~n_fields,
        type = "bar",
        marker = list(
          color = ~transitions,
          colorscale = list(c(0, "#74961E"), c(1, "#164f8c")),
          showscale = FALSE
        ),
        text = ~paste0("Transitions: ", transitions,
                       "<br>Fields: ", format(n_fields, big.mark = ","),
                       "<br>Percentage: ", percentage, "%"),
        hoverinfo = "text"
      ) %>%
        layout(
          title = list(text = "Distribution of Crop Transitions", font = list(color = "white")),
          xaxis = list(
            title = "Number of Transitions",
            color = "white",
            dtick = 1
          ),
          yaxis = list(title = "Number of Fields", color = "white"),
          paper_bgcolor = "#1f1b1b",
          plot_bgcolor = "#2d2d2d",
          font = list(color = "white")
        )
    })

    output$transition_stats_table <- renderTable({
      req(transition_data())

      stats <- transition_data()$stats

      data.frame(
        Statistic = c("Mean", "Median", "Std. Dev.", "Minimum", "Maximum"),
        Value = c(stats$mean, stats$median, stats$sd, stats$min, stats$max)
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$transition_categories_table <- DT::renderDataTable({
      req(transition_data())

      summary_df <- transition_data()$summary

      if (nrow(summary_df) == 0) {
        return(DT::datatable(
          data.frame(Message = "No transition data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      table_data <- summary_df %>%
        select(
          `Transitions` = transitions,
          Category = category,
          `Number of Fields` = n_fields,
          `Field %` = percentage,
          `Total Area (km²)` = total_area,
          `Area %` = area_percentage
        )

      DT::datatable(
        table_data,
        options = list(
          pageLength = 10,
          order = list(list(0, 'asc')),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = c("Field %", "Total Area (km²)", "Area %"), digits = 2)
    })

    # ==== NETWORK ANALYSIS OUTPUTS ====

    output$network_focus_crop_ui <- renderUI({
      req(network_indicators_data())
      tbl <- network_indicators_data()$precedents_table
      req(nrow(tbl) > 0)

      selectInput(
        "network_focus_crop",
        "Crop for return-time simulation:",
        choices = tbl$crop,
        selected = tbl$crop[1]
      )
    })

    output$network_density_metric <- renderText({
      req(network_indicators_data())
      d <- network_indicators_data()$density
      if (is.na(d)) "N/A" else paste0(round(d * 100, 1), "%")
    })

    output$network_precedents_metric <- renderText({
      req(network_indicators_data())
      p <- network_indicators_data()$mean_precedents
      if (is.na(p)) "N/A" else round(p, 2)
    })

    output$network_nodes_metric <- renderText({
      req(network_indicators_data())
      network_indicators_data()$n_nodes
    })

    output$network_edges_metric <- renderText({
      req(network_indicators_data())
      network_indicators_data()$n_edges
    })

    output$rotation_network_plot <- renderPlotly({
      req(network_edge_data())
      min_share <- if (is.null(input$network_min_share)) 0.5 else input$network_min_share
      show_loops <- if (is.null(input$network_show_selfloops)) TRUE else input$network_show_selfloops

      plot_rotation_network(
        network_edge_data(),
        crop_colors = crop_colors(),
        min_share = min_share / 100,
        show_selfloops = show_loops
      )
    })

    output$network_precedents_table <- DT::renderDataTable({
      req(network_indicators_data())
      tbl <- network_indicators_data()$precedents_table

      if (nrow(tbl) == 0) {
        return(DT::datatable(
          data.frame(Message = "No network data available"),
          options = list(dom = 't'),
          rownames = FALSE
        ))
      }

      table_data <- tbl %>%
        select(
          Crop = crop,
          `Precedents (in)` = n_precedents,
          `Successors (out)` = n_successors,
          `Total Area (km²)` = total_area_km2
        )

      DT::datatable(
        table_data,
        options = list(
          pageLength = 10,
          order = list(list(3, 'desc')),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'color': '#fff'});",
            "}"
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatRound(columns = "Total Area (km²)", digits = 2)
    })

    # Simulate return time for the currently selected focus crop
    observeEvent(input$run_return_time_sim, {
      req(network_edge_data(), input$network_focus_crop)

      tryCatch({
        rt <- simulate_crop_return_time(
          network_edge_data(), input$network_focus_crop,
          n_sim = 100, n_years = 100, seed = NULL
        )
        return_time_data(list(crop = input$network_focus_crop, return_times = rt))
      }, error = function(e) {
        showNotification(
          paste("Error simulating return time:", e$message),
          type = "error", duration = 5
        )
      })
    })

    output$return_time_median <- renderText({
      req(return_time_data())
      rt <- return_time_data()$return_times
      if (length(rt) == 0) "N/A" else paste(round(median(rt), 1), "yrs")
    })

    output$return_time_histogram <- renderPlotly({
      req(return_time_data())
      rt <- return_time_data()$return_times
      crop <- return_time_data()$crop

      if (length(rt) == 0) {
        return(
          plot_ly() %>%
            layout(
              title = list(text = paste0("No return observed for ", crop, " within the simulated horizon"),
                           font = list(size = 12, color = "white")),
              paper_bgcolor = "#1f1b1b", plot_bgcolor = "#2d2d2d",
              font = list(color = "white")
            )
        )
      }

      plot_ly(x = rt, type = "histogram",
              marker = list(color = "rgb(116, 150, 30)")) %>%
        layout(
          title = list(text = paste("Return time:", crop), font = list(size = 13, color = "white")),
          xaxis = list(title = "Years to return", color = "white", gridcolor = "#444"),
          yaxis = list(title = "Simulated occurrences", color = "white", gridcolor = "#444"),
          paper_bgcolor = "#1f1b1b", plot_bgcolor = "#2d2d2d",
          font = list(color = "white"),
          bargap = 0.1
        )
    })

    output$download_network_edges <- downloadHandler(
      filename = function() {
        paste0("rotation_network_edges_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(network_edge_data())
        write.csv(network_edge_data(), file, row.names = FALSE)
      }
    )

    # ==== DOWNLOAD HANDLERS ====
    output$download_monoculture_data <- downloadHandler(
      filename = function() {
        paste0("monoculture_analysis_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(monoculture_data())
        write.csv(monoculture_data()$summary, file, row.names = FALSE)
      }
    )

    output$download_cluster_data <- downloadHandler(
      filename = function() {
        paste0("cluster_analysis_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        req(cluster_data())
        write.csv(cluster_data()$patterns, file, row.names = FALSE)
      }
    )

  })
}