# Load Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(magrittr) 
library(ggplot2)
library(sf)
library(DT)
library(smwrGraphs)
library(smwrBase)
library(readr)
library(tidyr)
library(rmarkdown)
library(shinycssloaders)

# Sample Data
get_sample_data <- function() {
  tibble(
    LOCATION_NAME = sample(c("Karnataka", "Tamil Nadu", "Maharashtra", "Kerala", "Andhra Pradesh", "Gujarat", "Punjab", "West Bengal", "Odisha", "Bihar"), 20, replace = TRUE),
    CA = runif(20, 20, 100),
    MG = runif(20, 10, 50),
    Sodium = runif(20, 30, 150),
    K = runif(20, 2, 10),
    CHLORIDE = runif(20, 50, 250),
    SULPHATE = runif(20, 40, 200),
    BICARBONATE = runif(20, 80, 300)
  )
}

# UI
ui <- dashboardPage(
  title = "Ground Water Assessment Dashboard",
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; height: 60px;",
      tags$span("🌊 Ground Water Assessment", style = "font-size: 18px; font-weight: bold; color: white;")
    ),
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("📁 File Upload", tabName = "upload", icon = icon("upload")),
      menuItem("📊 Data Exploration", tabName = "data_explore", icon = icon("chart-bar")),
      menuItem("💧 Chemistry Analysis", tabName = "chemistry", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .box { border-radius: 10px; box-shadow: 0 1px 5px rgba(0,0,0,0.1); }
        .content-wrapper { background-color: #f8f9fa; }
        .main-sidebar { background-color: #1e3d59; }
        .skin-blue .sidebar a { color: #ffffff; }
        .skin-blue .sidebar-menu>li.active>a {
          background-color: #1e90ff; color: white;
          font-weight: bold;
        }
        .form-control, .selectpicker {
          border-radius: 8px !important;
        }
        .box-title {
          font-weight: bold;
          color: #1e3d59;
        }
      "))
    ),
    
    tabItems(
      # File Upload Tab
      tabItem(tabName = "upload",
              fluidRow(
                box(width = 6, title = "Upload Groundwater Data", status = "primary", solidHeader = TRUE,
                    fileInput("gpkg_upload", "Choose GPKG File", accept = c(".gpkg")),
                    actionButton("load_data", "Load Data", icon = icon("sync"))
                ),
                box(width = 6, title = "File Info", status = "info", solidHeader = TRUE,
                    verbatimTextOutput("file_info"))
              )
      ),
      
      # Data Exploration Tab
      tabItem(tabName = "data_explore",
              fluidRow(
                box(width = 6, title = "Dataset Overview", status = "primary", solidHeader = TRUE, uiOutput("dataset_summary")),
                box(width = 6, title = "Column Metadata", status = "info", solidHeader = TRUE, DTOutput("column_details"))
              ),
              fluidRow(
                box(width = 6, title = "Sample Data Preview", status = "warning", solidHeader = TRUE, DTOutput("data_preview")),
                box(width = 6, title = "Statistics Summary", status = "success", solidHeader = TRUE, uiOutput("data_statistics"))
              )
      ),
      
      # Chemistry Tab
      tabItem(tabName = "chemistry",
              fluidRow(
                box(width = 4, title = "📍 Select Location(s)", status = "primary", solidHeader = TRUE,
                    pickerInput("location_filter", label = "Location(s)",
                                choices = NULL, multiple = TRUE,
                                options = list(
                                  `actions-box` = TRUE,
                                  `live-search` = TRUE,
                                  `selected-text-format` = "count > 2"
                                ))
                ),
                box(width = 8, title = "Piper Diagram", status = "info", solidHeader = TRUE,
                    withSpinner(plotOutput("piper_plot", height = "700px"), type = 6)
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  data_storage <- reactiveValues(csv_data = get_sample_data(), data_loaded = FALSE)
  
  observeEvent(input$load_data, {
    req(input$gpkg_upload)
    tryCatch({
      showNotification("Custom GPKG upload not yet handled in this version", type = "warning")
    }, error = function(e) {
      showNotification(paste("Error processing file:", e$message), type = "error")
    })
  })
  
  output$file_info <- renderPrint({
    req(data_storage$csv_data)
    cat("File or Sample Data Loaded\n",
        "Number of Rows:", nrow(data_storage$csv_data), "\n",
        "Number of Columns:", ncol(data_storage$csv_data))
  })
  
  output$dataset_summary <- renderUI({
    req(data_storage$csv_data)
    HTML(paste(
      "<h4>Dataset Characteristics</h4>",
      "<p><strong>Total Rows:</strong>", nrow(data_storage$csv_data), "</p>",
      "<p><strong>Total Columns:</strong>", ncol(data_storage$csv_data), "</p>"
    ))
  })
  
  output$column_details <- renderDT({
    req(data_storage$csv_data)
    column_info <- data.frame(
      Column = names(data_storage$csv_data),
      Type = sapply(data_storage$csv_data, function(x) class(x)[1]),
      Unique_Values = sapply(data_storage$csv_data, function(x) length(unique(x))),
      Missing_Values = sapply(data_storage$csv_data, function(x) sum(is.na(x)))
    )
    datatable(column_info, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_preview <- renderDT({
    req(data_storage$csv_data)
    datatable(data_storage$csv_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$data_statistics <- renderUI({
    req(data_storage$csv_data)
    numeric_cols <- names(data_storage$csv_data)[sapply(data_storage$csv_data, is.numeric)]
    if (length(numeric_cols) > 0) {
      stats <- data_storage$csv_data %>%
        summarise(across(all_of(numeric_cols), list(Mean = mean, Median = median, Min = min, Max = max, SD = sd), na.rm = TRUE))
      HTML(paste("<h4>Numeric Column Statistics</h4><pre>", capture.output(print(stats)), "</pre>"))
    } else HTML("<p>No numeric columns found.</p>")
  })
  
  # Update Location Filter
  observe({
    df <- data_storage$csv_data
    updatePickerInput(session, "location_filter",
                      choices = unique(df$LOCATION_NAME),
                      selected = unique(df$LOCATION_NAME))
  })
  
  # Piper Plot
  output$piper_plot <- renderPlot({
    req(data_storage$csv_data)
    df <- data_storage$csv_data
    
    selected_locs <- input$location_filter
    if (!is.null(selected_locs)) {
      df <- df %>% filter(LOCATION_NAME %in% selected_locs)
    }
    
    required_cols <- c("CA", "MG", "Sodium", "K", "CHLORIDE", "SULPHATE", "BICARBONATE")
    if (!all(required_cols %in% colnames(df))) {
      showNotification("Required ions missing for Piper plot", type = "error")
      return(NULL)
    }
    
    to_meq <- function(mgL, mol_weight, valence) {
      (mgL / mol_weight) * valence
    }
    
    Ca <- to_meq(df$CA, 40.08, 2)
    Mg <- to_meq(df$MG, 24.31, 2)
    NaK <- to_meq(df$Sodium + df$K, 22.99, 1)
    Cl <- to_meq(df$CHLORIDE, 35.45, 1)
    SO4 <- to_meq(df$SULPHATE, 96.06, 2)
    HCO3 <- to_meq(df$BICARBONATE, 61.02, 1)
    
    sample_labels <- df$LOCATION_NAME
    
    piperPlot(Ca, Mg, NaK, Cl, SO4, HCO3,
              Plot = list(name = sample_labels),
              caption = "Piper Diagram – Groundwater Chemistry")
  })
}

# Run App
shinyApp(ui, server)
