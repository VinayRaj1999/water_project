# Load Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(leaflet)
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

# Custom Color Palette
iisc_palette <- c(
  primary = "#005A9C",
  secondary = "#00A86B",
  accent = "#4FC3F7",
  background = "#F0F4F8",
  text = "#2C3E50"
)

# WQI Calculation Function
calculate_wqi <- function(gpkg_path) {
  layer_name <- st_layers(gpkg_path)$name[1]
  water_sf <- st_read(gpkg_path, layer = layer_name, quiet = TRUE)
  if (st_crs(water_sf)$epsg != 4326) water_sf <- st_transform(water_sf, 4326)
  if (!all(st_is_valid(water_sf))) water_sf <- st_make_valid(water_sf)
  if ("NA" %in% names(water_sf)) names(water_sf)[names(water_sf) == "NA"] <- "Sodium"
  
  standards <- list(
    TDS = list(St = 1000, Wi = 0.121),
    EC = list(St = 2500, Wi = 0.121),
    NITRATE = list(St = 50, Wi = 0.152),
    SULPHATE = list(St = 250, Wi = 0.121),
    CHLORIDE = list(St = 250, Wi = 0.093),
    BICARBONATE = list(St = 500, Wi = 0.152),
    FLUORIDE = list(St = 1.2, Wi = 0.030),
    CA = list(St = 100, Wi = 0.060),
    MG = list(St = 50, Wi = 0.060),
    Sodium = list(St = 200, Wi = 0.060),
    K = list(St = 20, Wi = 0.030)
  )
  
  water_sf$WQI <- NA_real_
  param_names <- names(standards)
  for (param in param_names) {
    if (!param %in% names(water_sf)) water_sf[[param]] <- 0
    else water_sf[[param]][is.na(water_sf[[param]])] <- 0
  }
  
  for (param in param_names) {
    qi_col <- paste0("qi_", param)
    sli_col <- paste0("SLi_", param)
    water_sf[[qi_col]] <- water_sf[[param]] / standards[[param]]$St
    water_sf[[sli_col]] <- water_sf[[qi_col]] * standards[[param]]$Wi
  }
  
  sli_cols <- paste0("SLi_", param_names)
  sli_values <- st_drop_geometry(water_sf)[, sli_cols]
  sli_values[] <- lapply(sli_values, as.numeric)
  water_sf$WQI <- rowSums(sli_values, na.rm = TRUE)
  water_sf$Quality <- cut(
    water_sf$WQI,
    breaks = c(-Inf, 0.5, 1, 2, 3, Inf),
    labels = c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable"),
    right = FALSE
  )
  
  return(water_sf)
}

# UI
ui <- dashboardPage(
  title = "Ground Water Assessment Dashboard",
  skin = "blue",
  
  dashboardHeader(
    title = tags$div(
      style = "display: flex; align-items: center; height: 60px;",
      tags$span("Ground Water Assessment Dashboard",
                style = "color: white; font-weight: bold; font-size: 18px; margin-left: 30px;")
    ),
    titleWidth = 650
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📤 File Upload", tabName = "upload", icon = icon("file-upload")),
      menuItem("📊 Data Exploration", tabName = "data_explore", icon = icon("table")),
      menuItem("🧪 Ground Water Chemistry", tabName = "chemistry", icon = icon("flask"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML(sprintf('
        .skin-blue .main-header .logo { background-color: %s; color: white; }
        .skin-blue .main-header .navbar { background-color: %s; }
        body { background-color: %s; color: %s; }
        .box { border-top-color: %s; }
      ', iisc_palette["primary"], iisc_palette["secondary"],
                              iisc_palette["background"], iisc_palette["text"],
                              iisc_palette["accent"])))),
    
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload GPKG File", status = "primary", solidHeader = TRUE,
                    fileInput("gpkg_upload", "Choose GPKG File", accept = c(".gpkg")),
                    actionButton("load_data", "Load & Calculate WQI", icon = icon("calculator"))),
                box(title = "File Info", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("file_info"))
              )),
      
      tabItem(tabName = "data_explore",
              fluidRow(
                box(title = "Dataset Summary", status = "info", solidHeader = TRUE,
                    uiOutput("dataset_summary")),
                box(title = "Column Details", status = "warning", solidHeader = TRUE,
                    DTOutput("column_details"))
              ),
              fluidRow(
                box(title = "Preview Data", status = "success", solidHeader = TRUE,
                    DTOutput("data_preview")),
                box(title = "Numeric Statistics", status = "primary", solidHeader = TRUE,
                    uiOutput("data_statistics"))
              )),
      
      tabItem(tabName = "chemistry",
              fluidRow(
                box(title = "Filters", status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, uiOutput("state_ui")),
                      column(4, uiOutput("district_ui")),
                      column(4, uiOutput("block_ui"))
                    ))
              ),
              fluidRow(
                box(title = "Piper Plot", status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("piper_plot"), type = 6))
              ),
              fluidRow(
                box(title = "Water Types", status = "warning", solidHeader = TRUE, width = 12,
                    uiOutput("water_types_section"))
              ))
    )
  )
)
# Server
server <- function(input, output, session) {
  data_storage <- reactiveValues(csv_data = NULL, sf_data = NULL, water_sf = NULL)
  
  # Load GPKG and calculate WQI
  observeEvent(input$load_data, {
    req(input$gpkg_upload)
    tryCatch({
      water_sf <- calculate_wqi(input$gpkg_upload$datapath)
      data_storage$csv_data <- st_drop_geometry(water_sf)
      data_storage$sf_data <- water_sf
      data_storage$water_sf <- water_sf
      
      showNotification("GPKG loaded and WQI calculated successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # File Info Output
  output$file_info <- renderPrint({
    req(data_storage$csv_data)
    cat("Rows:", nrow(data_storage$csv_data), "\n")
    cat("Columns:", ncol(data_storage$csv_data), "\n")
    cat("Column Names:\n")
    print(colnames(data_storage$csv_data))
  })
  
  # Dataset Summary
  output$dataset_summary <- renderUI({
    req(data_storage$csv_data)
    HTML(paste(
      "<strong>Total Rows:</strong>", nrow(data_storage$csv_data), "<br>",
      "<strong>Total Columns:</strong>", ncol(data_storage$csv_data)
    ))
  })
  
  # Column Summary Table
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
  
  # Data Preview Table
  output$data_preview <- renderDT({
    req(data_storage$csv_data)
    datatable(data_storage$csv_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Numeric Statistics Summary
  output$data_statistics <- renderUI({
    req(data_storage$csv_data)
    numeric_cols <- names(data_storage$csv_data)[sapply(data_storage$csv_data, is.numeric)]
    if (length(numeric_cols) > 0) {
      stats <- data_storage$csv_data %>%
        select(all_of(numeric_cols)) %>%
        summarise(across(everything(), list(Mean = mean, Median = median, Min = min, Max = max, SD = sd), na.rm = TRUE))
      HTML(paste("<pre>", capture.output(print(stats)), "</pre>"))
    } else {
      HTML("No numeric columns found.")
    }
  })
  
  # UI Dropdowns for Chemistry Tab
  output$state_ui <- renderUI({
    req(data_storage$csv_data)
    selectInput("state_chem", "State/UT:", choices = unique(data_storage$csv_data$STATE_UT))
  })
  
  output$district_ui <- renderUI({
    req(data_storage$csv_data, input$state_chem)
    districts <- unique(data_storage$csv_data[data_storage$csv_data$STATE_UT == input$state_chem, "DISTRICT"])
    selectInput("district_chem", "District:", choices = districts)
  })
  
  output$block_ui <- renderUI({
    req(data_storage$csv_data, input$district_chem)
    blocks <- unique(data_storage$csv_data[data_storage$csv_data$DISTRICT == input$district_chem, "BLOCK"])
    selectInput("block_chem", "Block:", choices = blocks)
  })
  # Reactive for Chemistry Processing
  chemistry_data <- reactive({
    req(data_storage$csv_data, input$state_chem, input$district_chem, input$block_chem)
    df <- data_storage$csv_data %>%
      filter(STATE_UT == input$state_chem,
             DISTRICT == input$district_chem,
             BLOCK == input$block_chem) %>%
      mutate(
        CA = as.numeric(CA),
        MG = as.numeric(MG),
        Sodium = as.numeric(Sodium),
        CHLORIDE = as.numeric(CHLORIDE),
        SULPHATE = as.numeric(SULPHATE),
        BICARBONATE = as.numeric(BICARBONATE)
      ) %>%
      drop_na(CA, MG, Sodium, CHLORIDE, SULPHATE, BICARBONATE)
    
    if (nrow(df) == 0) return(NULL)
    
    df <- transform(df,
                    Ca.meq = conc2meq(CA, "calcium"),
                    Mg.meq = conc2meq(MG, "magnesium"),
                    Na.meq = conc2meq(Sodium, "sodium"),
                    Cl.meq = conc2meq(CHLORIDE, "chloride"),
                    SO4.meq = conc2meq(SULPHATE, "sulfate"),
                    HCO3.meq = conc2meq(BICARBONATE, "bicarb"))
    
    df <- df %>%
      mutate(
        total_cations = Ca.meq + Mg.meq + Na.meq,
        total_anions = Cl.meq + SO4.meq + HCO3.meq,
        Ca_pct = 100 * Ca.meq / total_cations,
        Mg_pct = 100 * Mg.meq / total_cations,
        Na_pct = 100 * Na.meq / total_cations,
        Cl_pct = 100 * Cl.meq / total_anions,
        SO4_pct = 100 * SO4.meq / total_anions,
        HCO3_pct = 100 * HCO3.meq / total_anions,
        cation_type = case_when(
          Ca_pct + Mg_pct > 50 ~ "Hard",
          Na_pct > 50 ~ "Alkali",
          TRUE ~ "Mixed"
        ),
        anion_type = case_when(
          HCO3_pct > 50 ~ "Carbonate",
          Cl_pct + SO4_pct > 50 ~ "Non-carbonate",
          TRUE ~ "Mixed"
        ),
        water_type = case_when(
          cation_type == "Hard" & anion_type == "Non-carbonate" ~ "Permanent Hardness",
          cation_type == "Hard" & anion_type == "Carbonate" ~ "Temporary Hardness",
          cation_type == "Alkali" & anion_type == "Carbonate" ~ "Alkali Carbonates",
          cation_type == "Alkali" & anion_type == "Non-carbonate" ~ "Saline",
          TRUE ~ "Mixed Type"
        )
      )
    
    return(df)
  })
  
  # Piper Plot Output
  output$piper_plot <- renderPlot({
    df <- chemistry_data()
    req(df)
    
    water_colors <- c(
      "Permanent Hardness" = "red",
      "Temporary Hardness" = "blue",
      "Alkali Carbonates" = "green",
      "Saline" = "purple",
      "Mixed Type" = "orange"
    )
    df$color <- water_colors[df$water_type]
    
    with(df, piperPlot(
      Ca.meq, Mg.meq, Na.meq,
      Cl.meq, HCO3.meq, SO4.meq,
      Plot = list(name = water_type, color = color),
      xAn.title = "Chloride"
    ))
    legend("topright", legend = names(water_colors), col = water_colors, pch = 16)
  })
  
  # Water Types Output
  output$water_types_section <- renderUI({
    df <- chemistry_data()
    if (is.null(df)) {
      HTML("<strong>No valid chemistry data available.</strong>")
    } else {
      types <- unique(df$water_type)
      HTML(paste("<strong>Identified Water Types:</strong><br>", paste(types, collapse = "<br>")))
    }
  })
  
  # Optional: Report download (PDF with plot)
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("ground_water_chemistry_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      req(chemistry_data())
      tempReport <- file.path(tempdir(), "chemistry_report.Rmd")
      file.copy("chemistry_report.Rmd", tempReport, overwrite = TRUE)
      
      df <- chemistry_data()
      df$color <- c(
        "Permanent Hardness" = "red",
        "Temporary Hardness" = "blue",
        "Alkali Carbonates" = "green",
        "Saline" = "purple",
        "Mixed Type" = "orange"
      )[df$water_type]
      
      plot_file <- file.path(tempdir(), "chem_plot.png")
      png(plot_file, width = 1000, height = 800, res = 150)
      with(df, piperPlot(Ca.meq, Mg.meq, Na.meq,
                         Cl.meq, HCO3.meq, SO4.meq,
                         Plot = list(name = water_type, color = color)))
      dev.off()
      
      params <- list(
        df = df,
        state = input$state_chem,
        district = input$district_chem,
        block = input$block_chem,
        plot_file = plot_file
      )
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params, envir = new.env(parent = globalenv()))
    }
  )
}

# Run App
shinyApp(ui, server)
