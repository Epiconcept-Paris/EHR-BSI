
#' Shiny-based Interactive BSI Dashboard
#'
#' This function creates a Shiny app with full interactive controls for
#' exploring BSI data with dropdowns, sliders, and filters. The app now includes
#' data upload functionality for both raw BSI data and reporting templates.
#'
#' @param data A list of data.frames. Expected to have an `ehrbsi` data.frame. 
#'   Optional - if not provided, users can upload data through the app interface.
#' @return Launches a Shiny application.
#' @details The app supports:
#' \itemize{
#'   \item Upload raw BSI data (CSV/Excel) for processing with country-specific transformations
#'   \item Upload reporting template Excel files with EHRBSI, Patient, Isolate, and Res sheets
#'   \item Interactive visualization with customizable axes, colors, and filters
#'   \item Data table exploration and summary statistics
#' }
#' @import shiny
#' @import ggplot2
#' @import readxl
#' @export
visual_bsi_dashboard <- function(data = NULL) {
  
  # Increase maximum upload size to 100MB
  options(shiny.maxRequestSize = 100*1024^2)
  
  # Define UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Interactive BSI Data Explorer"),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # Data Upload Section
        shiny::h4("Data Upload"),
        shiny::fileInput("data_file", "Choose File",
                        accept = c(".xlsx", ".csv"),
                        multiple = FALSE),
        
        shiny::radioButtons("file_type", "File Type:",
                           choices = list("Raw Data" = "raw", 
                                        "Reporting Template" = "template"),
                           selected = "raw"),
        
        shiny::conditionalPanel(
          condition = "input.file_type == 'raw'",
          shiny::radioButtons("country", "Country:",
                             choices = list("Estonia (EE)" = "EE", 
                                          "Malta (MT)" = "MT"),
                             selected = "EE")
        ),
        
        shiny::actionButton("process_data", "Process Data", 
                           class = "btn-primary"),
        
        shiny::hr(),
        
        # Existing controls
        shiny::h4("Visualization Controls"),
        shiny::selectInput("x_var", "X-Axis Variable:",
                          choices = c("HospitalId", "LaboratoryCode", "GeoLocation", 
                                    "HospitalSize", "HospitalType", "ReportingCountry"),
                          selected = "HospitalId"),
        
        shiny::selectInput("y_var", "Y-Axis Variable:",
                          choices = c("NumberOfTotalBSIs", "NumberOfHOHABSIs", 
                                    "NumberOfCABSIs", "NumberOfImportedHABSIs",
                                    "NumberOfBloodCultureSets", "NumberOfHospitalDischarges"),
                          selected = "NumberOfTotalBSIs"),
        
        shiny::selectInput("color_var", "Color Bars By:",
                          choices = c("steelblue", "darkgreen", "red", "purple", "orange"),
                          selected = "steelblue"),
        
        shiny::checkboxInput("show_values", "Show Values on Bars", value = TRUE),
        
        shiny::sliderInput("text_size", "Text Size:", min = 2, max = 6, value = 3, step = 0.5),
        
        shiny::hr(),
        shiny::h4("Filter Data:"),
        shiny::conditionalPanel(
          condition = "input.x_var == 'HospitalType'",
          shiny::checkboxGroupInput("hospital_type_filter", "Hospital Types:",
                                   choices = c(),
                                   selected = c())
        )
      ),
      
      shiny::mainPanel(
        # Status messages
        shiny::conditionalPanel(
          condition = "!output.data_available",
          shiny::div(
            style = "text-align: center; margin-top: 50px;",
            shiny::h3("No Data Available"),
            shiny::p("Please upload a file and process it to begin exploring BSI data.")
          )
        ),
        
        shiny::conditionalPanel(
          condition = "output.data_available",
          shiny::tabsetPanel(
            shiny::tabPanel("Plot", shiny::plotOutput("main_plot", height = "600px")),
            shiny::tabPanel("Data Table", shiny::dataTableOutput("data_table")),
            shiny::tabPanel("Summary", shiny::verbatimTextOutput("summary"))
          )
        )
      )
    )
  )
  
  # Define Server
  server <- function(input, output, session) {
    
    # Reactive values to store data
    values <- shiny::reactiveValues(
      current_data = data,
      processing = FALSE
    )
    
    # Check if data is available
    output$data_available <- shiny::reactive({
      !is.null(values$current_data) && !is.null(values$current_data$ehrbsi)
    })
    shiny::outputOptions(output, "data_available", suspendWhenHidden = FALSE)
    
    # Update filter choices when data changes
    shiny::observe({
      if (!is.null(values$current_data) && !is.null(values$current_data$ehrbsi)) {
        # Update hospital type filter choices
        hospital_types <- unique(values$current_data$ehrbsi$HospitalType)
        hospital_types <- hospital_types[!is.na(hospital_types)]
        
        shiny::updateCheckboxGroupInput(session, "hospital_type_filter",
                                       choices = hospital_types,
                                       selected = hospital_types)
      }
    })
    
    # Process uploaded data
    shiny::observeEvent(input$process_data, {
      shiny::req(input$data_file)
      
      values$processing <- TRUE
      
      tryCatch({
        file_path <- input$data_file$datapath
        file_ext <- tools::file_ext(input$data_file$name)
        
        if (input$file_type == "raw") {
          # Process raw data using process_country_bsi
          shiny::showNotification("Processing raw data...", type = "message", duration = NULL, id = "processing")
          
          # Read the uploaded data
          if (file_ext %in% c("xlsx", "xls")) {
            raw_data <- readxl::read_xlsx(file_path)
          } else {
            raw_data <- read.csv(file_path)
          }
          
          # Process the data
          result <- process_country_bsi(
            country = input$country,
            input_data = raw_data,
            episode_duration = 14,
            write_to_file = FALSE,
            calculate_episodes = TRUE
          )
          
          values$current_data <- result
          shiny::removeNotification("processing")
          shiny::showNotification("Raw data processed successfully!", type = "success", duration = 3)
          
        } else {
          # Process reporting template
          shiny::showNotification("Loading reporting template...", type = "message", duration = NULL, id = "processing")
          
          if (file_ext %in% c("xlsx", "xls")) {
            # Read Excel file with multiple sheets
            tryCatch({
              # Try to read all expected sheets
              ehrbsi <- readxl::read_xlsx(file_path, sheet = "EHRBSI")
              patient <- readxl::read_xlsx(file_path, sheet = "Patient")
              isolate <- readxl::read_xlsx(file_path, sheet = "Isolate")
              res <- readxl::read_xlsx(file_path, sheet = "Res")
              
              values$current_data <- list(
                ehrbsi = as.data.frame(ehrbsi),
                patient = as.data.frame(patient),
                isolate = as.data.frame(isolate),
                res = as.data.frame(res)
              )
              
              shiny::removeNotification("processing")
              shiny::showNotification("Reporting template loaded successfully!", type = "success", duration = 3)
              
            }, error = function(e) {
              # If sheet reading fails, try reading first sheet only
              warning_msg <- paste("Could not read expected sheets. Error:", e$message)
              shiny::showNotification(warning_msg, type = "warning", duration = 5)
              
              # Try reading first sheet as ehrbsi data
              data_sheet <- readxl::read_xlsx(file_path, sheet = 1)
              values$current_data <- list(ehrbsi = as.data.frame(data_sheet))
              
              shiny::removeNotification("processing")
              shiny::showNotification("Loaded first sheet as EHRBSI data", type = "warning", duration = 3)
            })
          } else {
            shiny::removeNotification("processing")
            shiny::showNotification("Reporting template must be an Excel file (.xlsx)", type = "error", duration = 5)
          }
        }
        
      }, error = function(e) {
        shiny::removeNotification("processing")
        error_msg <- paste("Error processing file:", e$message)
        shiny::showNotification(error_msg, type = "error", duration = 10)
      })
      
      values$processing <- FALSE
    })
    
    # Reactive data filtering
    filtered_data <- shiny::reactive({
      shiny::req(values$current_data, values$current_data$ehrbsi)
      
      plot_data <- values$current_data$ehrbsi
      
      # Remove NAs for selected variables
      if (input$x_var %in% names(plot_data) && input$y_var %in% names(plot_data)) {
        plot_data <- plot_data[!is.na(plot_data[[input$x_var]]) & !is.na(plot_data[[input$y_var]]), ]
      }
      
      # Apply filters if available
      if (input$x_var == "HospitalType" && !is.null(input$hospital_type_filter) && length(input$hospital_type_filter) > 0) {
        plot_data <- plot_data[plot_data$HospitalType %in% input$hospital_type_filter, ]
      }
      
      return(plot_data)
    })
    
    # Main plot
    output$main_plot <- shiny::renderPlot({
      shiny::req(values$current_data, values$current_data$ehrbsi)
      
      plot_data <- filtered_data()
      
      if (nrow(plot_data) == 0) {
        return(ggplot2::ggplot() + 
               ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data to display", size = 6) +
               ggplot2::theme_void())
      }
      
      # Check if selected variables exist in data
      if (!input$x_var %in% names(plot_data) || !input$y_var %in% names(plot_data)) {
        return(ggplot2::ggplot() + 
               ggplot2::annotate("text", x = 0.5, y = 0.5, 
                               label = paste("Selected variables not found in data.\nAvailable columns:", 
                                           paste(names(plot_data), collapse = ", ")), 
                               size = 4) +
               ggplot2::theme_void())
      }
      
      # Generate labels
      x_label <- gsub("([a-z])([A-Z])", "\\1 \\2", input$x_var)
      y_label <- gsub("([a-z])([A-Z])", "\\1 \\2", input$y_var)
      title <- paste(y_label, "by", x_label)
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = input$x_var, y = input$y_var)) +
        ggplot2::geom_col(fill = input$color_var, alpha = 0.8) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = title, x = x_label, y = y_label)
      
      if (input$show_values && is.numeric(plot_data[[input$y_var]])) {
        p <- p + ggplot2::geom_text(ggplot2::aes_string(label = input$y_var), 
                                   vjust = -0.5, size = input$text_size)
      }
      
      return(p)
    })
    
    # Data table
    output$data_table <- shiny::renderDataTable({
      shiny::req(values$current_data, values$current_data$ehrbsi)
      filtered_data()
    }, options = list(scrollX = TRUE))
    
    # Summary
    output$summary <- shiny::renderText({
      shiny::req(values$current_data, values$current_data$ehrbsi)
      
      plot_data <- filtered_data()
      
      if (nrow(plot_data) == 0) {
        return("No data to summarize")
      }
      
      if (!input$y_var %in% names(plot_data) || !is.numeric(plot_data[[input$y_var]])) {
        return(paste(
          "Dataset Summary:\n",
          "Total Records:", nrow(plot_data), "\n",
          "Available columns:", paste(names(plot_data), collapse = ", ")
        ))
      }
      
      paste(
        "Dataset Summary:\n",
        "Total Records:", nrow(plot_data), "\n",
        "X Variable (", input$x_var, ") unique values:", length(unique(plot_data[[input$x_var]])), "\n",
        "Y Variable (", input$y_var, ") range:", 
        min(plot_data[[input$y_var]], na.rm = TRUE), "to", 
        max(plot_data[[input$y_var]], na.rm = TRUE), "\n",
        "Total sum:", sum(plot_data[[input$y_var]], na.rm = TRUE)
      )
    })
  }
  
  # Launch the app
  shiny::shinyApp(ui = ui, server = server)
}



