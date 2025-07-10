
#' Shiny-based Interactive BSI Dashboard
#'
#' This function creates a Shiny app with full interactive controls for
#' exploring BSI data with dropdowns, sliders, and filters.
#'
#' @param data A list of data.frames. Expected to have an `ehrbsi` data.frame.
#' @return Launches a Shiny application.
#' @import shiny
#' @import ggplot2
#' @import DT
#' @export
visual_bsi_dashboard <- function(data) {
  
  # Define UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("Interactive BSI Data Explorer"),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
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
                                   choices = unique(data$ehrbsi$HospitalType),
                                   selected = unique(data$ehrbsi$HospitalType))
        )
      ),
      
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Plot", shiny::plotOutput("main_plot", height = "600px")),
          shiny::tabPanel("Data Table", DT::dataTableOutput("data_table")),
          shiny::tabPanel("Summary", shiny::verbatimTextOutput("summary"))
        )
      )
    )
  )
  
  # Define Server
  server <- function(input, output, session) {
    
    # Reactive data filtering
    filtered_data <- shiny::reactive({
      plot_data <- data$ehrbsi
      
      # Remove NAs for selected variables
      plot_data <- plot_data[!is.na(plot_data[[input$x_var]]) & !is.na(plot_data[[input$y_var]]), ]
      
      # Apply filters if available
      if (input$x_var == "HospitalType" && !is.null(input$hospital_type_filter)) {
        plot_data <- plot_data[plot_data$HospitalType %in% input$hospital_type_filter, ]
      }
      
      return(plot_data)
    })
    
    # Main plot
    output$main_plot <- shiny::renderPlot({
      plot_data <- filtered_data()
      
      # Generate labels
      x_label <- gsub("([a-z])([A-Z])", "\\1 \\2", input$x_var)
      y_label <- gsub("([a-z])([A-Z])", "\\1 \\2", input$y_var)
      title <- paste(y_label, "by", x_label)
      
      p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = input$x_var, y = input$y_var)) +
        ggplot2::geom_col(fill = input$color_var, alpha = 0.8) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = title, x = x_label, y = y_label)
      
      if (input$show_values) {
        p <- p + ggplot2::geom_text(ggplot2::aes_string(label = input$y_var), 
                                   vjust = -0.5, size = input$text_size)
      }
      
      return(p)
    })
    
    # Data table
    output$data_table <- DT::renderDataTable({
      DT::datatable(filtered_data(), options = list(scrollX = TRUE))
    })
    
    # Summary
    output$summary <- shiny::renderText({
      plot_data <- filtered_data()
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



