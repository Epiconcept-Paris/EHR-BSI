#' Generate Visual Report
#'
#' This function scans the package for visual functions (prefixed with `visual_`),
#' creates a Shiny app to display them interactively, and launches the app.
#'
#' @param data_list A list of data frames, typically the output from `process_country_bsi`.
#' @param output_file Not used in Shiny version (kept for compatibility).
#' @param output_dir Not used in Shiny version (kept for compatibility).
#' @param output_format Not used in Shiny version (kept for compatibility).
#'
#' @return The Shiny app object, invisibly.
#' @export
generate_visual_report <- function(data_list, output_file = NULL, output_dir = getwd(), output_format = "html") {

  # Get the package name (hardcoded for this package)
  pkg_name <- "EHR-BSI"

  # Find all exported functions in the package that start with "visual_"
  all_exports <- ls(getNamespaceInfo(pkg_name, "exports"))
  plot_funcs <- all_exports[startsWith(all_exports, "visual_")]

  if (length(plot_funcs) == 0) {
    warning("No visual functions (with prefix 'visual_') found in the package. No Shiny app will be launched.")
    return(invisible(NULL))
  }

  # Create nice titles from function names
  plot_titles <- sapply(plot_funcs, function(func_name) {
    plot_title <- gsub("_", " ", gsub("visual_", "", func_name))
    paste0(toupper(substring(plot_title, 1, 1)), substring(plot_title, 2))
  })

  # Create the Shiny UI
  ui <- shiny::fluidPage(
    shiny::titlePanel("EHR-BSI Data Visualisation Report"),
    
    # Create tabs for each plot function
    do.call(shiny::tabsetPanel, c(
      list(id = "plots_tabs"),
      lapply(seq_along(plot_funcs), function(i) {
        shiny::tabPanel(
          title = plot_titles[i],
          shiny::plotOutput(paste0("plot_", i), height = "600px")
        )
      })
    ))
  )

  # Create the Shiny server
  server <- function(input, output, session) {
    # Create reactive plots for each visual function
    for (i in seq_along(plot_funcs)) {
      local({
        func_name <- plot_funcs[i]
        plot_id <- paste0("plot_", i)
        
        output[[plot_id]] <- shiny::renderPlot({
          tryCatch({
            # Get the function from the package namespace
            plot_func <- get(func_name, envir = getNamespace(pkg_name))
            
            # Call the function with the data
            plot_result <- plot_func(data_list)
            
            # Check if it's a ggplot object and return it
            if (inherits(plot_result, 'ggplot')) {
              plot_result
            } else {
              # Create an error plot if function doesn't return ggplot
              ggplot2::ggplot() + 
                ggplot2::annotate("text", x = 0.5, y = 0.5, 
                                 label = paste("Error: Function", func_name, "did not return a ggplot object"),
                                 size = 5) +
                ggplot2::theme_void()
            }
          }, error = function(e) {
            # Create an error plot
            ggplot2::ggplot() + 
              ggplot2::annotate("text", x = 0.5, y = 0.5, 
                               label = paste("Error in", func_name, ":", e$message),
                               size = 4) +
              ggplot2::theme_void()
          })
        })
      })
    }
  }

  # Create and launch the Shiny app
  app <- shiny::shinyApp(ui = ui, server = server)
  
  message("Launching Shiny app with visual report...")
  shiny::runApp(app)
  
  return(invisible(app))
} 