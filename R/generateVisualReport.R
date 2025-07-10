#' Generate Visual Report
#'
#' This function scans the package for visual functions (prefixed with `visual_`),
#' creates a Quarto document to execute them, and renders it into an HTML or PDF report.
#'
#' @param data_list A list of data frames, typically the output from `process_country_bsi`.
#' @param output_file The name for the output report file.
#' @param output_dir The directory where the report will be saved. Defaults to the current working directory.
#' @param output_format The output format: "html" (default) or "pdf".
#'
#' @return The path to the generated report, invisibly.
#' @export
#' @importFrom quarto quarto_render
generate_visual_report <- function(data_list, output_file = NULL, output_dir = getwd(), output_format = "html") {

  # Set default output file based on format
  if (is.null(output_file)) {
    output_file <- paste0("visual-report.", output_format)
  }

  # Get the package name (hardcoded for this package)
  pkg_name <- "EHR-BSI"

  # Find all exported functions in the package that start with "visual_"
  all_exports <- ls(getNamespaceInfo(pkg_name, "exports"))
  plot_funcs <- all_exports[startsWith(all_exports, "visual_")]

  if (length(plot_funcs) == 0) {
    warning("No visual functions (with prefix 'visual_') found in the package. No report will be generated.")
    return(invisible(NULL))
  }

  # Save data to temporary RDS file for reliable transfer to Quarto
  temp_data_file <- tempfile(fileext = ".rds")
  saveRDS(data_list, temp_data_file)

  # Create the content for the temporary Quarto file
  qmd_header <- c(
    "---",
    "title: 'EHR-BSI Data Visualisation Report'",
    paste0("format: ", output_format),
    "execute:",
    "  echo: false",
    "  warning: false",
    "  error: true",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "# Load the package functions so they are available in the Quarto environment",
    paste0("devtools::load_all('", gsub("\\\\", "/", getwd()), "')"),
    "library(ggplot2)",
    "",
    "# Load data from temporary RDS file",
    paste0("data_list <- readRDS('", gsub("\\\\", "/", temp_data_file), "')"),
    "```",
    ""
  )

  # Generate a chunk for each plotting function
  plot_chunks <- lapply(plot_funcs, function(func_name) {
    # Create a nice title from the function name
    plot_title <- gsub("_", " ", gsub("visual_", "", func_name))
    plot_title <- paste0(toupper(substring(plot_title, 1, 1)), substring(plot_title, 2))

    c(
      paste("##", plot_title),
      "```{r}",
      "# Call the function and display result",
      paste0("plot_result <- `", pkg_name, "`:::", func_name, "(data_list)"),
      "",
      "if(inherits(plot_result, 'ggplot')) {",
      "  print(plot_result)",
      "} else {",
      "  cat('Error: Function did not return a ggplot object\\n')",
      "}",
      "```",
      ""
    )
  })

  # Write to a temporary .qmd file
  temp_qmd <- tempfile(fileext = ".qmd")
  qmd_content <- c(qmd_header, unlist(plot_chunks))
  writeLines(qmd_content, temp_qmd)

  # Render the Quarto document
  output_path <- file.path(output_dir, output_file)
  quarto::quarto_render(
    input = temp_qmd,
    output_file = output_file
  )

  # Clean up temporary files
  unlink(temp_data_file)
  unlink(temp_qmd)

  # Move the file to the correct directory if needed (Quarto renders to current dir)
  if(file.exists(output_file) && !file.exists(output_path) && output_dir != getwd()){
    file.rename(output_file, output_path)
  }

  message(paste("Visual report generated at:", output_path))
  return(invisible(output_path))
} 