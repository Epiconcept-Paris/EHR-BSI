
#' Hospital-Onset BSI Distribution by Hospital
#'
#' This visual function creates a bar chart showing the number of Hospital-Onset 
#' Healthcare-Associated BSIs (HO-HA BSIs) per hospital.
#'
#' @param data A list of data.frames. Expected to have an `ehrbsi` data.frame 
#'   with `HospitalId` and `NumberOfHOHABSIs` columns.
#' @return A ggplot object for rendering.
#' @import ggplot2
#' @export
visual_ho_bsi_by_hospital <- function(data) {
  # Filter out NA values and create the plot
  plot_data <- data$ehrbsi[!is.na(data$ehrbsi$NumberOfHOHABSIs), ]
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = HospitalId, y = NumberOfHOHABSIs)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(
      title = "Hospital-Onset BSIs by Hospital ID",
      x = "Hospital ID",
      y = "Number of HO-HA BSIs",
      caption = "HO-HA = Hospital-Onset"
    ) +
    ggplot2::geom_text(ggplot2::aes(label = NumberOfHOHABSIs), 
                      vjust = -0.5, size = 3)
}



