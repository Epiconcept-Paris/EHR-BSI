
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
  # Check if ehrbsi table exists and has data
  if (!is.null(data$ehrbsi) && is.data.frame(data$ehrbsi) && nrow(data$ehrbsi) > 0) {
    # Check if required columns exist
    if ("HospitalId" %in% names(data$ehrbsi) && "NumberOfHOHABSIs" %in% names(data$ehrbsi)) {
      # Filter out NA values and create the plot
      plot_data <- data$ehrbsi[!is.na(data$ehrbsi$NumberOfHOHABSIs), ]
      
      if (nrow(plot_data) > 0) {
        ggplot2::ggplot(plot_data, ggplot2::aes(x = HospitalId, y = NumberOfHOHABSIs)) +
          ggplot2::geom_col(fill = "steelblue", alpha = 0.8) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
          ggplot2::labs(
            title = "Hospital-Onset Healthcare-Associated BSIs by Hospital",
            x = "Hospital ID",
            y = "Number of HO-HA BSIs",
            caption = "HO-HA = Hospital-Onset Healthcare-Associated"
          ) +
          ggplot2::geom_text(ggplot2::aes(label = NumberOfHOHABSIs), 
                            vjust = -0.5, size = 3)
      } else {
        # No valid data after filtering
        ggplot2::ggplot(data.frame(x = "No Valid Data", y = 1), ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_col(fill = "grey") +
          ggplot2::ggtitle("No HO-HA BSI Data Available") +
          ggplot2::labs(y = "Status", x = "Data Availability")
      }
    } else {
      # Required columns missing
      missing_cols <- setdiff(c("HospitalId", "NumberOfHOHABSIs"), names(data$ehrbsi))
      ggplot2::ggplot(data.frame(x = "Missing Columns", y = 1), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_col(fill = "orange") +
        ggplot2::ggtitle(paste("Missing required columns:", paste(missing_cols, collapse = ", "))) +
        ggplot2::labs(y = "Status", x = "Data Issue")
    }
  } else {
    # No ehrbsi table available
    ggplot2::ggplot(data.frame(x = "No EHRBSI Data", y = 1), ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_col(fill = "red") +
      ggplot2::ggtitle("No EHRBSI Data Available") +
      ggplot2::labs(y = "Status", x = "Data Availability")
  }
}



