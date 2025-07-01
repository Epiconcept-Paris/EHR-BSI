#' Recode HAI long names to HAI short codes
#'
#' @param data Data frame containing HAI long antibiotic names
#' @param metadata_path Path to the metadata Excel file containing coded values
#' @param long_col Column name containing HAI long names
#' @param short_col Column name to create for HAI short codes
#' @param sheet Sheet name in the Excel file
#' @param coded_list_id Identifier for the antibiotic coded list
#'
#' @return Data frame with HAI short codes added
#' @keywords internal
recode_to_HAI_short <- function(data,
                                metadata_path,
                                long_col = "Antibiotic",
                                short_col = "Antibiotic",
                                sheet = "Coded values",
                                coded_list_id = "AntibioticHAI") {
  
  if (is.null(metadata_path) || !file.exists(metadata_path)) {
    warning("Metadata path not provided or file does not exist. Skipping HAI short code conversion.")
    return(data)
  }
  
  # Ensure required packages are available
  requireNamespace("readxl", quietly = TRUE)
  
  # Build long->short named vector from the Excel file
  lookup <- readxl::read_excel(
    path = metadata_path,
    sheet = sheet,
    col_types = "text"
  ) %>% 
    dplyr::filter(`Coded value list` == coded_list_id) %>% 
    dplyr::distinct(Description, `Coded value`)
  
  HAIlong2short_vec <- setNames(lookup$`Coded value`, lookup$Description)
  
  # Recode the long names to short codes
  out <- data %>% 
    dplyr::mutate(
      !!short_col := dplyr::recode(.data[[long_col]], !!!HAIlong2short_vec, .default = .data[[long_col]])
    )
  
  # Optional warning for unmapped long names
  unmapped <- setdiff(unique(out[[long_col]]), names(HAIlong2short_vec))
  if (length(unmapped) > 0) {
    warning("Unmapped HAI long names: ",
            paste(unmapped, collapse = ", "),
            call. = FALSE)
  }
  
  return(out)
} 