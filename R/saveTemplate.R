saveReportingTemplate <- function(result = NULL, country){
  # Create a new workbook
  wb <- createWorkbook()
  
  # Add each data frame as a new worksheet
  addWorksheet(wb, "EHRBSI")
  writeData(wb, sheet = "EHRBSI", result$ehrbsi)
  
  addWorksheet(wb, "Patient")
  writeData(wb, sheet = "Patient", result$patient)
  
  addWorksheet(wb, "Isolate")
  writeData(wb, sheet = "Isolate", result$isolate)
  
  addWorksheet(wb, "Res")
  writeData(wb, sheet = "Res", result$res)
  
  # Save the workbook to the working directory
  saveWorkbook(wb, paste0(country, "_EHRBSI_FullReportingTemplate.xlsx"), overwrite = TRUE) 
}