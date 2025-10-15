#' EHR-BSI Country Config Wizard (Shiny)
#'
#' A guided assistant to create or migrate country configuration files
#' with minimal manual editing. Supports schema v2 export.
#'
#' @export
config_wizard_app <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("EHR-BSI Country Config Wizard"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("sample", "Upload small sample (CSV/XLSX)", accept = c(".csv", ".xlsx", ".xls")),
        shiny::textInput("country", "Country code", value = "XX"),
        shiny::selectInput("ab_profile", "Antibiotic profile",
                           choices = c("malta_wide", "estonia_long", "custom"),
                           selected = "malta_wide"),
        shiny::actionButton("detect", "Auto-detect & suggest"),
        shiny::actionButton("validate", "Validate config"),
        shiny::actionButton("export", "Export v2 Excel")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel(
            "Column Mapping",
            shiny::p("Map raw columns to standard names. Auto-suggests based on heuristics; you can override."),
            shiny::uiOutput("mapping_controls"),
            shiny::hr(),
            shiny::uiOutput("mapping_ui"),
            shiny::hr(),
            shiny::verbatimTextOutput("mapping_summary")
          ),
          shiny::tabPanel(
            "Lookups",
            shiny::uiOutput("lookups_ui"),
            shiny::hr(),
            shiny::verbatimTextOutput("lookups_summary")
          ),
          shiny::tabPanel("IDs & Dates", shiny::uiOutput("ids_dates_ui")),
          shiny::tabPanel("Preview", shiny::verbatimTextOutput("preview")),
          shiny::tabPanel("Validation", shiny::verbatimTextOutput("val_out"))
        )
      )
    )
  )
  server <- function(input, output, session) {
    state <- shiny::reactiveValues(
      sample = NULL,
      excel_kv = list(),
      suggestions = list(),
      dictionary_map = NULL,   # data.frame raw -> standard
      lookups = list()         # list of data.frames with lookup_name/from_value/to_value
    )
    shiny::observeEvent(input$sample, {
      shiny::req(input$sample)
      path <- input$sample$datapath
      ext <- tools::file_ext(input$sample$name)
      df <- tryCatch({
        if (tolower(ext) %in% c("xlsx", "xls")) readxl::read_xlsx(path) else utils::read.csv(path)
      }, error = function(e) NULL)
      state$sample <- df
    })
    shiny::observeEvent(input$detect, {
      # Minimal heuristic suggestions (placeholders): set ab profile and basic keys
      kv <- state$excel_kv
      kv$schema_version <- 2
      kv$`antibiotic.profile` <- input$ab_profile
      kv$`date.format` <- "auto"
      # detect has_time and likely date columns from sample
      has_time_auto <- FALSE
      date_cols <- c("DateOfSpecCollection", "DateOfHospitalAdmission")
      df <- state$sample
      if (!is.null(df)) {
        cn <- names(df)
        guess <- cn[grepl("date|time|spec|adm|disch", cn, ignore.case = TRUE)]
        if (length(guess) > 0) date_cols <- unique(c(date_cols, guess))
        # time detection: any POSIXct or strings with ':' in likely date fields
        for (nm in intersect(cn, guess)) {
          v <- df[[nm]]
          if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) has_time_auto <- TRUE
          if (is.character(v) && any(grepl("\\d+:\\d+", v[seq_len(min(50, length(v)))]))) has_time_auto <- TRUE
        }
      }
      kv$has_time <- if (has_time_auto) TRUE else "auto"
      kv$`record_ids.bsi` <- "{HospitalId}-{year(DateOfSpecCollection)}"
      kv$`record_ids.patient` <- "{PatientId}-{date(DateOfHospitalAdmission)}"
      kv$`record_ids.isolate` <- "{IsolateId}_{MicroorganismCode}"
      kv$`lookups.include` <- "auto"
      # antibiotic profile-specific suggestions
      if (identical(input$ab_profile, "malta_wide")) {
        kv$`antibiotic.prefix` <- "ab_"
      } else if (identical(input$ab_profile, "estonia_long")) {
        kv$`antibiotic.test_column` <- "sensitivityTest_noncdm"
        kv$`antibiotic.result_column` <- "sensitivityResult_noncdm"
        kv$`antibiotic.value_column` <- "sensitivityValue_noncdm"
        kv$`antibiotic.unit_column` <- "sensitivityUnit_noncdm"
      }
      state$excel_kv <- kv
      shiny::showNotification("Suggestions applied. See Preview tab or Export.", type = "message", duration = 3)
    })
    output$mapping_ui <- shiny::renderUI({
      df <- state$sample
      if (is.null(df)) return(shiny::helpText("Upload a small sample to map columns."))
      # Candidate standard names (union of tables + some helper/noncdm fields)
      std_fun <- get0("get_standard_table_columns")
      std_names <- c()
      if (!is.null(std_fun)) {
        std_names <- unique(c(
          std_fun("patient"), std_fun("isolate"), std_fun("ehrbsi"), std_fun("res")
        ))
      }
      extra_names <- c(
        "HospitalId", "PatientId", "IsolateId", "MicroorganismCode", "MicroorganismCodeLabel",
        "DateOfSpecCollection", "DateOfHospitalAdmission", "DateOfHospitalDischarge",
        "UnitSpecialtyShort_noncdm", "OutcomeOfCase_noncdm", "sensitivityTest_noncdm",
        "sensitivityResult_noncdm", "sensitivityValue_noncdm", "sensitivityUnit_noncdm",
        "EpisodeStartDate_noncdm", "PreviousAdmission_noncdm", "patientType_noncdm"
      )
      candidates <- sort(unique(c(std_names, extra_names)))
      # Heuristic guesser
      norm <- function(x) tolower(gsub("[^a-z0-9]", "", x))
      guess_for <- function(raw_name) {
        rn <- norm(raw_name)
        pattern_map <- list(
          HospitalId = c("hospital", "facility", "hospid", "hosp"),
          PatientId = c("patient", "patid", "nhs"),
          IsolateId = c("isolate", "sampleid", "specimenid"),
          DateOfSpecCollection = c("spec", "collection", "sampledate", "bloodculture"),
          DateOfHospitalAdmission = c("admit", "admission", "entry"),
          DateOfHospitalDischarge = c("discharge", "leave", "exit"),
          MicroorganismCode = c("organism", "pathogen", "microorganism", "species", "bug"),
          MicroorganismCodeLabel = c("organismname", "pathogenname", "speciesname"),
          Sex = c("sex", "gender"),
          Age = c("age")
        )
        for (k in names(pattern_map)) {
          pats <- pattern_map[[k]]
          if (any(startsWith(rn, pats))) return(k)
        }
        # direct name match to candidates
        matches <- candidates[ norm(candidates) == rn ]
        if (length(matches) > 0) return(matches[1])
        NA_character_
      }
      # Build mapping controls
      ctrls <- list(shiny::p(sprintf("Sample has %d columns.", ncol(df))), shiny::tableOutput("sample_head"))
      for (i in seq_along(names(df))) {
        raw_nm <- names(df)[i]
        input_id <- paste0("map_", i)
        default_val <- guess_for(raw_nm)
        ctrls[[length(ctrls) + 1]] <- shiny::div(
          style = "display:flex; gap:10px; align-items:center;",
          shiny::div(style = "width:280px; font-weight:600;", raw_nm),
          shiny::selectizeInput(input_id, NULL, choices = c("(ignore)", candidates), selected = ifelse(is.na(default_val), "(ignore)", default_val))
        )
      }
      ctrls
    })
    output$mapping_controls <- shiny::renderUI({
      shiny::div(
        shiny::actionButton("apply_mapping", "Apply mapping", class = "btn-primary"),
        style = "margin-bottom:8px;"
      )
    })
    shiny::observeEvent(input$apply_mapping, {
      df <- state$sample
      if (is.null(df)) return(NULL)
      res <- data.frame(raw_column_name = character(0), standard_column_name = character(0), stringsAsFactors = FALSE)
      for (i in seq_along(names(df))) {
        sel <- input[[paste0("map_", i)]]
        if (!is.null(sel) && nzchar(sel) && sel != "(ignore)") {
          res <- rbind(res, data.frame(raw_column_name = names(df)[i], standard_column_name = sel, stringsAsFactors = FALSE))
        }
      }
      state$dictionary_map <- res
      output$mapping_summary <- shiny::renderPrint({ res })
      shiny::showNotification(paste0("Mapped ", nrow(res), " columns."), type = "message", duration = 3)
    })
    output$sample_head <- shiny::renderTable({
      if (is.null(state$sample)) return(NULL)
      utils::head(state$sample, 5)
    })
    output$lookups_ui <- shiny::renderUI({
      df <- state$sample
      shiny::tagList(
        shiny::div(style = "background:#fff3cd;border:1px solid #ffeeba;border-left:4px solid #ffc107;border-radius:4px;padding:10px;margin-bottom:8px;",
                   shiny::HTML("This tool seeds only <code>from_value</code> using distinct values from your data and leaves <code>to_value</code> blank.<br/>After export, open Excel and fill <code>to_value</code> with the standardized EHR‑BSI values you want.")),
        shiny::div(style = "display:flex; gap:8px; align-items:center;",
                   shiny::selectInput("lookup_source_col", NULL, choices = if (is.null(df)) character(0) else names(df)),
                   shiny::textInput("lookup_name", NULL, value = paste0(input$country, "_Lookup1"), placeholder = "Lookup name (e.g., EE_Outcome)"),
                   shiny::actionButton("add_lookup", "Add lookup")
        ),
        shiny::hr(),
        shiny::h5("Preview"),
        shiny::tableOutput("lookup_preview")
      )
    })
    shiny::observeEvent(input$add_lookup, {
      df <- state$sample
      col <- input$lookup_source_col
      ln <- input$lookup_name
      if (is.null(df) || !nzchar(col) || !nzchar(ln) || !(col %in% names(df))) return(NULL)
      vals <- unique(df[[col]])
      vals <- vals[!is.na(vals)]
      # leave to_value empty so users must enter a standardized target later
      lk <- data.frame(lookup_name = ln, from_value = as.character(vals), to_value = NA_character_, stringsAsFactors = FALSE)
      state$lookups[[ln]] <- lk
      output$lookups_summary <- shiny::renderPrint({
        sapply(state$lookups, nrow)
      })
      output$lookup_preview <- shiny::renderTable({
        head(lk, 15)
      }, striped = TRUE, bordered = TRUE, spacing = "s")
      shiny::showNotification(paste0("Added lookup '", ln, "' with ", nrow(lk), " unique source values. 'to_value' left blank; fill after export."), type = "message", duration = 5)
    })
    output$ids_dates_ui <- shiny::renderUI({
      kv <- state$excel_kv
      shiny::verbatimTextOutput("ids_dates_text")
    })
    output$ids_dates_text <- shiny::renderText({
      kv <- state$excel_kv
      paste(capture.output(str(kv)), collapse = "\n")
    })
    # Live preview of current key-values
    output$preview <- shiny::renderText({
      kv <- state$excel_kv
      if (length(kv) == 0) return("No suggestions yet. Click 'Auto-detect & suggest'.")
      paste(capture.output(str(kv)), collapse = "\n")
    })
    shiny::observeEvent(input$validate, {
      cc <- input$country
      # Prefer in-package validator; fallback to a quick local check using current kv
      res <- tryCatch({
        fun <- get0("validate_country_config")
        if (is.null(fun)) {
          # quick local validation on kv
          kv <- state$excel_kv
          errs <- c(); warns <- c()
          if (is.null(kv$`antibiotic.profile`)) warns <- c(warns, "antibiotic.profile not set")
          if (is.null(kv$`record_ids.bsi`) || is.null(kv$`record_ids.patient`) || is.null(kv$`record_ids.isolate`))
            errs <- c(errs, "record_ids.* templates missing")
          list(errors = errs, warnings = warns)
        } else {
          fun(cc)
        }
      }, error = function(e) list(errors = e$message, warnings = character(0)))
      output$val_out <- shiny::renderPrint({ res })
    })
    shiny::observeEvent(input$export, {
      cc <- input$country
      # Write a minimal v2 workbook using current kv; if no dictionary exists, create Config-only workbook
      out <- file.path("reference", "dictionaries", paste0(cc, "_v2.xlsx"))
      tryCatch({
        wb <- openxlsx::createWorkbook()
        # Assemble Config
        openxlsx::addWorksheet(wb, "Config")
        kv <- state$excel_kv
        if (length(kv) == 0) kv <- list(schema_version = 2)
        # Add lookups.include if user created any lookups
        if (length(state$lookups) > 0) {
          kv$`lookups.include` <- paste(names(state$lookups), collapse = ",")
        }
        df_cfg <- data.frame(config_key = names(kv), config_value = unname(unlist(kv)), stringsAsFactors = FALSE)
        openxlsx::writeData(wb, "Config", df_cfg)
        # Dictionary sheet (if mapping exists)
        if (!is.null(state$dictionary_map) && nrow(state$dictionary_map) > 0) {
          openxlsx::addWorksheet(wb, "Dictionary")
          openxlsx::writeData(wb, "Dictionary", state$dictionary_map)
        } else {
          # still add empty Dictionary with correct headers for convenience
          openxlsx::addWorksheet(wb, "Dictionary")
          openxlsx::writeData(wb, "Dictionary", data.frame(raw_column_name = character(0), standard_column_name = character(0)))
        }
        # Lookups sheet (compile all)
        openxlsx::addWorksheet(wb, "Lookups")
        if (length(state$lookups) > 0) {
          lk_all <- do.call(rbind, state$lookups)
        } else {
          lk_all <- data.frame(lookup_name = character(0), from_value = character(0), to_value = character(0))
        }
        openxlsx::writeData(wb, "Lookups", lk_all)
        openxlsx::saveWorkbook(wb, out, overwrite = TRUE)
        shiny::showNotification(paste("Exported:", out), type = "message")
      }, error = function(e) shiny::showNotification(e$message, type = "error"))
    })
  }
  shiny::shinyApp(ui, server)
}


