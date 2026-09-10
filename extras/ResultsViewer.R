# CustomRNVF - results viewer (Shiny)
#
# Usage:
#   source("extras/ResultsViewer.R")
#   launchResultsViewer("results")

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(dplyr)
  library(readr)
})

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.crnvf_read_csv <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
}

.crnvf_norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

.crnvf_col <- function(df, candidates) {
  if (is.null(df)) return(NULL)
  norm <- .crnvf_norm(names(df))
  for (cand in candidates) {
    hit <- which(norm == .crnvf_norm(cand))
    if (length(hit)) return(names(df)[hit[1]])
  }
  NULL
}

.crnvf_find_file <- function(baseFolder, pattern) {
  if (is.null(baseFolder) || !dir.exists(baseFolder)) return(NULL)
  files <- list.files(baseFolder, pattern = pattern, full.names = TRUE, recursive = TRUE)
  if (length(files) > 0) files[1] else NULL
}

.crnvf_database_folders <- function(resultsFolder) {
  isDbFolder <- function(p) {
    dir.exists(file.path(p, "CohortGeneratorModule")) ||
      dir.exists(file.path(p, "CohortDiagnosticsModule")) ||
      dir.exists(file.path(p, "CharacterizationModule")) ||
      dir.exists(file.path(p, "CohortIncidenceModule")) ||
      dir.exists(file.path(p, "CohortMethodModule")) ||
      file.exists(file.path(p, "cohort_counts.csv")) ||
      file.exists(file.path(p, "cohort_count.csv"))
  }
  if (isDbFolder(resultsFolder)) {
    return(stats::setNames(resultsFolder, basename(resultsFolder)))
  }
  subdirs <- list.dirs(resultsFolder, recursive = FALSE)
  subdirs <- subdirs[vapply(subdirs, isDbFolder, logical(1))]
  stats::setNames(subdirs, basename(subdirs))
}

.crnvf_cohort_names <- function() {
  candidates <- c(
    tryCatch(
      system.file("settings", "CohortsToCreate.csv", package = "CustomRNVF"),
      error = function(e) ""
    ),
    file.path("inst", "settings", "CohortsToCreate.csv"),
    file.path("..", "inst", "settings", "CohortsToCreate.csv")
  )
  path <- candidates[nzchar(candidates) & file.exists(candidates)][1]
  if (is.na(path)) return(NULL)
  df <- .crnvf_read_csv(path)
  idCol <- .crnvf_col(df, c("cohortId", "cohort_id", "cohortDefinitionId"))
  nameCol <- .crnvf_col(df, c("cohortName", "cohort_name", "atlasName", "name"))
  if (is.null(idCol) || is.null(nameCol)) return(NULL)
  stats::setNames(as.character(df[[nameCol]]), as.character(df[[idCol]]))
}

.crnvf_label <- function(id, lookup) {
  id <- as.character(id)
  out <- ifelse(!is.null(lookup) & id %in% names(lookup),
                paste0(lookup[id], " (", id, ")"),
                id)
  unname(out)
}

.crnvf_tar_label <- function(tarId) {
  labs <- c("1" = "On-treatment", "2" = "Intent-to-treat (+365d)")
  out <- labs[as.character(tarId)]
  out[is.na(out)] <- paste0("TAR ", tarId[is.na(out)])
  unname(out)
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

.crnvf_ui <- function() {
  shinydashboard::dashboardPage(
    skin = "blue",
    shinydashboard::dashboardHeader(title = "CustomRNVF results"),
    shinydashboard::dashboardSidebar(
      shiny::uiOutput("databasePicker"),
      shinydashboard::sidebarMenu(
        id = "tabs",
        shinydashboard::menuItem("Cohort counts", tabName = "counts", icon = shiny::icon("users")),
        shinydashboard::menuItem("Incidence rates", tabName = "incidence", icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("Characterization", tabName = "char", icon = shiny::icon("table")),
        shinydashboard::menuItem("Cohort diagnostics", tabName = "diag", icon = shiny::icon("stethoscope")),
        shinydashboard::menuItem("CohortMethod", tabName = "cm", icon = shiny::icon("scale-balanced")),
        shinydashboard::menuItem("Log", tabName = "log", icon = shiny::icon("file-lines"))
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        
        # ---- Cohort counts -------------------------------------------------
        shinydashboard::tabItem(
          tabName = "counts",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Cohort counts", status = "primary", solidHeader = TRUE,
              plotly::plotlyOutput("countsPlot", height = "380px")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Table", status = "primary", solidHeader = TRUE,
              DT::DTOutput("countsTable")
            )
          )
        ),
        
        # ---- Incidence -----------------------------------------------------
        shinydashboard::tabItem(
          tabName = "incidence",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Filters", status = "primary", solidHeader = TRUE, collapsible = TRUE,
              shiny::fluidRow(
                shiny::column(4, shiny::uiOutput("irTargetPicker")),
                shiny::column(4, shiny::uiOutput("irOutcomePicker")),
                shiny::column(4, shiny::uiOutput("irTarPicker")),
                shiny::column(
                  4,
                  shiny::selectInput(
                    "irMetric", "Metric",
                    c("Incidence rate (/100 PY)" = "rate",
                      "Incidence proportion (/100 persons)" = "proportion"),
                    selected = "rate"
                  )
                )
              )
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Incidence", status = "primary", solidHeader = TRUE,
              plotly::plotlyOutput("irPlot", height = "420px")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Table", status = "primary", solidHeader = TRUE,
              DT::DTOutput("irTable")
            )
          )
        ),
        
        # ---- Characterization ----------------------------------------------
        shinydashboard::tabItem(
          tabName = "char",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Top covariates by prevalence", status = "primary",
              solidHeader = TRUE, collapsible = TRUE,
              shiny::fluidRow(
                shiny::column(6, shiny::uiOutput("covCohortPicker")),
                shiny::column(6, shiny::sliderInput("covTopN", "Show top N", min = 5, max = 50, value = 20, step = 5))
              ),
              plotly::plotlyOutput("covPlot", height = "520px")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Raw characterization files", status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
              shiny::uiOutput("charFilePicker"),
              DT::DTOutput("charFileTable")
            )
          )
        ),
        
        # ---- Cohort diagnostics --------------------------------------------
        shinydashboard::tabItem(
          tabName = "diag",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "CohortDiagnostics", status = "primary", solidHeader = TRUE,
              shiny::htmlOutput("diagStatus"),
              shiny::br(),
              shiny::actionButton("launchDiag", "Launch DiagnosticsExplorer",
                                  icon = shiny::icon("arrow-up-right-from-square"),
                                  class = "btn-primary"),
              shiny::helpText(
                "Opens OHDSI's CohortDiagnostics::launchDiagnosticsExplorer in a separate window."
              )
            )
          )
        ),
        
        # ---- CohortMethod --------------------------------------------------
        shinydashboard::tabItem(
          tabName = "cm",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Kaplan-Meier Curve", status = "primary", solidHeader = TRUE,
              shiny::fluidRow(
                shiny::column(6, shiny::uiOutput("kmOutcomePicker")),
                shiny::column(6, shiny::uiOutput("kmAnalysisPicker"))
              ),
              plotly::plotlyOutput("kmPlot", height = "450px")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Cox Model Hazard Ratios (CKD vs Non-CKD)", status = "primary", solidHeader = TRUE,
              shiny::helpText("Hazard Ratios (HR) < 1 indicate lower risk with CKD, HR > 1 indicates higher risk."),
              DT::DTOutput("cmResultsTable")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Attrition / Retention Post-Matching", status = "info", solidHeader = TRUE,
              collapsible = TRUE,
              DT::DTOutput("cmAttritionTable")
            )
          )
        ),
        
        # ---- Log -----------------------------------------------------------
        shinydashboard::tabItem(
          tabName = "log",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Execution Log", status = "primary", solidHeader = TRUE,
              shiny::verbatimTextOutput("logText")
            )
          )
        )
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

.crnvf_server <- function(resultsFolder) {
  databaseFolders <- .crnvf_database_folders(resultsFolder)
  cohortLookup <- .crnvf_cohort_names()
  
  function(input, output, session) {
    if (length(databaseFolders) == 0) {
      shiny::showNotification(
        sprintf("No result folders found under '%s'.", resultsFolder),
        type = "error", duration = NULL
      )
    }
    
    output$databasePicker <- shiny::renderUI({
      shiny::selectInput("database", "Database",
                         choices = names(databaseFolders),
                         selected = names(databaseFolders)[1])
    })
    
    dbFolder <- shiny::reactive({
      shiny::req(input$database)
      unname(databaseFolders[input$database])
    })
    
    # ---- Cohort counts -----------------------------------------------------
    counts <- shiny::reactive({
      path <- .crnvf_find_file(dbFolder(), "(cg_)?cohort_count(s)?\\.csv$")
      shiny::validate(shiny::need(!is.null(path), "Cohort counts CSV not found."))
      df <- .crnvf_read_csv(path)
      shiny::validate(shiny::need(!is.null(df), "Could not read cohort count CSV."))
      
      idCol <- .crnvf_col(df, c("cohortId", "cohortDefinitionId", "cohort_id", "cohort_definition_id"))
      shiny::validate(shiny::need(!is.null(idCol), "No cohort ID column identified."))
      df$.label <- .crnvf_label(df[[idCol]], cohortLookup)
      df
    })
    
    output$countsPlot <- plotly::renderPlotly({
      df <- counts()
      subjCol <- .crnvf_col(df, c("cohortSubjects", "cohort_subjects", "subjects", "subjectCount"))
      entCol <- .crnvf_col(df, c("cohortEntries", "cohort_entries", "entries", "entryCount"))
      shiny::validate(shiny::need(!is.null(subjCol), "No subject count column found."))
      
      plotData <- df[order(-df[[subjCol]]), ]
      plotData$.label <- factor(plotData$.label, levels = unique(plotData$.label))
      p <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(x = .data$.label, y = .data[[subjCol]],
                     text = if (!is.null(entCol)) {
                       paste0("Subjects: ", .data[[subjCol]], "<br>Entries: ", .data[[entCol]])
                     } else {
                       paste0("Subjects: ", .data[[subjCol]])
                     })
      ) +
        ggplot2::geom_col(fill = "#3c8dbc") +
        ggplot2::labs(x = NULL, y = "Subjects") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
      plotly::ggplotly(p, tooltip = "text")
    })
    
    output$countsTable <- DT::renderDT({
      DT::datatable(counts()[, setdiff(names(counts()), ".label")],
                    rownames = FALSE, options = list(pageLength = 25))
    })
    
    # ---- Incidence ---------------------------------------------------------
    incidence <- shiny::reactive({
      path <- .crnvf_find_file(dbFolder(), "(ci_incidence_summary|incidence_summary|incidence_rate)\\.csv$")
      shiny::validate(shiny::need(!is.null(path), "Incidence summary CSV not found."))
      df <- .crnvf_read_csv(path)
      shiny::validate(shiny::need(!is.null(df), "Could not read incidence summary CSV."))
      df
    })
    
    output$irTargetPicker <- shiny::renderUI({
      df <- incidence()
      tCol <- .crnvf_col(df, c("targetCohortDefinitionId", "targetId", "targetCohortId", "target_id", "target_cohort_definition_id"))
      shiny::req(tCol)
      ids <- sort(unique(df[[tCol]]))
      shiny::selectInput("irTarget", "Target",
                         choices = stats::setNames(ids, .crnvf_label(ids, cohortLookup)),
                         selected = ids[1])
    })
    
    output$irOutcomePicker <- shiny::renderUI({
      df <- incidence()
      oCol <- .crnvf_col(df, c("outcomeCohortDefinitionId", "outcomeId", "outcomeCohortId", "outcome_id", "outcome_cohort_definition_id"))
      shiny::req(oCol)
      ids <- sort(unique(df[[oCol]]))
      shiny::selectInput("irOutcome", "Outcome",
                         choices = stats::setNames(ids, .crnvf_label(ids, cohortLookup)),
                         selected = ids,
                         multiple = TRUE)
    })
    
    output$irTarPicker <- shiny::renderUI({
      df <- incidence()
      tarCol <- .crnvf_col(df, c("tarId", "timeAtRiskId", "tar_id", "cleanWindow"))
      shiny::req(tarCol)
      ids <- sort(unique(df[[tarCol]]))
      shiny::selectInput("irTar", "Time-at-risk",
                         choices = stats::setNames(ids, .crnvf_tar_label(ids)),
                         selected = ids, multiple = TRUE)
    })
    
    irFiltered <- shiny::reactive({
      df <- incidence()
      tCol <- .crnvf_col(df, c("targetCohortDefinitionId", "targetId", "targetCohortId", "target_id", "target_cohort_definition_id"))
      oCol <- .crnvf_col(df, c("outcomeCohortDefinitionId", "outcomeId", "outcomeCohortId", "outcome_id", "outcome_cohort_definition_id"))
      tarCol <- .crnvf_col(df, c("tarId", "timeAtRiskId", "tar_id", "cleanWindow"))
      
      if (!is.null(tCol) && !is.null(input$irTarget)) {
        df <- df[df[[tCol]] == input$irTarget, , drop = FALSE]
      }
      if (!is.null(oCol) && !is.null(input$irOutcome)) {
        df <- df[df[[oCol]] %in% input$irOutcome, , drop = FALSE]
      }
      if (!is.null(tarCol) && !is.null(input$irTar)) {
        df <- df[df[[tarCol]] %in% input$irTar, , drop = FALSE]
      }
      df
    })
    
    output$irPlot <- plotly::renderPlotly({
      df <- irFiltered()
      shiny::validate(shiny::need(nrow(df) > 0, "No rows match the filters."))
      oCol <- .crnvf_col(df, c("outcomeCohortDefinitionId", "outcomeId", "outcomeCohortId", "outcome_id", "outcome_cohort_definition_id"))
      tarCol <- .crnvf_col(df, c("tarId", "timeAtRiskId", "tar_id", "cleanWindow"))
      rateCol <- .crnvf_col(df, c("incidenceRateP100py", "incidenceRate", "irP100py", "incidence_rate_p100py", "incidence_rate"))
      propCol <- .crnvf_col(df, c("incidenceProportionP100p", "incidenceProportion", "ipP100p", "incidence_proportion_p100p", "incidence_proportion"))
      
      yCol <- if (input$irMetric == "rate") rateCol else propCol
      yLab <- if (input$irMetric == "rate") "Incidence rate (/100 PY)" else "Incidence proportion (/100 persons)"
      
      shiny::validate(shiny::need(!is.null(yCol), "Selected metric column not found."))
      df$.outcome <- .crnvf_label(df[[oCol]], cohortLookup)
      df$.tar <- if (!is.null(tarCol)) .crnvf_tar_label(df[[tarCol]]) else "Default"
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data$.outcome, y = .data[[yCol]], fill = .data$.tar,
                     text = paste0("Outcome: ", .data$.outcome,
                                   "<br>", .data$.tar,
                                   "<br>", yLab, ": ", round(.data[[yCol]], 3)))
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge()) +
        ggplot2::labs(x = NULL, y = yLab, fill = "Time-at-risk") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
      plotly::ggplotly(p, tooltip = "text")
    })
    
    output$irTable <- DT::renderDT({
      DT::datatable(irFiltered(), rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
    })
    
    # ---- Characterization --------------------------------------------------
    charFiles <- shiny::reactive({
      base <- dbFolder()
      subDir <- file.path(base, "CharacterizationModule")
      targetFolder <- if (dir.exists(subDir)) subDir else base
      list.files(targetFolder, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
    })
    
    covData <- shiny::reactive({
      files <- charFiles()
      shiny::validate(shiny::need(length(files) > 0, "No characterization CSVs found."))
      
      pick <- function(patterns) {
        norm <- .crnvf_norm(basename(files))
        for (pat in patterns) {
          hit <- which(norm == .crnvf_norm(pat))
          if (length(hit)) return(files[hit[1]])
        }
        NULL
      }
      
      values <- .crnvf_read_csv(pick(c("covariates.csv", "covariate.csv", "c_covariates.csv")))
      ref <- .crnvf_read_csv(pick(c("covariateRef.csv", "covariate_ref.csv", "c_covariate_ref.csv")))
      
      shiny::validate(shiny::need(!is.null(values) && !is.null(ref), "Covariate value/reference files not found."))
      
      idCol <- .crnvf_col(values, c("covariateId", "covariate_id"))
      refIdCol <- .crnvf_col(ref, c("covariateId", "covariate_id"))
      nameCol <- .crnvf_col(ref, c("covariateName", "covariate_name"))
      valCol <- .crnvf_col(values, c("averageValue", "mean", "proportion", "average_value"))
      cohortCol <- .crnvf_col(values, c("cohortDefinitionId", "targetCohortId", "cohortId", "cohort_definition_id", "cohort_id"))
      
      shiny::validate(shiny::need(!is.null(idCol) && !is.null(valCol) && !is.null(nameCol),
                                  "Could not locate covariate id / value / name columns."))
      
      merged <- dplyr::left_join(
        values, ref[, c(refIdCol, nameCol)],
        by = stats::setNames(refIdCol, idCol)
      )
      list(data = merged, valCol = valCol, nameCol = nameCol, cohortCol = cohortCol)
    })
    
    output$covCohortPicker <- shiny::renderUI({
      cd <- covData()
      if (is.null(cd$cohortCol)) return(NULL)
      ids <- sort(unique(cd$data[[cd$cohortCol]]))
      shiny::selectInput("covCohort", "Cohort",
                         choices = stats::setNames(ids, .crnvf_label(ids, cohortLookup)),
                         selected = ids[1])
    })
    
    output$covPlot <- plotly::renderPlotly({
      cd <- covData()
      df <- cd$data
      if (!is.null(cd$cohortCol) && !is.null(input$covCohort)) {
        df <- df[df[[cd$cohortCol]] == input$covCohort, , drop = FALSE]
      }
      shiny::validate(shiny::need(nrow(df) > 0, "No covariates for this cohort."))
      df <- df[order(-df[[cd$valCol]]), ]
      df <- utils::head(df, input$covTopN)
      
      # Deduplicate covariate names to resolve 'factor level is duplicated'
      displayName <- make.unique(as.character(df[[cd$nameCol]]))
      df$.displayName <- factor(displayName, levels = rev(displayName))
      
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data[[cd$valCol]], y = .data$.displayName,
                     text = paste0(.data[[cd$nameCol]], "<br>Value: ", round(.data[[cd$valCol]], 4)))
      ) +
        ggplot2::geom_col(fill = "#00a65a") +
        ggplot2::labs(x = "Mean / proportion", y = NULL) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })
    
    output$charFilePicker <- shiny::renderUI({
      files <- charFiles()
      shiny::validate(shiny::need(length(files) > 0, "No characterization CSVs found."))
      shiny::selectInput("charFile", "File", choices = stats::setNames(files, basename(files)))
    })
    
    output$charFileTable <- DT::renderDT({
      shiny::req(input$charFile)
      df <- .crnvf_read_csv(input$charFile)
      shiny::validate(shiny::need(!is.null(df), "Could not read file."))
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
    })
    
    # ---- Cohort diagnostics ------------------------------------------------
    diagFolder <- shiny::reactive({
      p1 <- file.path(dbFolder(), "CohortDiagnosticsModule")
      p2 <- file.path(dbFolder(), "cohortDiagnostics")
      if (dir.exists(p1)) return(p1)
      p2
    })
    
    output$diagStatus <- shiny::renderUI({
      f <- diagFolder()
      if (!dir.exists(f)) {
        return(shiny::HTML("<span style='color:#dd4b39'>No cohortDiagnostics export found for this database.</span>"))
      }
      shiny::HTML(sprintf("<b>Export folder:</b> <code>%s</code>", f))
    })
    
    shiny::observeEvent(input$launchDiag, {
      f <- diagFolder()
      if (!dir.exists(f)) {
        shiny::showNotification("No cohortDiagnostics export to launch.", type = "error")
        return()
      }
      if (!requireNamespace("CohortDiagnostics", quietly = TRUE)) {
        shiny::showNotification("CohortDiagnostics is not installed.", type = "error")
        return()
      }
      tryCatch({
        sqlitePath <- file.path(f, "MergedCohortDiagnosticsData.sqlite")
        if (!file.exists(sqlitePath)) {
          shiny::showNotification("Merging diagnostics results...", type = "message")
          CohortDiagnostics::createMergedResultsFile(
            dataFolder = f, sqliteDbPath = sqlitePath, overwrite = FALSE
          )
        }
        shiny::showNotification("Launching DiagnosticsExplorer...", type = "message")
        CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = sqlitePath)
      }, error = function(e) {
        shiny::showNotification(paste("DiagnosticsExplorer error:", conditionMessage(e)),
                                type = "error", duration = NULL)
      })
    })
    
    # ---- CohortMethod ------------------------------------------------------
    kmData <- shiny::reactive({
      kmPath <- .crnvf_find_file(dbFolder(), "cm_kaplan_meier_dist\\.csv$")
      shiny::validate(shiny::need(!is.null(kmPath), "No cm_kaplan_meier_dist.csv found."))
      .crnvf_read_csv(kmPath)
    })
    
    output$kmOutcomePicker <- shiny::renderUI({
      df <- kmData()
      oCol <- .crnvf_col(df, c("outcomeId", "outcome_id"))
      shiny::req(oCol)
      ids <- sort(unique(df[[oCol]]))
      shiny::selectInput("kmOutcome", "Outcome",
                         choices = stats::setNames(ids, .crnvf_label(ids, cohortLookup)),
                         selected = ids[1])
    })
    
    output$kmAnalysisPicker <- shiny::renderUI({
      df <- kmData()
      aCol <- .crnvf_col(df, c("analysisId", "analysis_id"))
      if (is.null(aCol)) return(NULL)
      ids <- sort(unique(df[[aCol]]))
      shiny::selectInput("kmAnalysis", "Analysis ID", choices = ids, selected = ids[1])
    })
    
    output$kmPlot <- plotly::renderPlotly({
      df <- kmData()
      shiny::validate(shiny::need(!is.null(df), "Kaplan-Meier data not found."))
      
      oCol <- .crnvf_col(df, c("outcomeId", "outcome_id"))
      aCol <- .crnvf_col(df, c("analysisId", "analysis_id"))
      tCol <- .crnvf_col(df, c("targetId", "target_id"))
      cCol <- .crnvf_col(df, c("comparatorId", "comparator_id"))
      
      # Maps `time_day`, `time`, or `timeDays`
      timeCol    <- .crnvf_col(df, c("time_day", "timeDays", "time", "day", "days"))
      sTargetCol <- .crnvf_col(df, c("target_survival", "targetSurvival"))
      sCompCol   <- .crnvf_col(df, c("comparator_survival", "comparatorSurvival"))
      
      tLbCol <- .crnvf_col(df, c("target_survival_lb", "targetSurvivalLb"))
      tUbCol <- .crnvf_col(df, c("target_survival_ub", "targetSurvivalUb"))
      cLbCol <- .crnvf_col(df, c("comparator_survival_lb", "comparatorSurvivalLb"))
      cUbCol <- .crnvf_col(df, c("comparator_survival_ub", "comparatorSurvivalUb"))
      
      shiny::validate(
        shiny::need(!is.null(timeCol) && !is.null(sTargetCol) && !is.null(sCompCol),
                    "Kaplan-Meier survival columns missing.")
      )
      
      if (!is.null(oCol) && !is.null(input$kmOutcome)) {
        df <- df[as.character(df[[oCol]]) == as.character(input$kmOutcome), , drop = FALSE]
      }
      if (!is.null(aCol) && !is.null(input$kmAnalysis)) {
        df <- df[as.character(df[[aCol]]) == as.character(input$kmAnalysis), , drop = FALSE]
      }
      
      shiny::validate(shiny::need(nrow(df) > 0, "No data available for the selected filters."))
      
      df <- df[order(df[[timeCol]]), ]
      
      targetName <- if (!is.null(tCol)) .crnvf_label(df[[tCol]][1], cohortLookup) else "Target"
      comparatorName <- if (!is.null(cCol)) .crnvf_label(df[[cCol]][1], cohortLookup) else "Comparator"
      
      targetDf <- data.frame(
        Time = df[[timeCol]],
        Survival = df[[sTargetCol]],
        Lb = if (!is.null(tLbCol)) df[[tLbCol]] else NA_real_,
        Ub = if (!is.null(tUbCol)) df[[tUbCol]] else NA_real_,
        Cohort = targetName
      )
      
      compDf <- data.frame(
        Time = df[[timeCol]],
        Survival = df[[sCompCol]],
        Lb = if (!is.null(cLbCol)) df[[cLbCol]] else NA_real_,
        Ub = if (!is.null(cUbCol)) df[[cUbCol]] else NA_real_,
        Cohort = comparatorName
      )
      
      plotData <- rbind(targetDf, compDf)
      
      p <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(
          x = .data$Time,
          y = .data$Survival,
          color = .data$Cohort,
          fill = .data$Cohort,
          group = .data$Cohort,
          text = paste0(
            "<b>Cohort:</b> ", .data$Cohort,
            "<br><b>Day:</b> ", .data$Time,
            "<br><b>Survival:</b> ", round(.data$Survival * 100, 2), "%",
            ifelse(!is.na(.data$Lb), paste0("<br><b>95% CI:</b> [", round(.data$Lb * 100, 2), "%, ", round(.data$Ub * 100, 2), "%]"), "")
          )
        )
      ) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$Lb, ymax = .data$Ub), alpha = 0.15, linetype = 0) +
        ggplot2::geom_step(linewidth = 0.9) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
        ggplot2::scale_color_manual(values = c("#3c8dbc", "#dd4b39")) +
        ggplot2::scale_fill_manual(values = c("#3c8dbc", "#dd4b39")) +
        ggplot2::labs(x = "Days since index", y = "Survival probability", color = "Arm", fill = "Arm") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
      
      plotly::ggplotly(p, tooltip = "text")
    })
    
    output$cmResultsTable <- DT::renderDT({
      resPath <- .crnvf_find_file(dbFolder(), "cm_result\\.csv$")
      shiny::validate(shiny::need(!is.null(resPath), "No cm_result.csv found."))
      df <- .crnvf_read_csv(resPath)
      
      tCol <- .crnvf_col(df, c("targetId", "target_id"))
      cCol <- .crnvf_col(df, c("comparatorId", "comparator_id"))
      oCol <- .crnvf_col(df, c("outcomeId", "outcome_id"))
      
      if (!is.null(tCol)) df$Target <- .crnvf_label(df[[tCol]], cohortLookup)
      if (!is.null(cCol)) df$Comparator <- .crnvf_label(df[[cCol]], cohortLookup)
      if (!is.null(oCol)) df$Outcome <- .crnvf_label(df[[oCol]], cohortLookup)
      
      colsToShow <- intersect(
        c("Target", "Comparator", "Outcome", "rr", "ci95Lb", "ci95Ub", "p", "targetSubjects", "comparatorSubjects",
          "ci_95_lb", "ci_95_ub", "target_subjects", "comparator_subjects"),
        names(df)
      )
      displayDf <- df[, colsToShow, drop = FALSE]
      
      numCols <- intersect(c("rr", "ci95Lb", "ci95Ub", "p", "ci_95_lb", "ci_95_ub"), names(displayDf))
      DT::datatable(displayDf, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE)) |>
        DT::formatRound(columns = numCols, digits = 3)
    })
    
    output$cmAttritionTable <- DT::renderDT({
      attPath <- .crnvf_find_file(dbFolder(), "cm_attrition\\.csv$")
      shiny::validate(shiny::need(!is.null(attPath), "No cm_attrition.csv found."))
      df <- .crnvf_read_csv(attPath)
      DT::datatable(df, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # ---- Log ---------------------------------------------------------------
    output$logText <- shiny::renderText({
      logPath <- .crnvf_find_file(dbFolder(), "(strategus-log|log)\\.txt$")
      if (is.null(logPath) || !file.exists(logPath)) return("Log file not found.")
      paste(readLines(logPath, warn = FALSE), collapse = "\n")
    })
  }
}

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

launchResultsViewer <- function(resultsFolder = Sys.getenv("CUSTOM_RNVF_RESULTS", "results"),
                                launch.browser = TRUE,
                                ...) {
  for (pkg in c("shiny", "shinydashboard", "DT", "ggplot2", "plotly", "dplyr", "readr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Run renv::restore().", call. = FALSE)
    }
  }
  
  if (!dir.exists(resultsFolder)) {
    stop("Directory '", resultsFolder, "' does not exist.", call. = FALSE)
  }
  
  app <- shiny::shinyApp(
    ui = .crnvf_ui(),
    server = .crnvf_server(resultsFolder)
  )
  shiny::runApp(app, launch.browser = launch.browser, ...)
}

if (interactive() && identical(environment(), globalenv())) {
  message("Loaded CustomRNVF results viewer. Start it with:\n",
          "  launchResultsViewer(\"results\")")
}