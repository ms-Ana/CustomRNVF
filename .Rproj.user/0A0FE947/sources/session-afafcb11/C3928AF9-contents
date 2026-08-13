# CustomRNVF - results viewer (Shiny)
#
# A small, self-contained Shiny app for eyeballing the results that
# `CustomRNVF::execute()` writes under `results/<databaseId>/`:
#
#   cohort_counts.csv
#   incidence/incidence_summary.csv
#   characterization/*.csv            (Characterization export)
#   cohortDiagnostics/                (CohortDiagnostics export)
#   log.txt
#
# It reads only the exported CSVs - it never touches the CDM and contains no
# analytic logic. Cohort diagnostics has a purpose-built OHDSI viewer
# (DiagnosticsExplorer), so this app launches that rather than re-implementing
# it.
#
# Usage:
#   source("extras/ResultsViewer.R")
#   launchResultsViewer("results")          # folder that holds the db subfolders
#   # or point straight at one database folder:
#   launchResultsViewer("results/my_cdm")
#
# Dependencies (all pinned in renv.lock): shiny, shinydashboard, DT, ggplot2,
# plotly, dplyr, readr.

# ---------------------------------------------------------------------------
# Helpers (plain functions; no side effects until launchResultsViewer() runs)
# ---------------------------------------------------------------------------

# Read a CSV if it exists, otherwise return NULL. Column names are returned
# unchanged; callers normalise as needed.
.crnvf_read_csv <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    error = function(e) NULL
  )
}

# Lower-case + strip non-alphanumerics, for tolerant column / file matching
# across the camelCase vs snake_case differences between OHDSI packages.
.crnvf_norm <- function(x) gsub("[^a-z0-9]", "", tolower(x))

# Return the first column (by normalised name) present in `df`, else NULL.
.crnvf_col <- function(df, candidates) {
  if (is.null(df)) return(NULL)
  norm <- .crnvf_norm(names(df))
  for (cand in candidates) {
    hit <- which(norm == .crnvf_norm(cand))
    if (length(hit)) return(names(df)[hit[1]])
  }
  NULL
}

# Identify which immediate subfolders of `resultsFolder` look like a per-
# database results folder. If `resultsFolder` itself is one, return it.
.crnvf_database_folders <- function(resultsFolder) {
  isDbFolder <- function(p) {
    file.exists(file.path(p, "cohort_counts.csv")) ||
      dir.exists(file.path(p, "characterization")) ||
      dir.exists(file.path(p, "incidence")) ||
      dir.exists(file.path(p, "cohortDiagnostics"))
  }
  if (isDbFolder(resultsFolder)) {
    return(stats::setNames(resultsFolder, basename(resultsFolder)))
  }
  subdirs <- list.dirs(resultsFolder, recursive = FALSE)
  subdirs <- subdirs[vapply(subdirs, isDbFolder, logical(1))]
  stats::setNames(subdirs, basename(subdirs))
}

# Map cohortId -> cohortName from the package settings file, if reachable.
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
  idCol <- .crnvf_col(df, c("cohortId"))
  nameCol <- .crnvf_col(df, c("cohortName", "atlasName", "name"))
  if (is.null(idCol) || is.null(nameCol)) return(NULL)
  stats::setNames(as.character(df[[nameCol]]), as.character(df[[idCol]]))
}

# Human label for a cohort id, falling back to the id itself.
.crnvf_label <- function(id, lookup) {
  id <- as.character(id)
  out <- ifelse(!is.null(lookup) & id %in% names(lookup),
                paste0(lookup[id], " (", id, ")"),
                id)
  unname(out)
}

# Friendly time-at-risk label for the two windows execute() defines.
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
        shinydashboard::menuItem("Cohort counts", tabName = "counts",
                                 icon = shiny::icon("users")),
        shinydashboard::menuItem("Incidence rates", tabName = "incidence",
                                 icon = shiny::icon("chart-line")),
        shinydashboard::menuItem("Characterization", tabName = "char",
                                 icon = shiny::icon("table")),
        shinydashboard::menuItem("Cohort diagnostics", tabName = "diag",
                                 icon = shiny::icon("stethoscope")),
        shinydashboard::menuItem("Log", tabName = "log",
                                 icon = shiny::icon("file-lines"))
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(

        # ---- Cohort counts --------------------------------------------------
        shinydashboard::tabItem(
          tabName = "counts",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Cohort counts", status = "primary",
              solidHeader = TRUE,
              plotly::plotlyOutput("countsPlot", height = "380px")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Table", status = "primary",
              solidHeader = TRUE,
              DT::DTOutput("countsTable")
            )
          )
        ),

        # ---- Incidence ------------------------------------------------------
        shinydashboard::tabItem(
          tabName = "incidence",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Filters", status = "primary",
              solidHeader = TRUE, collapsible = TRUE,
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
              width = 12, title = "Incidence", status = "primary",
              solidHeader = TRUE,
              plotly::plotlyOutput("irPlot", height = "420px")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Table", status = "primary",
              solidHeader = TRUE,
              DT::DTOutput("irTable")
            )
          )
        ),

        # ---- Characterization ----------------------------------------------
        shinydashboard::tabItem(
          tabName = "char",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "Top covariates by prevalence",
              status = "primary", solidHeader = TRUE, collapsible = TRUE,
              shiny::fluidRow(
                shiny::column(6, shiny::uiOutput("covCohortPicker")),
                shiny::column(6, shiny::sliderInput("covTopN", "Show top N",
                                                    min = 5, max = 50,
                                                    value = 20, step = 5))
              ),
              plotly::plotlyOutput("covPlot", height = "520px")
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              width = 12,
              title = "Raw characterization files", status = "primary",
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
              width = 12, title = "CohortDiagnostics", status = "primary",
              solidHeader = TRUE,
              shiny::htmlOutput("diagStatus"),
              shiny::br(),
              shiny::actionButton("launchDiag", "Launch DiagnosticsExplorer",
                                  icon = shiny::icon("up-right-from-square"),
                                  class = "btn-primary"),
              shiny::helpText(
                "Opens OHDSI's CohortDiagnostics::launchDiagnosticsExplorer ",
                "on this database's exported diagnostics in a separate window."
              )
            )
          )
        ),

        # ---- Log ------------------------------------------------------------
        shinydashboard::tabItem(
          tabName = "log",
          shiny::fluidRow(
            shinydashboard::box(
              width = 12, title = "execution log", status = "primary",
              solidHeader = TRUE,
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

    # ---- Cohort counts ----------------------------------------------------
    counts <- shiny::reactive({
      df <- .crnvf_read_csv(file.path(dbFolder(), "cohort_counts.csv"))
      shiny::validate(shiny::need(!is.null(df), "cohort_counts.csv not found."))
      idCol <- .crnvf_col(df, c("cohortId", "cohortDefinitionId"))
      df$.label <- .crnvf_label(df[[idCol]], cohortLookup)
      df
    })

    output$countsPlot <- plotly::renderPlotly({
      df <- counts()
      subjCol <- .crnvf_col(df, c("cohortSubjects", "subjects"))
      entCol <- .crnvf_col(df, c("cohortEntries", "entries"))
      shiny::validate(shiny::need(!is.null(subjCol), "No subject count column."))
      plotData <- df[order(-df[[subjCol]]), ]
      plotData$.label <- factor(plotData$.label, levels = plotData$.label)
      p <- ggplot2::ggplot(
        plotData,
        ggplot2::aes(x = .data$.label, y = .data[[subjCol]],
                     text = if (!is.null(entCol)) {
                       paste0("Subjects: ", .data[[subjCol]],
                              "<br>Entries: ", .data[[entCol]])
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

    # ---- Incidence --------------------------------------------------------
    incidence <- shiny::reactive({
      df <- .crnvf_read_csv(file.path(dbFolder(), "incidence",
                                      "incidence_summary.csv"))
      shiny::validate(shiny::need(!is.null(df),
                                  "incidence/incidence_summary.csv not found."))
      df
    })

    output$irTargetPicker <- shiny::renderUI({
      df <- incidence()
      tCol <- .crnvf_col(df, c("targetCohortDefinitionId", "targetId",
                               "targetCohortId"))
      shiny::req(tCol)
      ids <- sort(unique(df[[tCol]]))
      shiny::selectInput("irTarget", "Target",
                         choices = stats::setNames(ids,
                                                   .crnvf_label(ids, cohortLookup)),
                         selected = ids[1])
    })

    output$irOutcomePicker <- shiny::renderUI({
      df <- incidence()
      oCol <- .crnvf_col(df, c("outcomeCohortDefinitionId", "outcomeId"))
      shiny::req(oCol)
      ids <- sort(unique(df[[oCol]]))
      shiny::selectInput("irOutcome", "Outcome",
                         choices = stats::setNames(ids,
                                                   .crnvf_label(ids, cohortLookup)),
                         selected = ids,
                         multiple = TRUE)
    })

    output$irTarPicker <- shiny::renderUI({
      df <- incidence()
      tarCol <- .crnvf_col(df, c("tarId"))
      shiny::req(tarCol)
      ids <- sort(unique(df[[tarCol]]))
      shiny::selectInput("irTar", "Time-at-risk",
                         choices = stats::setNames(ids, .crnvf_tar_label(ids)),
                         selected = ids, multiple = TRUE)
    })

    irFiltered <- shiny::reactive({
      df <- incidence()
      tCol <- .crnvf_col(df, c("targetCohortDefinitionId", "targetId",
                               "targetCohortId"))
      oCol <- .crnvf_col(df, c("outcomeCohortDefinitionId", "outcomeId"))
      tarCol <- .crnvf_col(df, c("tarId"))
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
      oCol <- .crnvf_col(df, c("outcomeCohortDefinitionId", "outcomeId"))
      tarCol <- .crnvf_col(df, c("tarId"))
      rateCol <- .crnvf_col(df, c("incidenceRateP100py", "incidenceRate"))
      propCol <- .crnvf_col(df, c("incidenceProportionP100p",
                                  "incidenceProportion"))
      yCol <- if (input$irMetric == "rate") rateCol else propCol
      yLab <- if (input$irMetric == "rate") "Incidence rate (/100 PY)" else
        "Incidence proportion (/100 persons)"
      shiny::validate(shiny::need(!is.null(yCol),
                                  "Selected metric column not found."))
      df$.outcome <- .crnvf_label(df[[oCol]], cohortLookup)
      df$.tar <- .crnvf_tar_label(df[[tarCol]])
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data$.outcome, y = .data[[yCol]], fill = .data$.tar,
                     text = paste0("Outcome: ", .data$.outcome,
                                   "<br>", .data$.tar,
                                   "<br>", yLab, ": ",
                                   round(.data[[yCol]], 3)))
      ) +
        ggplot2::geom_col(position = ggplot2::position_dodge()) +
        ggplot2::labs(x = NULL, y = yLab, fill = "Time-at-risk") +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
      plotly::ggplotly(p, tooltip = "text")
    })

    output$irTable <- DT::renderDT({
      DT::datatable(irFiltered(), rownames = FALSE,
                    options = list(pageLength = 25, scrollX = TRUE))
    })

    # ---- Characterization -------------------------------------------------
    charFolder <- shiny::reactive(file.path(dbFolder(), "characterization"))

    charFiles <- shiny::reactive({
      f <- charFolder()
      if (!dir.exists(f)) return(character(0))
      list.files(f, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
    })

    # Locate covariate value + reference tables (names vary by package version).
    covData <- shiny::reactive({
      files <- charFiles()
      shiny::validate(shiny::need(length(files) > 0,
                                  "No characterization CSVs found."))
      pick <- function(patterns) {
        norm <- .crnvf_norm(basename(files))
        for (pat in patterns) {
          hit <- which(norm == .crnvf_norm(pat))
          if (length(hit)) return(files[hit[1]])
        }
        NULL
      }
      values <- .crnvf_read_csv(pick(c("covariates.csv", "covariate.csv")))
      ref <- .crnvf_read_csv(pick(c("covariateRef.csv", "covariate_ref.csv")))
      shiny::validate(shiny::need(!is.null(values) && !is.null(ref),
                                  "Covariate value/reference files not found."))
      idCol <- .crnvf_col(values, c("covariateId"))
      refIdCol <- .crnvf_col(ref, c("covariateId"))
      nameCol <- .crnvf_col(ref, c("covariateName"))
      valCol <- .crnvf_col(values, c("averageValue", "mean", "proportion"))
      cohortCol <- .crnvf_col(values, c("cohortDefinitionId", "targetCohortId",
                                        "cohortId"))
      shiny::validate(shiny::need(
        !is.null(idCol) && !is.null(valCol) && !is.null(nameCol),
        "Could not locate covariate id / value / name columns."))
      merged <- dplyr::left_join(
        values, ref[, c(refIdCol, nameCol)],
        by = stats::setNames(refIdCol, idCol)
      )
      list(data = merged, valCol = valCol, nameCol = nameCol,
           cohortCol = cohortCol)
    })

    output$covCohortPicker <- shiny::renderUI({
      cd <- covData()
      if (is.null(cd$cohortCol)) return(NULL)
      ids <- sort(unique(cd$data[[cd$cohortCol]]))
      shiny::selectInput("covCohort", "Cohort",
                         choices = stats::setNames(ids,
                                                   .crnvf_label(ids, cohortLookup)),
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
      df[[cd$nameCol]] <- factor(df[[cd$nameCol]], levels = rev(df[[cd$nameCol]]))
      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data[[cd$valCol]], y = .data[[cd$nameCol]],
                     text = paste0(.data[[cd$nameCol]], "<br>Value: ",
                                   round(.data[[cd$valCol]], 4)))
      ) +
        ggplot2::geom_col(fill = "#00a65a") +
        ggplot2::labs(x = "Mean / proportion", y = NULL) +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = "text")
    })

    output$charFilePicker <- shiny::renderUI({
      files <- charFiles()
      shiny::validate(shiny::need(length(files) > 0,
                                  "No characterization CSVs found."))
      shiny::selectInput("charFile", "File",
                         choices = stats::setNames(files, basename(files)))
    })

    output$charFileTable <- DT::renderDT({
      shiny::req(input$charFile)
      df <- .crnvf_read_csv(input$charFile)
      shiny::validate(shiny::need(!is.null(df), "Could not read file."))
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 15, scrollX = TRUE))
    })

    # ---- Cohort diagnostics ----------------------------------------------
    diagFolder <- shiny::reactive(file.path(dbFolder(), "cohortDiagnostics"))

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
        shiny::showNotification("No cohortDiagnostics export to launch.",
                                type = "error")
        return()
      }
      if (!requireNamespace("CohortDiagnostics", quietly = TRUE)) {
        shiny::showNotification("CohortDiagnostics is not installed.",
                                type = "error")
        return()
      }
      # CohortDiagnostics needs a merged sqlite results file; build it on first
      # launch if it is not already there, then open the explorer.
      tryCatch({
        sqlitePath <- file.path(f, "MergedCohortDiagnosticsData.sqlite")
        if (!file.exists(sqlitePath)) {
          shiny::showNotification("Merging diagnostics results...",
                                  type = "message")
          CohortDiagnostics::createMergedResultsFile(
            dataFolder = f, sqliteDbPath = sqlitePath, overwrite = FALSE
          )
        }
        shiny::showNotification("Launching DiagnosticsExplorer...",
                                type = "message")
        CohortDiagnostics::launchDiagnosticsExplorer(sqliteDbPath = sqlitePath)
      }, error = function(e) {
        shiny::showNotification(paste("DiagnosticsExplorer error:",
                                      conditionMessage(e)),
                                type = "error", duration = NULL)
      })
    })

    # ---- Log --------------------------------------------------------------
    output$logText <- shiny::renderText({
      logPath <- file.path(dbFolder(), "log.txt")
      if (!file.exists(logPath)) return("log.txt not found.")
      paste(readLines(logPath, warn = FALSE), collapse = "\n")
    })
  }
}

# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

#' Launch the CustomRNVF results viewer.
#'
#' @param resultsFolder Path to the study output folder. This may be the top
#'   `results/` folder that holds one subfolder per `databaseId`, or a single
#'   database folder directly. Defaults to the `CUSTOM_RNVF_RESULTS` environment
#'   variable, or `results/` under the working directory.
#' @param launch.browser Whether to open a browser window. Passed to
#'   `shiny::runApp()`.
#' @param ... Further arguments passed to `shiny::runApp()`.
launchResultsViewer <- function(resultsFolder = Sys.getenv("CUSTOM_RNVF_RESULTS",
                                                           "results"),
                                launch.browser = TRUE,
                                ...) {
  for (pkg in c("shiny", "shinydashboard", "DT", "ggplot2", "plotly",
                "dplyr", "readr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required. Run renv::restore().", call. = FALSE)
    }
  }
  if (!dir.exists(resultsFolder)) {
    stop("Results folder does not exist: ", resultsFolder, call. = FALSE)
  }
  app <- shiny::shinyApp(ui = .crnvf_ui(),
                         server = .crnvf_server(resultsFolder))
  shiny::runApp(app, launch.browser = launch.browser, ...)
}

# When sourced interactively, hint at how to start the app rather than auto-
# launching (so `source()` stays side-effect free).
if (interactive() && identical(environment(), globalenv())) {
  message("Loaded CustomRNVF results viewer. Start it with:\n",
          "  launchResultsViewer(\"results\")")
}
