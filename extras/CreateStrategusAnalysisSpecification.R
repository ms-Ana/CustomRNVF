# CustomRNVF - generate the Strategus analysis specification
#
# Builds `extras/StrategusAnalysisSpecification.json` from the cohort definitions
# in `inst/` using the Strategus module API. The specification mirrors the study
# implemented in `R/execute.R` (cohort generation, cohort diagnostics,
# characterization with two time-at-risk windows, and cohort incidence) for the
# module package versions pinned in `renv.lock` (HADES 2025Q3, Strategus 1.4.1).
#
# The specification is portable: it contains NO connection details, schemas, or
# databaseId - those belong to the execution settings supplied at run time.
#
# Usage (from the package root):
#   source("extras/CreateStrategusAnalysisSpecification.R")

suppressPackageStartupMessages(library(Strategus))

# ===========================================================================
# EDIT HERE - study scope
# ---------------------------------------------------------------------------
# Set which cohorts (by cohortId, as listed in inst/settings/CohortsToCreate.csv)
# act as targets (exposures) and which act as outcomes. These drive the
# characterization case series and the incidence-rate target x outcome pairings.
# Every id below must exist in CohortsToCreate.csv, and an id cannot be both a
# target and an outcome. The script validates this before building anything.
targetIds  <- c(312, 311)
outcomeIds <- c(253, 257, 260, 261, 258)

outputFile <- "extras/StrategusAnalysisSpecification.json"
# ===========================================================================

# --- Cohorts (read straight from the package's inst/ folder) ---------------
cohortDefinitionSet <- CohortGenerator::getCohortDefinitionSet(
  settingsFileName = "inst/settings/CohortsToCreate.csv",
  jsonFolder       = "inst/cohorts",
  sqlFolder        = "inst/sql/sql_server",
  packageName      = NULL,
  verbose          = FALSE
)

# --- Validate the scope against the loaded cohort set ----------------------
knownIds <- cohortDefinitionSet$cohortId
missingIds <- setdiff(c(targetIds, outcomeIds), knownIds)
if (length(missingIds) > 0) {
  stop("These target/outcome ids are not in CohortsToCreate.csv: ",
       paste(missingIds, collapse = ", "),
       "\nAvailable ids: ", paste(sort(knownIds), collapse = ", "),
       call. = FALSE)
}
overlap <- intersect(targetIds, outcomeIds)
if (length(overlap) > 0) {
  stop("Ids used as both target and outcome: ",
       paste(overlap, collapse = ", "), call. = FALSE)
}

cohortName <- function(id) {
  cohortDefinitionSet$cohortName[cohortDefinitionSet$cohortId == id]
}

# --- Cohort generation -----------------------------------------------------
cohortGeneratorModule <- CohortGeneratorModule$new()
cohortSharedResource  <- cohortGeneratorModule$createCohortSharedResourceSpecifications(
  cohortDefinitionSet
)
cohortGeneratorSpecs <- cohortGeneratorModule$createModuleSpecifications(
  generateStats = TRUE
)

# --- Cohort diagnostics ----------------------------------------------------
cohortDiagnosticsModule <- CohortDiagnosticsModule$new()
cohortDiagnosticsSpecs  <- cohortDiagnosticsModule$createModuleSpecifications()

# --- Characterization ------------------------------------------------------
# Two time-at-risk windows, matching R/execute.R:
#   1. on-treatment      : cohort start -> cohort end
#   2. intent-to-treat   : cohort start -> cohort start + 365 days
# characterizationModule <- CharacterizationModule$new()
# characterizationSpecs  <- characterizationModule$createModuleSpecifications(
#   targetIds        = targetIds,
#   outcomeIds       = outcomeIds,
#   outcomeWashoutDays = rep(365, length(outcomeIds)),  # one per outcome
#   riskWindowStart  = c(0, 0),
#   startAnchor     = c("cohort start", "cohort start"),
#   riskWindowEnd   = c(0, 365),
#   endAnchor       = c("cohort end",   "cohort start"),
#   includeTimeToEvent            = TRUE,
#   includeDechallengeRechallenge = TRUE,
#   includeAggregateCovariate     = TRUE
# )
# 
# # --- Cohort incidence ------------------------------------------------------
# # Same two time-at-risk windows; every target x every outcome, 365-day clean
# # window (as in R/execute.R).
# targetRefs <- lapply(targetIds, function(id) {
#   CohortIncidence::createCohortRef(id = id, name = cohortName(id))
# })
# outcomeRefs <- lapply(seq_along(outcomeIds), function(i) {
#   CohortIncidence::createOutcomeDef(
#     id          = i,
#     name        = cohortName(outcomeIds[i]),
#     cohortId    = outcomeIds[i],
#     cleanWindow = 365
#   )
# })
# tarOnTreatment <- CohortIncidence::createTimeAtRiskDef(
#   id = 1, startWith = "start", endWith = "end"
# )
# tarIntentToTreat <- CohortIncidence::createTimeAtRiskDef(
#   id = 2, startWith = "start", endWith = "start", endOffset = 365
# )
# irAnalysis <- CohortIncidence::createIncidenceAnalysis(
#   targets  = targetIds,
#   outcomes = seq_along(outcomeIds),
#   tars     = c(1, 2)
# )
# irDesign <- CohortIncidence::createIncidenceDesign(
#   targetDefs   = targetRefs,
#   outcomeDefs  = outcomeRefs,
#   tars         = list(tarOnTreatment, tarIntentToTreat),
#   analysisList = list(irAnalysis)
# )
# cohortIncidenceModule <- CohortIncidenceModule$new()
# cohortIncidenceSpecs  <- cohortIncidenceModule$createModuleSpecifications(
#   irDesign = irDesign$toList()
# )

# --- Assemble + serialize --------------------------------------------------
analysisSpecifications <- createEmptyAnalysisSpecifications() |>
  addSharedResources(cohortSharedResource) |>
  addCohortGeneratorModuleSpecifications(cohortGeneratorSpecs) |>
  addCohortDiagnosticsModuleSpecifications(cohortDiagnosticsSpecs) 
  #addCharacterizationModuleSpecifications(characterizationSpecs) |>
  #addCohortIncidenceModuleSpecifications(cohortIncidenceSpecs)

ParallelLogger::saveSettingsToJson(analysisSpecifications, outputFile)
message("Wrote ", outputFile, " (targets: ", paste(targetIds, collapse = ", "),
        " | outcomes: ", paste(outcomeIds, collapse = ", "), ")")
