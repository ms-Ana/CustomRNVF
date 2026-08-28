# CreateCohortDefinitionSet.R
#
# ONE-TIME (per study) step. Pulls cohort definitions from an ATLAS WebAPI
# instance into the package's inst/ folder using OHDSI's ROhdsiWebApi. CirceR
# generates the SQL - no SQL is authored by hand.
#
# Prerequisites:
#   1. Fill in inst/settings/CohortsToCreate.csv with the cohorts you want
#      (columns: atlasId, cohortId, cohortName). atlasId = the cohort id in ATLAS.
#   2. Set the ATLAS base URL below and ensure you can reach the WebAPI.
#
# Run this from the package root (the working directory must be the package dir).

library(ROhdsiWebApi)


#base.url <- "https://optima-rc.hzdr.de/WebAPI"
#base.url <- "https://optima-rc.hzdr.de:9443/WebAPI"
my_browser_token <- "Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJhLmRlcnpoYW5za2FpYUBoemRyLmRlIiwiU2Vzc2lvbi1JRCI6bnVsbCwiZXhwIjoxNzg3MjY0Mjk1fQ.A5t9NKXQLjuAUGBW0CGZuksu38IeXufhWVZ8KTq6pc0MgMWOIIIJ9hOkbN7qKsY0t1-NfIHACfRqzf4m-F-CoQ" 
baseUrl <- "https://optima.hzdr.de:9443/WebAPI"

ROhdsiWebApi::setAuthHeader(baseUrl, my_browser_token)

# ---- Pull cohorts into inst/ --------------------------------------------------
# Reads inst/settings/CohortsToCreate.csv, fetches each cohort's expression from
# ATLAS, writes JSON to inst/cohorts and CirceR-generated SQL to
# inst/sql/sql_server, and records the file references.
ROhdsiWebApi::insertCohortDefinitionSetInPackage(
  fileName = "inst/settings/CohortsToCreate.csv",
  baseUrl = base.url,
  jsonFolder = "inst/cohorts",
  sqlFolder = "inst/sql/sql_server",
  insertTableSql = TRUE,
  insertCohortCreationR = FALSE,
  generateStats = TRUE,
  packageName = "CustomRNVF"
)

message("Cohort definitions written to inst/cohorts and inst/sql/sql_server.")
