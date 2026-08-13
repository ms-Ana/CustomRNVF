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
base.url <- "https://pioneer.hzdr.de/WebAPI"
token <- "Bearer eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJhbmRyZWkua2lzZWxldkBiZG1jb25zdWx0aW5nLmZyIiwiU2Vzc2lvbi1JRCI6bnVsbCwiZXhwIjoxNzg0NzQ3MzIyfQ.KYn1m8Aj9YkCtVUiQ6QGWXfAQMe60nrxJfmoELOrpMPrdp_iTMQcveplqof9oQoWqps4RM1jqMpt23L_D5w5nA"

ROhdsiWebApi::setAuthHeader(base.url, token)

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
