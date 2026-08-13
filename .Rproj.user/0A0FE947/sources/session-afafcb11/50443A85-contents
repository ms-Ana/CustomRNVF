# CodeToRun.R
#
# Run the CustomRNVF study against your OMOP CDM. Fill in the connection details
# and schema names below, then source this file.
#
# Before running this for the first time, install the pinned dependencies:
#   renv::restore()
# and (one-time) fetch the cohorts:
#   source("extras/CreateCohortDefinitionSet.R")

library(CustomRNVF)

# Maximum number of cores to be used:
maxCores <- parallel::detectCores()

# The folder where the study intermediate and result files will be written:
homedir = "/Users/andreikiselev/Documents/Rdevelopment"
outputFolder <- file.path(homedir, "StudyResults/OptimaDemoLC")

# Optional: specify where the temporary files (used by the Andromeda package) will be created:
options(andromedaTempFolder = file.path(outputFolder, "andromedaTemp"))

# Details for connecting to the server:
# Database management system name; has to be one of the following:
#'sql server','oracle','postgresql','pdw','impala','netezza','bigquery','spark','sqlite','redshift','hive','sqlite extended','duckdb','snowflake','synapse'
DBMS = "postgresql"
#user name
source(file.path(homedir, "credentials.R"))

USER = db_user_BDM
#password
PASSWORD= db_pass_BDM
#server
SERVER = "51.15.207.59/rdb"
#port
DB_PORT = 3522
# path to local JDBC driver
pathToDriver=Sys.getenv("DATABASECONNECTOR_JAR_FOLDER")

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = DBMS,
                                                                user = USER,
                                                                password = PASSWORD,
                                                                server = SERVER,
                                                                port = DB_PORT,
                                                                pathToDriver = pathToDriver)
# The name of the database schema where the CDM data can be found:

cdmDatabaseSchema <- "lungcancer_synth10k"

# ---- Schemas and study parameters ---------------------------------------------
cdmDatabaseSchema <- "lungcancer_synth10k"        # read-only OMOP CDM schema
cohortDatabaseSchema <- "lungcancer_synth10k_results"     # writable schema for the cohort table
cohortTable          <- "custom_rnvf"
tempEmulationSchema  <- NULL                            # set for Oracle/BigQuery/Spark, else NULL
databaseId           <- "lungcancer_synth10k"       # short label for this CDM, e.g. "CDM_2025"
outputFolder         <- file.path(homedir, "results", "CustomRNVF", databaseId)
minCellCount         <- 5

# Cohort ids (must exist in inst/settings/CohortsToCreate.csv):
targetIds  <- c(251)
outcomeIds <- c(252)

# ---- Run ----------------------------------------------------------------------
CustomRNVF::execute(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTable = cohortTable,
  outputFolder = outputFolder,
  databaseId = databaseId,
  targetIds = targetIds,
  outcomeIds = outcomeIds,
  minCellCount = minCellCount,
  tempEmulationSchema = tempEmulationSchema,
  createCohorts = TRUE,
  runCohortDiagnostics = TRUE,
  runCharacterization = TRUE,
  runIncidenceRate = TRUE
)
