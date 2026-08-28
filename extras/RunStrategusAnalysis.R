library(Strategus)
library(DatabaseConnector)


jsonFilePath <- "./StrategusAnalysisSpecification.json"
analysisSpecifications <- ParallelLogger::loadSettingsFromJson(jsonFilePath)

envkeyring <- keyring::backend_env$new()
envkeyring$set_with_value("DB_PASS", username = Sys.getenv("DB_USERNAME"), password = getPass::getPass())
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  connectionString = "jdbc:postgresql://postgresql:5432/ohdsi",
  user = Sys.getenv("DB_USERNAME"),
  server = "postgresql/ohdsi",
  password = envkeyring$get("DB_PASS", username = Sys.getenv("DB_USERNAME")),
  pathToDriver = "~/Test/pathToDriverPostgres")
options(connectionObserver = NULL)
conn <- DatabaseConnector::connect(connectionDetails)


workDatabaseSchema <- ""
cdmDatabaseSchema <- ""
workFolder <- ""
resultsFolder <- ""
executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = connectionDetails,
  workDatabaseSchema = workDatabaseSchema, 
  cdmDatabaseSchema = cdmDatabaseSchema,      
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "custom_rnvf_cohorts"),
  workFolder =  workFolder,   
  resultsFolder = resultsFolder,    
  minCellCount = 5                            
)

Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  connectionDetails = connectionDetails
)

