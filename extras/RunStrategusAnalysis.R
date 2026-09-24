library(Strategus)
library(DatabaseConnector)


jsonFilePath <- "./extras/StrategusAnalysisSpecification.json"
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


workDatabaseSchema <- "anastasiiaderzhanskaia"
cdmDatabaseSchema <- "cdm_lungcancer_syth10k_251104"
workFolder <- "work"
resultsFolder <- "results"
executionSettings <- Strategus::createCdmExecutionSettings(
  workDatabaseSchema = workDatabaseSchema, 
  cdmDatabaseSchema = cdmDatabaseSchema,      
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "custom_rnvf_cohorts"),
  workFolder = workFolder,   
  resultsFolder = resultsFolder,    
  minCellCount = 5                            
)

Strategus::execute(
  analysisSpecifications = analysisSpecifications,
  executionSettings = executionSettings,
  connectionDetails = connectionDetails
)



