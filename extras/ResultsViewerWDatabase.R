library(dplyr)

library(ShinyAppBuilder)
library(OhdsiShinyModules)



envkeyring <- keyring::backend_env$new()
envkeyring$set_with_value("DB_PASS", username = Sys.getenv("DB_USERNAME"), password = getPass::getPass())
# Specify the connection to the results database
resultsConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  connectionString = "jdbc:postgresql://postgresql:5432/ohdsi",
  user = Sys.getenv("DB_USERNAME"),
  server = "postgresql/ohdsi",
  password = envkeyring$get("DB_PASS", username = Sys.getenv("DB_USERNAME")),
  pathToDriver = "~/Test/pathToDriverPostgres"
)


resultsDataModelSettings <- Strategus::createResultsDataModelSettings(
  resultsDatabaseSchema = "anastasiiaderzhanskaia",
  resultsFolder = "results",
)

Strategus::createResultDataModel(
  analysisSpecifications = analysisSpecifications,
  resultsDataModelSettings = resultsDataModelSettings,
  resultsConnectionDetails = resultsConnectionDetails
)

Strategus::uploadResults(
  analysisSpecifications = analysisSpecifications,
  resultsDataModelSettings = resultsDataModelSettings,
  resultsConnectionDetails = resultsConnectionDetails
)

library(ShinyAppBuilder)
library(OhdsiShinyModules)

# ADD OR REMOVE MODULES TAILORED TO YOUR STUDY
shinyConfig <- initializeModuleConfig() |>
  addModuleConfig(
    createDefaultAboutConfig()
  ) |>
  addModuleConfig(
    createDefaultDatasourcesConfig()
  ) |>
  addModuleConfig(
    createDefaultCohortGeneratorConfig()
  ) |>
  addModuleConfig(
    createDefaultCohortDiagnosticsConfig()
  ) |>
  addModuleConfig(
    createDefaultCharacterizationConfig()
  ) |>
  addModuleConfig(
    createDefaultPredictionConfig()
  ) |>
  addModuleConfig(
    createDefaultEstimationConfig()
  )

envkeyring <- keyring::backend_env$new()
envkeyring$set_with_value("DB_PASS", username = Sys.getenv("DB_USERNAME"), password = getPass::getPass())
# Specify the connection to the results database
resultsConnectionDetails1 <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  connectionString = "jdbc:postgresql://postgresql:5432/ohdsi",
  user = Sys.getenv("DB_USERNAME"),
  server = "postgresql/ohdsi",
  password = envkeyring$get("DB_PASS", username = Sys.getenv("DB_USERNAME")),
  pathToDriver = "~/Test/pathToDriverPostgres"
)
options(connectionObserver = NULL)
conn <- DatabaseConnector::connect(resultsConnectionDetails1)

# now create the shiny app based on the config file and view the results
# based on the connection
ShinyAppBuilder::createShinyApp(
  config = shinyConfig,
  connectionDetails = resultsConnectionDetails1,
  resultDatabaseSettings = createDefaultResultDatabaseSettings(schema = "anastasiiaderzhanskaia"),
  title = "OptimeDemo",
  studyDescription = "OptimeDemo"
)

downloadJdbcDrivers(
  dbms,
  pathToDriver = Sys.getenv("DATABASECONNECTOR_JAR_FOLDER"),
  method = "auto",
)



