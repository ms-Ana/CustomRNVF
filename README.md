# CustomRNVF

An OHDSI study package that runs, against an OMOP CDM:

1. **Cohort generation** — `CohortGenerator`
2. **Cohort diagnostics** — `CohortDiagnostics`
3. **Characterization** — `Characterization` (baseline + on-cohort covariates, time-to-event,
   dechallenge/rechallenge), with on-treatment and intent-to-treat time-at-risk windows
4. **Incidence rate calculation** — `CohortIncidence`, with on-treatment and intent-to-treat
   time-at-risk windows

The package contains **only orchestration** (`R/execute.R`). All analytics and SQL are produced by
the underlying OHDSI packages — there is no custom analytic code and no hand-written SQL. Package
versions are pinned in `renv.lock` (OHDSI HADES 2025Q3).

## Requirements

- R 4.4.1, Java (for `DatabaseConnector` / `rJava`), and the appropriate JDBC driver
  (`DatabaseConnector::downloadJdbcDrivers()`).
- Network access to your ATLAS WebAPI (for cohort definitions) and your OMOP CDM.

## Setup

```r
# 1. Install the exact pinned dependencies
renv::restore()
```

## Workflow

### Step 1 — Fetch cohort definitions from ATLAS (one-time per study)

1. Edit `inst/settings/CohortsToCreate.csv` — one row per cohort:
   `atlasId` (the cohort id in ATLAS), `cohortId` (the id used in this study), `cohortName`.
2. Set `ATLAS_BASE_URL` (and auth, if needed) and run:

```r
source("extras/CreateCohortDefinitionSet.R")
```

This populates `inst/cohorts/*.json` and `inst/sql/sql_server/*.sql` via `ROhdsiWebApi`/CirceR.

### Step 2 — Run the study against your CDM

Edit the connection details, schema names, and `targetIds` / `outcomeIds` in
`extras/CodeToRun.R`, then:

```r
source("extras/CodeToRun.R")
```

Results are written under `results/<databaseId>/`:

- `cohort_counts.csv`
- `cohortDiagnostics/` (CohortDiagnostics SQLite/CSV export)
- `characterization/` (characterization CSVs)
- `incidence/incidence_summary.csv`
- `log.txt`, `errorReport.txt`

## Required inputs

| Input | Where |
|-------|-------|
| ATLAS WebAPI base URL (+ auth) | `extras/CreateCohortDefinitionSet.R` / `ATLAS_BASE_URL` |
| Cohorts (target/outcome ids) | `inst/settings/CohortsToCreate.csv` |
| CDM connection + schemas | `extras/CodeToRun.R` (env vars) |
| Target / outcome ids | `extras/CodeToRun.R` |
