library(TreatmentPatterns)

# Work around v1.0.0 bug: is_installed() compares a version against a numeric
# default, which modern R rejects. Replace the check with requireNamespace().
assignInNamespace(
  "ensure_installed",
  function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop(sQuote(pkg), " must be installed for launchResultsExplorer().", call. = FALSE)
  },
  ns = "TreatmentPatterns"
)

# Make sure the Shiny app's packages are actually present
need <- setdiff(c("shiny","shinydashboard","reshape2","ggplot2","data.table","DT"),
                rownames(installed.packages()))
if (length(need)) install.packages(need)

TreatmentPatterns::launchResultsExplorer(
  outputFolder = "/Users/andreikiselev/Downloads/output 2"
) 




launchResultsExplorer(
   outputFolder = file.path("/Users/andreikiselev/Downloads/output 2")
 )
 