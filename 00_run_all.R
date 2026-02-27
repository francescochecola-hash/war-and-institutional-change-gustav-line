################################################################################
# Francesco Checola
# War and Institutional Change: The Case of Gustav Line
# Replication Package
# 00_run_all.R
################################################################################

rm(list = ls())
gc()

# ----------------------------
# 1. Load required packages
# ----------------------------
required_packages <- unique(c(
  "here","fs","tidyverse","sf","rdrobust","rddensity","fixest",
  "haven","tools","fuzzyjoin","stringdist","broom","sandwich","lmtest","ggnewscale"
))

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

invisible(lapply(required_packages, library, character.only = TRUE))

cat("Project root detected by here():\n")
print(here())

# ----------------------------
# 2. Save console output to log
# ----------------------------
fs::dir_create(here("results"), recurse = TRUE)
log_file <- here("results", "run_all_log.txt")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

cat("\nLog file:", log_file, "\n\n")

# ----------------------------
# 3. Create folders
# ----------------------------
fs::dir_create(here("data", "processed"), recurse = TRUE)
fs::dir_create(here("data", "processed", "import"), recurse = TRUE)
fs::dir_create(here("data", "processed", "select"), recurse = TRUE)
fs::dir_create(here("data", "processed", "merge"), recurse = TRUE)

fs::dir_create(here("results", "tables"), recurse = TRUE)
fs::dir_create(here("results", "figures"), recurse = TRUE)

# ----------------------------
# 4. Run scripts in order
# ----------------------------
scripts <- list.files(
  here("scripts"),
  pattern = "\\.R$",
  full.names = TRUE
)
scripts <- sort(scripts)

for (script in scripts) {
  cat("Running:", script, "\n")
  tryCatch(
    source(script, local = .GlobalEnv),
    error = function(e) {
      message("\nERROR in: ", script)
      stop(e)
    }
  )
}

cat("\nReplication completed successfully.\n")